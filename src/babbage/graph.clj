(ns babbage.graph
  (:require [babbage.util :as u]
            [clojure.set :as set]
            [babbage.derefcolls :as derefcolls]
            [macroparser.bindings :as b]
            [macroparser.functions :as f]
            [macroparser.parsers :as p]
            [the.parsatron :as parsatron]))

(set! *warn-on-reflection* true)

(defn- parse-defgraphfn []
  (p/parseq->map
   (p/named :name (p/symbol))
   (p/named :provides (p/maybe (p/keyword)))
   (p/named :docstring (p/maybe (p/string)))
   (p/named :attr-map (p/maybe (p/flattened-map)))
   (f/arities)))

(defn- ok-destructuring?
  [b]
  (or (symbol? b)
      (symbol? (:as b))))

(defn- get-requires [bindings]
  (mapv (comp keyword (fn [b] (if (symbol? b) b (:as b)))) bindings))

(defmacro defgraphfn
  "Define a function that documents its required input and produced
   output. Functions defined with defproducer will have a metadata map
   on the *function* (*not* the var!) containing a :producer key
   corresponding to the function name, and a :requires key
   corresponding to the names of its arguments.

   The syntax of defgraphfn is almost identical to that of defn. The
   differences are as follows:

   - an optional keyword can follow the function name. If provided,
     that keyword will be the key used in the result map for this
     function, and other graph functions can access this function's
     output using the name of that keyword. This allows multiple
     functions to act as the same node in a graph.

   - multiple arities are not allowed, though both
       (defgraphfn foo [args] body)
     and
       (defgraphfn foo ([args] body))
     are allowed.

   - in the argument vector, destructuring of the top-level arguments
     is only allowed with :as parameters. This is allowed:
       (defgraphfn foo [[a {f :g} :as c] {:keys [x y] :as z}] body)
     because each of the two arguments to foo can be identified with a
     single name (c and z respectively), and those names can be
     considered its dependencies. (The second element of c can be
     destructured without an :as because it is not a direct dependency
     of foo.) This is not allowed:
       (defgraphfn bar [[a {f :g}] {:keys [x y]}] body)
     because there is no way to tell what the dependencies of bar are."
  {:arglists '(name provides? docstring? attr-map? params & body)}
  [& args]
  (let [parsed (parsatron/run (parse-defgraphfn) args)
        arities (:arities parsed)
        name (:name parsed)]
    (assert (== 1 (count arities))
            (str "Multiple arities not supported for graph functions: " (:name parsed)))
    (assert (nil? (:rest (:params (first arities))))
            (str "Rest params not allowed in top-level bindings in graph fn:" (:name parsed)))
    (assert (and (every? ok-destructuring? (:bindings (:params (first arities)))))
            (str "Destructuring top-level arguments in graph fn " (:name parsed) " requires :as"))
    (let [provides (or (:provides parsed) (keyword name))
          requires (get-requires (:bindings (:params (first arities))))
          attr-map (merge (:attr-map parsed)
                          {:arglists (list 'quote (list (b/unparse-bindings (:params (first arities)))))})
          fn-attr-map (merge attr-map {:provides provides
                                       :requires requires})]
      `(let [f# ~(f/unparse-function (assoc parsed :type 'fn))]
         (def ~(with-meta name attr-map)
           (with-meta f# ~fn-attr-map))))))

(def defaults {:leaf-strat apply
               :layer-strat pmap
               :lazy? false})

(defn- mapentry->node [[k v]]
  {:requires nil :provides (keyword (name k)) :value v})

(defmacro wrap-when [test wrap-with expr]
  (let [wrap-with (if (seq? wrap-with) wrap-with (list wrap-with))]
    (cond
     (true? test) `(~@wrap-with ~expr)
     (or (nil? test) (false? test)) expr
     :else `(if ~test (~@wrap-with ~expr) ~expr))))

(defn- run-layer-elt [result leaf-strat lazy? elt]
  (if (not (seq (:requires elt)))
    [(:provides elt) (wrap-when lazy? delay (:value elt))]
    (let [r (wrap-when lazy? delay (leaf-strat (:value elt)
                                               (map (wrap-when lazy? (comp deref) result)
                                                    (:requires elt))))]
      (cond
       (keyword? (:provides elt)) [(:provides elt) r]
       lazy? (zipmap (:provides elt) (for [i (range (count (:provides elt)))]
                                       (delay (nth (deref r) i))))
       :else (zipmap (:provides elt) r)))))

(defn- run-layer [layer-strat leaf-strat lazy? result layer]
  (if (= 1 (count layer))
    (let [m (run-layer-elt result leaf-strat lazy? (first layer))]
      (merge result (into {} (list m))))
    (merge result
           (->> layer
                (layer-strat (partial run-layer-elt result leaf-strat lazy?))
                (into {})))))

(defn- run-layers [layer-strat leaf-strat lazy? layers]
  (reduce (partial run-layer layer-strat leaf-strat lazy?) {} layers))

(defn node-meta [node]
  (let [{:keys [provides requires] :as m} (meta node)]
    (when-not (or (keyword? provides) (and (sequential? provides) (every? keyword? provides)))
      (throw (Exception. (str "Node lacks provides metadata: " node (meta node)))))
    (when (not (every? keyword? requires))
      (throw (Exception. (str "Node has invalid requires metadata: " node (meta node)))))
    (assoc m :value node)))

(defn unique-provides! [provides]
  (let [unique (set provides)]
    (assert (= (count unique) (count provides))
            (let [f (vec (filter (fn [[k v]] (not= v 1)) (frequencies provides)))]
              (str "Error: attempting to provide the same value(s) multiple times: " f))))
  provides)

(defn run-graph-strategy
  "Run the graph fns in \"nodes\", supplying them with initial values
   in \"initial-values\", a map, using options supplied in a map.

   The options currently accepted are :layer-strat, :leaf-strat,
   and :lazy?.

   leaf-strat is called to evaluate individual graph fns; its first
   argument is the function itself and the second is a seq of
   arguments to the function, or nil if it is a nullary function. It
   defaults to apply.

   layer-strat is called on groups of graph-fns all of which depend on
   values in previously computed groups and none of which depend on
   values any other member of the group provides. (They are the
   breadths produced by a breadth-first search of the dependency
   graph.) Its first argument is a function that takes a single graph
   fn and its second is a seq of graph fns. It defaults to pmap.

   if lazy? is true, result values will be lazily computed: fetching a
   value from the map will cause it, and its (recursive) dependencies,
   to be computed and cached."
  [options initial-values & nodes]
  (let [options (merge defaults options)
        initial-value-nodes (map mapentry->node initial-values)
        provider-nodes (map node-meta (remove nil? nodes))
        nodes (concat initial-value-nodes provider-nodes)]
    (unique-provides! (flatten (map :provides nodes)))
    (let [{:keys [lazy? layer-strat leaf-strat]} options
          r (run-layers layer-strat leaf-strat lazy?
                        (u/layers nodes))]
      (if lazy? (derefcolls/->DerefMap r) r))))

(defn- key->sym [k]
  (if (keyword? k)
    (symbol (name k))
    (mapv key->sym k)))

(defn- layer-elt-let-expr [elt]
  [(:value elt) (if (not-empty (:requires elt)) (mapv key->sym (:requires elt)) '())])

;; break me up, please.
(defn- layer->let-row [layer-strat leaf-strat lazy? layer]
  (let [bounds (mapv (comp key->sym :provides) layer)
        bindings (mapv layer-elt-let-expr layer)]
    (if lazy?
      (let [simple (mapv (fn [s] (if (sequential? s) (gensym) s)) bounds)]
        (if (= 1 (count bounds))
          (let [b (first bounds) s (first simple)
                single-expr `(delay (~leaf-strat ~(ffirst bindings) (map deref ~(second (first bindings)))))]
            (if (= b s)
              [b single-expr]
              (concat [s single-expr]
                      (apply concat (for [i (range (count b))]
                                      `[~(nth b i) (delay (nth (deref ~s) ~i))])))))
          (concat [simple `(~layer-strat (fn [f# args#] (delay (~leaf-strat f# (map deref args#))))
                                         ~(mapv first bindings) ~(mapv second bindings))]
                  (apply concat (for [[b s] (map vector bounds simple)]
                                  (when-not (= b s)
                                    (apply concat (for [[b i] (map vector b (range))]
                                                    `[~b (delay (nth (deref ~s) ~i))]))))))))
      (if (= 1 (count bounds))
        [(first bounds) `(~leaf-strat ~(ffirst bindings) ~(second (first bindings)))]
        [bounds `(~layer-strat ~leaf-strat ~(mapv first bindings) ~(mapv second bindings))]))))

(defn run-inline [initial-values options nodes]
  (let [{:keys [lazy? layer-strat leaf-strat]} options
        initial-value-nodes (map mapentry->node initial-values)
        provider-nodes (map (fn [node] (merge (meta (deref (resolve node))) {:value node})) nodes)
        nodes (concat initial-value-nodes provider-nodes)
        layers (rest (u/layers nodes))]
    (unique-provides! (flatten (map :provides nodes)))
    `(let ~(vec (concat
                 (mapcat (fn [[k v]] `[~(symbol (name k)) (wrap-when ~lazy? delay ~v)]) initial-values)
                 (mapcat (partial layer->let-row layer-strat leaf-strat lazy?)
                         layers)))
       (wrap-when ~lazy? derefcolls/->DerefMap
                  (hash-map ~@(interleave (flatten (map :provides nodes))
                                          (flatten (map (comp key->sym :provides) nodes))))))))

(defn good-map? [m]
  (or (and (map? m) (every? keyword? (keys m)))
      (and (symbol? m) (good-map? (deref (resolve m))))))

(defn resolve-safe [o]
  (if (symbol? o)
    (deref (resolve o))
    o))

(defmacro run-graph-strategy*
  "Accepts the same arguments as run-graph-strategy, but attempts to
   do the dependency analysis at compile time: if initial-values and
   options are maps (or symbols resolving to a map) whose keys are all
   keywords, and every node is a symbol, expands into a let statement.
   Otherwise, falls back to run-graph-strategy.

   Warning: resolve's semantics means that this does not do what you'd expect:

   (def i-values {...}) ;; \"outer\"
   (let [i-values (assoc i-values :key value)] ;; \"inner\"
       (run-graph-strategy* i-values :default :default ...))

   run-graph-strategy* in this case will pick up the *outer* value of i-values."
  [options initial-values & nodes]
  (if (and (good-map? initial-values)
           (good-map? options)
           (every? symbol? nodes))
    (let [initial-values (resolve-safe initial-values)
          options (merge defaults (resolve-safe options))]
      (run-inline initial-values options nodes))
    `(run-graph-strategy ~options ~initial-values ~@nodes)))

(defn run-graph
  "Run the graph fns in \"nodes\", supplying them with initial values
   in \"initial-values\", a map. Uses default option values."
  [initial-values & nodes]
  (apply run-graph-strategy defaults initial-values nodes))

(defmacro run-graph*
  "Like run-graph-strategy* but uses default values as in run-graph."
  [initial-values & nodes]
  `(run-graph-strategy* defaults ~initial-values ~@nodes))

(defn compile-graph-strategy
  "Create a function from the graph functions in nodes, using options options.
   The resulting function accepts the arguments necessary to compute
   all the results of all the nodes, and returns the values computed
   by each node in a vector."
  [options & nodes]
  (let [syms (repeatedly (count nodes) gensym)
        mnodes (map (fn [node sym] (assoc (node-meta node) :value sym)) nodes syms)
        {:keys [layer-strat leaf-strat lazy?]} (merge defaults options)
        [layers required] (u/layers-and-required mnodes)
        required (sort required)
        rsyms (map key->sym required)
        new-provides (sort (unique-provides! (flatten (map :provides mnodes))))
        f (apply (eval
                  `(fn [~@syms]
                     (fn [~@rsyms]
                       (wrap-when ~lazy?
                                  (let [~@(mapcat (fn [s] [s `(delay ~s)]) rsyms)])
                                  (let [~@(mapcat #(layer->let-row layer-strat leaf-strat lazy? %)
                                                  layers)]
                                    (if ~lazy?
                                      (derefcolls/->DerefSeq ~(mapv key->sym new-provides))
                                      ~(mapv key->sym new-provides)))))))
                 nodes)]
    (with-meta f {:requires required :provides new-provides})))

(defn compile-graph
  "Like compile-graph-strategy, using default options."
  [& nodes]
  (apply compile-graph-strategy defaults nodes))
