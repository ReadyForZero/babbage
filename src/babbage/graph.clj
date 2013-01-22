(ns babbage.graph
  (:require [babbage.util :as u]
            [clojure.set :as set]
            [babbage.derefmap :as derefmap]
            [macroparser.bindings :as b]
            [macroparser.functions :as f]
            [macroparser.parsers :as p]
            [the.parsatron :as parsatron]))

(defn parse-defgraphfn []
  (p/parseq->map
   (p/named :name (p/symbol))
   (p/named :provides (p/maybe (p/keyword)))
   (p/named :docstring (p/maybe (p/string)))
   (p/named :attr-map (p/maybe (p/map)))
   (f/arities)))

(defmacro defgraphfn
  "Define a function that documents its required input and produced
   output. Functions defined with defproducer will have a metadata map
   on the *function* (*not* the var!) containing a :producer key
   corresponding to the function name, and a :requires key
   corresponding to the names of its arguments.

   N.B. Neither multiple arities nor argument destructuring is currently supported."
  {:arglists '(name provides? docstring? attr-map? params & body)}
  [& args]
  (let [parsed (parsatron/run (parse-defgraphfn) args)
        arities (:arities parsed)
        name (:name parsed)]
    (assert (== 1 (count arities))
            (str "Multiple arities not supported for graph functions: " (:name parsed)))
    (assert (and (every? symbol? (:bindings (:params (first arities))))
                 (nil? (:rest (:params (first arities))))
                 (nil? (:as (:params (first arities)))))
            (str "Destructuring not supported for graph functions: " (:name parsed)))
    (let [provides (or (:provides parsed) (keyword name))
          requires (mapv keyword (b/bound-symbols (:params (first arities))))
          attr-map (merge (:attr-map parsed)
                          {:arglists (list 'quote (list (vec (b/bound-symbols (:params (first arities))))))})
          fn-attr-map (merge attr-map {:provides provides :requires requires})]
      `(def ~(with-meta name attr-map)
         (with-meta ~(f/unparse-function (assoc parsed :type 'fn)) ~fn-attr-map)))))

(def defaults {:leaf-strat apply
               :layer-strat pmap
               :lazy? false})

(defn- mapentry->node [[k v]]
  {:requires nil :provides (keyword (name k)) :value (constantly v)})

(defn- mapentry->nodem [[k v :as entry]]
  (assoc (mapentry->node entry) :value `(constantly ~v)))

(defmacro wrap-when [test wrap-with expr]
  (let [wrap-with (if (seq? wrap-with) wrap-with (list wrap-with))]
    (cond
     (= true test) `(~@wrap-with ~expr)
     (or (nil? test) (= false test)) expr
     :else `(if ~test (~@wrap-with ~expr) ~expr))))

(defn- run-layer-elt [result leaf-strat lazy? elt]
  [(:provides elt)
   (wrap-when lazy? delay
              (leaf-strat (:value elt) (map (wrap-when lazy? (comp deref) result)
                                            (:requires elt))))])

(defn- run-layer [layer-strat leaf-strat lazy? result layer]
  (merge result
         (->> layer
              (layer-strat (partial run-layer-elt result leaf-strat lazy?))
              (into {}))))

(defn- run-layers [layer-strat leaf-strat lazy? layers]
  (reduce (partial run-layer layer-strat leaf-strat lazy?) {} layers))

(defn node-meta [node]
  (let [{:keys [provides requires] :as m} (meta node)]
    (when (and (not (keyword? provides))
               (or (not (seq? requires)) (empty? requires)))
      (throw (Exception. (str "Node has neither provides nor requires metadata: " node (meta node)))))
    m))

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
  [initial-values options & nodes]
  (let [options (merge defaults options)
        initial-value-nodes (map mapentry->node initial-values)
        provider-nodes (map (fn [provider] (merge (node-meta provider) {:value provider}))
                            (remove nil? nodes))
        {:keys [lazy? layer-strat leaf-strat]} options
        r (run-layers layer-strat leaf-strat lazy?
                      (u/layers (concat initial-value-nodes provider-nodes)))]
    (if lazy? (derefmap/->DerefMap r) r)))

(defn- key->sym [k] (symbol (name k)))

(defn- layer-elt-let-expr [elt]
  [(:value elt) (if (seq? (:requires elt)) (mapv key->sym (:requires elt)) '())])

(defn- layer->let-row [layer-strat leaf-strat lazy? layer]
  (let [bounds (mapv (comp key->sym :provides) layer)
        bindings (mapv layer-elt-let-expr layer)]
    [bounds `(~layer-strat (fn [f# args#]
                             (wrap-when ~lazy? delay
                                        (~leaf-strat f# (wrap-when ~lazy? (map deref) args#))))
                           ~(mapv first bindings)
                           ~(mapv second bindings))]))

(defn run-inline [initial-values options nodes]
  (let [{:keys [lazy? layer-strat leaf-strat]} options
        initial-value-nodes (map mapentry->nodem initial-values)
        provider-nodes (map (fn [node] (merge (meta (deref (resolve node))) {:value node})) nodes)
        nodes (concat initial-value-nodes provider-nodes)
        layers (u/layers nodes)]
    `(let ~(vec (mapcat (partial layer->let-row layer-strat leaf-strat lazy?)
                        layers))
       (wrap-when true derefmap/->DerefMap
                  (hash-map ~@(interleave (map :provides nodes)
                                          (map (comp key->sym :provides) nodes)))))))

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
  [initial-values options & nodes]
  (if (and (good-map? initial-values)
           (good-map? options)
           (every? symbol? nodes))
    (let [initial-values (resolve-safe initial-values)
          options (merge defaults (resolve-safe options))]
      (run-inline initial-values options nodes))
    `(run-graph-strategy ~initial-values ~options ~@nodes)))

(defn run-graph
  "Run the graph fns in \"nodes\", supplying them with initial values
   in \"initial-values\", a map. Uses default option values."
  [initial-values & nodes]
  (apply run-graph-strategy initial-values defaults nodes))

(defmacro run-graph*
  "Like run-graph-strategy* but uses default values as in run-graph."
  [initial-values & nodes]
  `(run-graph-strategy ~initial-values defaults ~@nodes))

(defn compile-graph-strat
  "Create a function from the graph functions in nodes. The resulting
   function accepts as its first argument map that must contain keys
   corresponding to all the parameters necessary to run the graph to
   completion; its second argument is an option map as in run-graph-strategy."
  [& nodes]
  (let [[layers still-required] (u/layers-and-required (map (fn [n] (merge (meta n) {:value n}))
                                                            nodes))]
    (fn [initial-values options]
      (assert (set/subset? still-required (set (keys initial-values))))
      (let [initial-value-nodes (map mapentry->node initial-values)
            options (merge defaults options)
            {:keys [layer-strat leaf-strat lazy?]} options]
        (run-layers layer-strat leaf-strat lazy? (concat [initial-value-nodes] layers))))))

(defn compile-graph
  "Like compile-graph-strat, except the returned function does not accept an options map."
  [& nodes]
  (let [f (apply compile-graph-strat nodes)]
    (fn [initial-values]
      (f initial-values defaults))))
