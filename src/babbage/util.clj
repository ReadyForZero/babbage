(ns babbage.util
  (:require [babbage.monoid :as m]
            [clojure.set :as set]
            [macroparser.functions :as f]
            [the.parsatron :as parsatron]))

(defmacro if-ns-avail [req then & [else]]
  (if (try (eval req) true
           (catch Throwable _ false))
    then
    else))

(defn stringify
  "Returns a string, writing out 'nil' if nil."
  [x]
  (if (nil? x)
    "nil"
    (str x)))

(defmacro safely-run [{:keys [where what]} & body]
  `(try
     ~@body
     (catch Exception e#
       (throw (Exception. (str ~where " failed on: " ~what))))))

(defn maybe-throw-error [a-str a-val]
  (when (nil? a-val)
    (throw
     (Exception.
      (format "%s is nil, expecting non-nil." a-str)))))

(defmacro -!>
  "Equivalent to ->, except an intermediate nil value triggers an exception."
  ([a]
     (let [a-str (stringify a)]
       `(let [a-val# ~a]
          (maybe-throw-error ~a-str a-val#)
          a-val#)))
  ([a b]
     (let [a-str (stringify a)
           b-str (stringify b)
           exp-str (format "(-!> %s %s)" a-str b-str)]
       `(let [a-val# ~a]
          (maybe-throw-error ~a-str a-val#)
          (let [result# (-> a-val# ~b)]
            (maybe-throw-error ~exp-str result#)
            result#)
          )))
  ([a b & cs] `(-!> (-!> ~a ~b) ~@cs)))

(defmacro defnmeta
  "Exactly like defn, except that any metadata created in the optional
   attr-map argument to defn will be placed on both the var and the
   value. (Metadata created using the ^{...} reader syntax will only
   be on the var.)"
   {:arglists (:arglists (meta #'defn))}
  [& args]
  (let [parsed (parsatron/run (f/parse-defn-like) args)
        attr-map (:attr-map parsed)
        name (:name parsed)]
    `(def ~(with-meta name attr-map)
       (with-meta ~(f/unparse-fn-like (assoc parsed :type 'fn)) ~attr-map))))

(defmacro fnmeta [meta & sigs]
  `(with-meta (fn ~@sigs) ~meta))

(defn ->set [k]
  (if (sequential? k)
    (set k)
    #{k}))

(defn dfs
  "Returns nil if a cycle was encountered, otherwise pair of visited
  nodes, map of nodes to ancestors."
  [f seen ancestors depmap]
  (let [f-name (->set (:provides f))
        seen (set/union seen f-name)
        out (keep depmap (:requires f))]
    (loop [out out seen seen ancestors ancestors]
      (if-let [g (first out)]
        (let [g-name (->set (:provides g))
              f-ancestors (reduce set/union #{} (map ancestors f-name))]
          (if (some seen g-name)
            (when-not (some #(contains? f-ancestors %) g-name) ;; back edge
              (recur (rest out) (set/union seen g-name) ancestors)) ;; cross edge
            ;; forward edge
            (when-let [[new-seen new-anc] (dfs g seen
                                               (reduce (fn [anc g-name*]
                                                         (assoc anc g-name* (set/union f-name f-ancestors)))
                                                       ancestors
                                                       g-name)
                                               depmap)]
              (recur (rest out) new-seen new-anc))))
        [seen ancestors]))))

(defn circular? [has-requires depmap]
  (let [depmap (reduce (fn [acc [k v]]
                              (if (sequential? k)
                                (reduce #(assoc %1 %2 v) acc k)
                                (assoc acc k v))) {} depmap)]
    (if-let [f (first has-requires)]
      (let [dfs-result (dfs f #{} {} depmap)]
        (if (nil? dfs-result)
          true
          (recur (remove (fn [{:keys [provides]}]
                           (if (sequential? provides)
                             (every? #(contains? (first dfs-result) %) provides)
                             (contains? (first dfs-result) provides))) has-requires) depmap)))
      false)))

(defn still-required [independent dependent]
  (let [required (set (reduce m/<> (map :requires dependent)))
        provided (reduce set/union (map (comp ->set :provides) (concat dependent independent)))]
    (set/difference required provided)))

(defn group-deps [already-grouped dependent]
  (loop [already-grouped already-grouped dependent dependent groups []]
    (if (seq dependent)
      (let [{this-group true remainder false} (group-by #(every? already-grouped (:requires %)) dependent)]
        (recur (reduce set/union already-grouped (map (comp ->set :provides) this-group))
               remainder
               (conj groups this-group)))
      groups)))

;; fixing up the functions. The "map" here could be pmap---the
;; functions in the group are independent of each other.
(defn transform-group [group]
  (fn [result]
    (let [neue (->> (map (fn [g] [(:provides g) ((:value g) result)]) group)
                    (into {}))]
      (merge result neue))))

(defn dependent? [o]
  (-> o meta :requires set?))

(defn layers
  "Given a DAG consisting of seq of nodes (represented as maps with a
   label under the :provides key and outgoing edges under
   the :requires key), return a seq of seqs of nodes s.t. the first
   element contains the nodes with no incoming edges, the second
   contains the nodes with incoming edges from the first, etc.

   Throws exceptions if a node :requires a nonexistent node or in the
   case of circular dependencies."
  [nodes]
  (let [{dependent true independent false} (group-by (comp boolean seq :requires) nodes)
        depmap (zipmap (map :provides dependent) dependent)]
    (cond
     ;; would also be caught by the danglers? check below.
     (empty? independent) (throw (Exception. "Cannot have only dependent values."))
     (empty? dependent) [nodes]
     (circular? dependent depmap) (throw (Exception. "Cannot have circular dependencies."))
     (not-empty (still-required independent dependent))
     (throw (Exception. (str "Cannot depend on unproduced values: " (still-required independent dependent))))
     :else (concat [independent] (group-deps (set (mapcat (comp ->set :provides) independent)) dependent)))))

(defn layers-and-required
  [nodes]
  (let [{dependent true independent false} (group-by (comp boolean seq :requires) nodes)
        depmap (zipmap (map :provides dependent) dependent)]
    (cond
     (empty? dependent) [[nodes] #{}]
     (circular? dependent depmap) (throw (Exception. "Cannot have circular dependencies."))
     :else (let [required (still-required independent dependent)]
             [(concat (when independent [independent])
                      (group-deps (set/union required (set (map :provides independent))) dependent))
              required]))))

(defn ->prov [m]
  (if (:many m)
    (assoc m :provides (:names m))
    (assoc m :provides [(:name m)])))

(deftype PostProcessing [ms processors]
  m/Monoid
  (<> [self other]
    (if (nil? other)
      self
      (let [^PostProcessing other other]
        (PostProcessing. (m/<> ms (.ms other)) processors))))
  (mempty? [self] (or (empty? ms) (m/mempty? ms)))
  (mempty [self] (PostProcessing. nil nil))
  (value [self] (let [m (m/value ms)]
                  (reduce (fn [acc f] (f acc)) m processors))))

(defn ->processor [processor-layer]
  (apply comp (map :processor processor-layer)))

(defn post-processor [ms processors]
  (PostProcessing. ms (map ->processor processors)))


(defn run-monoid [ent v i mon]
  (if (:whole-record mon)
    ((:monoid-fun mon) ent v i)
    ((:monoid-fun mon) v i)))

(defn- run-sfuncs [ent v i sfuncs]
  (let [res (transient {})]
    (doseq [sf sfuncs]
      (if (:many sf)
        (doseq [[k v] (run-monoid ent v i sf)]
          (assoc! res k v))
        (assoc! res (:name sf) (run-monoid ent v i sf))))
    (persistent! res)))

(defn prep [sfuncs]
  (let [[sfuncs & posts] (layers (map ->prov sfuncs))]
    (if (seq posts)
      (fn [ent v i] (let [m (run-sfuncs ent v i sfuncs)]
                   (post-processor m posts)))
      (fn [ent v i] (run-sfuncs ent v i sfuncs)))))
