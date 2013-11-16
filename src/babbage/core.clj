;; Owner: wolfson@readyforzero.com
;; calculating interesting stats
(ns babbage.core
  (:require [babbage.monoid :as m]
            [babbage.util :as util]
            [clojure.string :as str])
  (:use [clojure.algo.generic.functor :only [fmap]]
        [babbage.functors :only [tfmap]]
        [trammel.core :only [defconstrainedfn]])
  (:refer-clojure :exclude [complement split-with]))

(defn stats
  "Create a function for calculating groups of statistics.

   Arguments are an extractor function and at least one statistical
   function.

   The return value is a function of one argument. The result of
   calling the extractor function on this argument is then passed to
   the statistical functions.

   For instance, (stats :x sum count max) will return a function that
   expects a map with an :x key and tallies the sum, count, and max
   the :x values:

   > (m/value ((stats :x sum count max) {:x 1}))
   {:sum 1 :count 1 :max 1}

   It is not in general necessary to call the return value of this function directly."
  [extractor stat-func1 & stats-funcs]
  (let [f (util/prep (cons stat-func1 stats-funcs))]
    (fn [ent] (f ent (extractor ent)))))

(defn consolidate
  "Merge several stats functions into one."
  [sfunc & sfuncs]
  (let [sfuncs (cons sfunc sfuncs)]
    {:monoid-fun (util/prep sfuncs)
     :whole-record true
     :names (mapcat #(or (:names %)
                         [(:name %)]) sfuncs)
     :many true}))

(defn- update-monoid-fun [sfunc whole not-whole]
  (if (:whole-record sfunc)
    (update-in sfunc [:monoid-fun] whole)
    (update-in sfunc [:monoid-fun] not-whole)))

(defn lift-seq
  "Lift stats functions to operate over a seq of values:

  > (calculate (stats :x p/count (lift-seq p/mean (rename p/count :seq-count))) [{:x [1 2]} {:x [2 3 7]}])
  {:all {:count 2, :mean 3.0, :seq-count 5}}

  Note the use of rename to avoid one count clobbering the other."
  ([sfunc]
     (update-monoid-fun sfunc
                        (fn [mfun] #(reduce m/<> nil (map (partial mfun %1) %2)))
                        (fn [mfun] #(reduce m/<> nil (map mfun %)))))
  ([sf sf2 & sfs]
     (lift-seq (apply consolidate sf sf2 sfs))))

(defn nest
  "Move the result of sfunc into an inner map."
  [inner-name sfunc & sfuncs]
  {:name inner-name
   :whole-record true
   :monoid-fun (util/prep (cons sfunc sfuncs))})

(defn lift-seq-inner
  "Lift stats functions to operate over a sequence of values, with the
   result placed under field-name:

   > (calculate (stats :x p/count (lift-seq-inner :inlist p/mean)) [{:x [1 2]} {:x [2 3 7]}])
   {:all {:inlist {:mean 3.0}, :count 2}}"
  [inner-name sfunc & sfuncs]
  (nest inner-name (apply lift-seq sfunc sfuncs)))

(defn rename
  "Change the name under which the result for sf will be located."
  [sf new-name]
  (assoc sf :name new-name))

(defn post
  "Add an arbitrary post-processing function to a stats fn. E.g., to
   count the number of unique elements:

   (require '[babbage.provided.core :as p])
   ;; actually already in provided
   (def count-unique (post :count-unique count p/set))

   Whereupon:

   > (calculate (stats identity p/count-unique) [1 2 3 4 5 6 3 2 1 6 4 2 3 1 5 3 2 4])
   {:all {:count-unique 6}}

   If one were also interested in the identity of the unique values,
   this could be more efficiently specifying a dependency and
   post-processing function (see ratio in provided.core)."

  [name f {:keys [monoid-fun]}]
  {:name name
   :monoid-fun (fn [x] (when x (let [v (monoid-fun x)]
                                (m/delegate v f))))})

(defmacro statfn
  "Create a function for computing statistics suitable for passing as
  an argument to stats. \"monoidfn\" should be the underlying function
  that combines values. The computed value will be inserted in the
  result map with the same name as fn-name or, if it is provided, the
  value of the :name optional argument. The :requires optional
  argument should contain the names of the functions that monoidfn
  expects already to have run.

  If the function being defined is not parameterized, use defstatfn.
  This macro is only necessary for creating functions on the fly: see
  ratio and histogram in babbage.provided.core for examples.

  Functions that are more than just simple wrappers will probably not
  be able to use this macro (or defstatfn): see e.g.
  map-{with-key,with-value,of}."
  [fn-name monoidfn & {:keys [name]}]
  (let [kwname (or name (keyword fn-name))]
    `{:name ~kwname
      :monoid-fun ~monoidfn}))

(defmacro defstatfn
  "Define a function for computing statistics suitable for passing as
   an argument to stats. Same as statfn, except that def is used to
   create a top-level var holding the function, and there is an
   additional :doc optional argument for supplying a docstring."
   [fn-name monoidfn & {:keys [name doc]}]
  `(def ~fn-name (with-meta (statfn ~fn-name ~monoidfn :name ~name)
                   {:doc ~doc})))

(defn by
  ([extractor sfunc]
     (assoc (update-monoid-fun sfunc
                               (fn [mfun] #(when %2 (mfun %1 (extractor %1))))
                               (fn [mfun] #(when %2 (mfun (extractor %1)))))
       :whole-record true))
  ([extractor sfunc sfunc2 & sfuncs]
     (by extractor (apply consolidate sfunc sfunc2 sfuncs))))

(defn map-with-key
  "Add a map named field-name to the result map. The keys for the map
   are computed using key-extractor. The values are drawn from the
   extractor passed to the enclosing stats call, and the remaining
   arguments are, as in stats, at least one function for computing
   statistics.

   The function for computing the key receives the whole record the
   statistics are calculated for.

   Example: If records have the structure

      {:type [keyword], :balance [number]}

   Then

      (stats :balance mean (map-with-key :type :type->balance mean))

   Computes the mean of all balances, and a map keyed by type whose
   values contain the means of balances for that type:

     {:mean 40, :type->balance {:foo {:mean 20}, :bar {:mean 56}}}"
  [key-extractor field-name value-sfunc1 & value-sfuncs]
  (let [f (util/prep (cons value-sfunc1 value-sfuncs))]
    {:name field-name
     :whole-record true
     :monoid-fun (fn [ent v] (when v {(key-extractor ent)
                                     (f ent v)}))}))

(defn map-with-value
  "Add a map named field-name to the result map. The values for the
   map are computed using value-extractor. The keys are drawn from the
   extractor passed to the enclosing stats call, and the remaining
   arguments are, as in stats, at least one function for computing
   statistics.

   The function for computing the value receives the whole record the
   statistics are calculated for.

   Example: if records have the strucutre

      {:type [keyword], :balance [number]}

   Then

      (stats :type count-binned (map-with-value :balance :type->balance mean))

   Computes the number of records per type, and the mean of the
   balances per type:

     {:count-binned {:foo 20, :bar 13}, :type->balance {:foo {:mean 20}, :bar {:mean 56}}}"
  [value-extractor field-name value-sfunc1 & value-sfuncs]
  (let [f (util/prep (cons value-sfunc1 value-sfuncs))]
    {:name field-name
     :whole-record true
     :monoid-fun (fn [ent v] (when v {v (f ent (value-extractor ent))}))}))

(defconstrainedfn map-of
  "Add a map named field-name to the result map. With type :key, as
   map-with-key; with type :value, as map-with-value."
  [type extractor field-name sfunc1 & sfuncs]
  [(#{:key :value} type)]
  (case type
    :key (apply map-with-key extractor field-name sfunc1 sfuncs)
    :value (apply map-with-value extractor field-name sfunc1 sfuncs)))

(defn split-input
  "Group values together according to the result of split-fn.

   Similar to histogram (but allows more flexible grouping), and to
   computed-set (but shows up in the fields, rather than the sets).

   Ex.:

   (require '[babbage.provided.core :as p])
   (def fields (stats :x (split-input #(int (/ % 10)) :every-ten
                          p/first p/sum p/last)))
   (calculate fields [{:x 1} {:x 11} {:x 3} {:x 2} {:x 14}])
    -->  {:all {:every-ten {1 {:last 14, :sum 25, :first 11}, 0 {:last 2, :sum 6, :first 1}}}}"
  [split-fn field-name sfunc1 & sfuncs]
  (let [f (util/prep (cons sfunc1 sfuncs))]
    {:monoid-fun (fn [ent v] {(split-fn v) (f ent v)})
     :whole-record true
     :name field-name}))

(defn sets
  "Describe subsets for which to calculate statistics.

   The argument, if given, should be a map whose keys are subset names
   and whose values are predicates members of the subset should
   satisfy. An additional set, with the key :all and containing
   everything, will also be calculated.

   If no argument is given, only the :all set will be calculated."
  ([] (sets {:all (constantly true)}))
  ([pred-map]
      (let [pred-map (if (:all pred-map)
                       pred-map
                       (assoc pred-map :all (constantly true)))]
        (fn [fields]
          (let [finalizer (if (fn? fields) :_ identity)
                fields (if (fn? fields) {:_ fields} fields)]
            (fn [ent]
              (tfmap (fn [pred]
                      (when (util/safely-run {:where "Set predicate" :what ent} (pred ent))
                        (finalizer
                         (tfmap #(util/safely-run {:where "Field extractor" :what ent} (% ent))
                               fields))))
                    pred-map)))))))

(defn computed-set
  "Compute a set dynamically as input is processed. set-name-f should
   return nil if the input item belongs in no set, or the name of the
   set if it belongs in one. Note that set-name-f is passed the entire
   entity being processed, so it may have to extract a value from a
   map:

   > (defn bucketize [n]
       (when n
         (let [lower (* 10 (int (/ n 10)))
               upper (+ 10 lower)]
           (keyword (str \"between-\" lower \"-and-\" upper)))))
   #'babbage.core/bucketize
   > (calculate (-> (sets) (computed-set (comp bucketize :x)) (complement :between-0-and-10))
                (stats :x babbage.provided.core/count)
                [{:x 11} {:x 4}])
   {:between-0-and-10 {:count 1},
    :not-between-0-and-10 {:count 1},
    :between-10-and-20 {:count 1},
    :all {:count 2}}

   Note also that (in general) functions manipulating sets, including
   this one, complement, intersection, etc. are like middleware; the
   order matters. The right complement is calculated in the previous
   case because the items are put into buckets *first*. Compare:

   > (calculate (-> (sets) (complement :between-0-and-10) (computed-set (comp bucketize :x)))
                (stats :x babbage.provided.core/count)
                [{:x 11} {:x 4}])
   {:between-0-and-10 {:count 1},
    :between-10-and-20 {:count 1},
    :not-between-0-and-10 {:count 2},
    :all {:count 2}}

   The complement of :between-0-and-10 is calculated incorrectly
   because, when the complement function runs, nothing has been put
   into that set yet, even though something *will* be."
  [f set-name-f]
  (fn [fields]
    (let [f (f fields)]
      (fn [ent]
        (let [res (f ent)]
          (if-let [set-name (set-name-f ent)]
            (assoc res set-name (:all res))
            res))))))

(defmacro ^:private defsetop [name doc destruct [result-name result-body] [res-name res-body]]
  `(defn ~name ~doc [f# ~@destruct]
     (let [~result-name ~result-body]
       (fn [fields#]
         (let [f# (f# fields#)]
           (fn [ent#]
             (let [~res-name (f# ent#)]
               ~res-body)))))))

(defsetop complement
  "Add the complement of the subset named by key. If complement-key is
   provided that will be used as the name of the complement; otherwise
   \"not-\" will be prefixed to key."
  [key & [complement-key]]
  [complement-key (or complement-key (keyword (str "not-" (name key))))]
  [res (if (key res) res (assoc res complement-key (:all res)))])

(defn make-set-function-name
  "(make-set-function-name \"and\" [:a :b :c]) -> :Sa-and-b-and-cZ"
  [logical-operation-name keys]
  (keyword (str "S" (str/join (str "-" logical-operation-name "-") (map name keys)) "Z")))

(defn intersection-name [& keys]
  (make-set-function-name "and" keys))

(defn union-name [& keys]
  (make-set-function-name "or" keys))

(defn complement-name [k]
  (keyword (str "not-" (name k))))

(defsetop intersect
  "Add the intersection of two or more subsets, named by the key
   arguments, to the set given by the first argument. The resulting
   intersection will be named :Sx-and-y-and-...-and-zZ, where x, y, ... z
   are the names of the argument sets."
  [key1 & keys]
  [result-name (make-set-function-name "and" (list* key1 keys))]
  [res (assoc res result-name (when (every? res keys) (get res key1)))])

(defsetop union
  "Add the union of two or more subsets, named by the key arguments,
   to the set given by the first argument. The resulting union will be
   named :Sx-or-y-or-...-or-zZ, where x, y, ... z are the names of the
   argument sets." 
  [key1 & keys]
  [result-name (make-set-function-name "or" (list* key1 keys))]
  [res (assoc res result-name (some res (list* key1 keys)))])

(defn tails [xs]
  (lazy-seq (cons xs (if (empty? xs)
                       '()
                       (tails (rest xs))))))

(defn- pick [xs]
  (for [i (first xs)
        r (if (seq (rest xs)) (pick (rest xs)) [()])]
    (flatten [i r])))

(defn- nested-set-names
  [real-namer & keyseqs]
  (let [keyseqs (map #(if (or (seq? %) (vector? %)) % [%]) keyseqs)
        keyseq-tails (drop-last 2 (tails keyseqs))]
    (mapcat #(map (partial apply real-namer) (pick %)) keyseq-tails)))

(def unions-names (partial nested-set-names union-name))
(def intersections-names (partial nested-set-names intersection-name))

(defn- nested-set-operations
  [real-op op-str f & keyseqs]
  (letfn [(op* [f keyseqs]
            (condp = (clojure.core/count keyseqs)
              0 f 1 f
              2 (reduce (fn [f firsts]
                          (reduce (fn [f seconds] (apply real-op f (flatten (clojure.core/list seconds firsts))))
                                  f
                                  (second keyseqs)))
                        f
                        (first keyseqs))
              (let [generated (for [f (first keyseqs) s (second keyseqs)]
                                (flatten [s f]))]
                (recur (op* f [(first keyseqs) (second keyseqs)])
                       (list* generated (drop 2 keyseqs))))))]
    (op* f (reverse (map #(if (or (seq? %) (vector? %)) % [%]) keyseqs)))))

(def ^{:arglists '([s & keyseqs])
       :doc "Created nested intersections of the set, using the
  cross-products of pairs of the provided keys, starting with the
  innermost:

  (intersections s :foo [:bar :baz] [[:quux :frob] :spam])

  creates the following subsets, in this order:

  Sbar-and-quux-and-frobZ
  Sbar-and-spamZ
  Sbaz-and-quux-and-frobZ
  Sbaz-and-spamZ
  Sfoo-and-bar-and-quux-and-frobZ
  Sfoo-and-bar-and-spamZ
  Sfoo-and-baz-and-quux-and-frobZ
  Sfoo-and-baz-and-spamZ"}
  intersections (partial nested-set-operations intersect "and"))

(def ^{:arglists '([s & keyseqs])
       :doc "Created nested unions of the set, using the
  cross-products of pairs of the provided keys, starting with the
  innermost:

  (unions s :foo [:bar :baz] [[:quux :frob] :spam])

  creates the following subsets, in this order:

  Sbar-or-quux-or-frobZ
  Sbar-or-spamZ
  Sbaz-or-quux-or-frobZ
  Sbaz-or-spamZ
  Sfoo-or-bar-or-quux-or-frobZ
  Sfoo-or-bar-or-spamZ
  Sfoo-or-baz-or-quux-or-frobZ
  Sfoo-or-baz-or-spamZ"}
  unions (partial nested-set-operations union "or"))

;; mapping can be parallelized
;; reduction can be parallelized (ops are associative)
(defn calculate
  ([fields input]
     (calculate (sets) fields input))
  ([sets-fn fields input]
     (let [leaf-fn (sets-fn fields)]
       (when (seq input)
         (m/value (reduce m/<> (pmap leaf-fn input)))))))

(util/if-ns-avail (require '[clojure.core.reducers :as r])
                  (defn r-calculate
                    ([fields input]
                       (calculate (sets) fields input))
                    ([sets-fn fields input]
                       (let [leaf-fn (sets-fn fields)]
                         (when (seq input)
                           (m/value (r/fold (fn ([a b] (m/<> a b))
                                              ([] nil))
                                            m/<>
                                            (r/map leaf-fn input)))))))
                  (def r-calculate calculate))

(defn xget
  "Enables fetching of a value across multiple sets."
  [data sets & ks]
  (map #(get-in data (cons % ks)) sets))

(defn xget*
  "Fetch a value across all sets."
  [data & ks]
  (vals (fmap #(get-in % ks) data)))
