;; Owner: wolfson@readyforzero.com
;; calculating interesting stats
(ns babbage.core
  (:require [babbage.monoid :as m]
            [babbage.util :as util]
            [clojure.string :as str])
  (:use [clojure.algo.generic.functor :only [fmap]]
        [trammel.core :only [defconstrainedfn]])
  (:refer-clojure :exclude [complement]))

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
  (let [m (reduce (fn [acc f] (f acc)) {} (cons stat-func1 stats-funcs))
        m (util/prepare-map m)]
    (fn [ent]
      (let [v (extractor ent)]
        (fmap (fn [f]
                (if (-> f meta :whole-record)
                  (f ent v)
                  (f v))) m)))))

(defmacro statfn
  "Create a function for computing statistics suitable for passing as
  an argument to stats. \"monoidfn\" should be the underlying function
  that combines values. The computed value will be inserted in the
  result map with the same name as fn-name or, if it is provided, the
  value of the :name optional argument. The :requires optional
  argument should contain the names of the functions that monoidfn
  expects already to have run.

  Functions that are more than just simple wrappers will probably not
  be able to use this macro: see e.g. ratio or
  map-{with-key,with-value,of}."
  [fn-name monoidfn & {:keys [requires name]}]
  (let [requires (cond (nil? requires) []
                       (vector? requires) requires
                       :else [requires])
        threading-safe (map clojure.core/list requires)
        kwname (or name (keyword fn-name))]
    `(fn ~fn-name [m#] (-> m# ~@threading-safe (assoc ~kwname ~monoidfn)))))

(defmacro defstatfn
  "Define a function for computing statistics suitable for passing as
   an argument to stats. Same as statfn, except that def is used to
   create a var holding the function, and there is an additional :doc
   optional argument for supplying a docstring."
  [fn-name monoidfn & {:keys [requires name doc]}]
  `(def ~fn-name (with-meta (statfn ~fn-name ~monoidfn :requires ~requires :name ~name)
                   {:doc ~doc})))

(defn by
   "Allows passing whole records to a stats function within a call to
   stats. Similar to map-with-key and map-with-value, but without
   creating a map."
   [extractor field-name sfunc1 & sfuncs]
  (let [sfuncs (cons sfunc1 sfuncs)
        prepared (apply stats identity sfuncs)]
    (fn [m] (assoc m field-name (with-meta (fn [ent v] (when v (prepared (extractor ent))))
                                 {:whole-record true})))))

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
  (let [sfuncs (cons value-sfunc1 value-sfuncs)
        prepared (apply stats identity sfuncs)]
    (fn [m] (assoc m field-name (with-meta (fn [ent v]
                                            (when v
                                              {(key-extractor ent)
                                               (prepared v)}))
                                 {:whole-record true})))))

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
  (let [sfuncs (cons value-sfunc1 value-sfuncs)
        prepared (apply stats value-extractor sfuncs)]
    (fn [m] (assoc m field-name (with-meta (fn [ent v]
                                            (when v
                                              {v (prepared ent)}))
                                 {:whole-record true})))))

(defconstrainedfn map-of
  "Add a map named field-name to the result map. With type :key, as
   map-with-key; with type :value, as map-with-value."
  [type extractor field-name sfunc1 & sfuncs]
  [(#{:key :value} type)]
  (case type
    :key (apply map-with-key extractor field-name sfunc1 sfuncs)
    :value (apply map-with-value extractor field-name sfunc1 sfuncs)))

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
              (fmap (fn [pred]
                      (when (pred ent)
                        (finalizer (fmap #(% ent) fields)))) pred-map)))))))

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

(defn xget
  "Enables fetching of a value across multiple sets."
  [data sets & ks]
  (map #(get-in data (cons % ks)) sets))
