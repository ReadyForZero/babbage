(ns babbage.functor
  (:require clojure.pprint
            [clojure.algo.generic.functor :as f]))

(defn curry
  "Return a curried version of f, which will continue currying itself
   until it has received arity arguments. N.B. the resulting function
   accepts arguments one at a time."
  [f arity]
  (if (== arity 0) (f) (fn [arg] (curry (partial f arg) (dec arity)))))

(def arb 'arb)

(deftype Pure [o]
  clojure.lang.IPersistentMap
  (without [this _] this)
  (assoc [this _ _] this)
  (assocEx [this _ _] this)
  clojure.lang.ILookup
  (containsKey [this _] true)
  (entryAt [this k] [k o])
  (valAt [this _] o))

(defmethod f/fmap Pure
  [f m]
  (Pure. (f (get m arb))))

(defmethod print-method Pure [o ^java.io.Writer w]
  (.write w "(new ")
  (pr Pure)
  (.write w " ")
  (print-method (get o arb) w)
  (.write w ")"))

(defmethod clojure.pprint/simple-dispatch Pure
  [o]
  (clojure.pprint/pprint {arb (get o arb)}))

(defn pure? [o]
  (= Pure (type o)))

(defmacro nil-or [alt-1 alt-2]
  `(let [alt-1# ~alt-1]
     (if (nil? alt-1#)
       ~alt-2
       alt-1#)))

(defn- map-apply [default function-map argument-map]
  (cond
   (and (pure? function-map) (pure? argument-map))
   (Pure. ((get function-map arb) (or (get argument-map arb) default)))
   (pure? function-map)
   (->> argument-map
        (map (fn [[k v]] [k ((get function-map k) (if (nil? v) default v))]))
        (into {}))
   :else
   (->> function-map
        (map (fn [[k v]] [k (v (nil-or (get argument-map k) default))]))
        (into {}))))


(defn any-pure? [m]
  (or (pure? m)
      (and (map? m)
           (some any-pure? (vals m)))
      false))

(defn purify
  "Replace all pure values in a map with nil."
  [m]
  (cond (pure? m) nil
        (map? m) (f/fmap purify m)
        :else m))

(defn in-maps
  "With three arguments, return a function which, when called with arity
   maps, applies op to the values of the map (as in fmap).

   With more than three arguments, immediately applies op.

   If any key in an argument map has a nil value, it will be replaced
   by the default; if any argument is nil, it will be replaced
   by (pure default). The exception is if *all* arguments are nil, in
   which case the result will be nil.

   (in-map / 2 0 {:x 6} {:x 2}) --> {:x 3}

   Prefer in-nested-maps for computations at the fringes of maps containing maps containing ...

   Note that the non-nil arguments determine the results that will
   eventually be present; e.g., if one of the maps is {}, rather
   than nil, the result will be {}."  
  ([op arity default] (fn [& args] (apply in-maps op arity default args)))
  ([op arity default & args]
     (assert (== arity (count args))
             (format "op has arity %d but %d arguments provided" arity (count args)))
     (purify
      (reduce (partial map-apply default)
              (Pure. (curry op arity))
              (map #(nil-or % (Pure. default)) args)))))

(defn- in-nested-maps* [nesting op arity default]
  (if (== nesting 1)
    (in-maps op arity default)
    (let [op (in-maps op arity default)]
      (recur (dec nesting) op arity (Pure. default)))))

(defn in-nested-maps
  "Apply op with the supplied arity and default in maps nested nesting levels deep."
  ([nesting op arity default] (fn [& args] (apply in-nested-maps nesting op arity default args)))
  ([nesting op arity default & args] (apply (in-nested-maps* nesting op arity default) args)))
