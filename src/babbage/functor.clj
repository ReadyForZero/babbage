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
   (nil? argument-map)
   (recur default function-map (Pure. default))
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

(defn in-maps
  "With three arguments, return a function which, when called with arity
   maps, applies op to the values of the map (as in fmap).

   With more than three arguments, immediately applies op.

   If any key in an argument map has a nil value, it will be replaced
   by the default; if any argument is nil, it will be replaced
   by (pure default).

   (in-map / 2 0 {:x 6} {:x 2}) --> {:x 3}

   Prefer in-nested-maps for computations at the fringes of maps containing maps containing ...

   Note that the non-nil arguments determine the results that will
   eventually be present; e.g., if one of the maps is {}, rather
   than nil, the result will be {}.

   Purity and danger: if all the arguments are nil (or, with
   in-nested-maps, if all the values of a given key at a given level
   of nesting are nil), then, since in-maps can't figure out what the
   relevant keys for which to apply the function are, the result will
   be a map-like object that thinks it contains the default value for
   *any* key:

   (in-maps + 2 0 nil nil) --> (pure 0)

   (in-nested-maps 2 + 2 1 {:x {:x 2} :y nil} {:x nil :y nil}) --> {:x {:x 3} :y (pure 2)}

   (We have (pure 2) under the :y key because of the addition (pure 1) + (pure 1).)

   You can use assoc, dissoc, and get with these map-like objects, but
   they are not seqable, *even though* they are instances of
   clojure.lang.Seqable (there seems to be no way to avoid this, since
   Associative extends IPersistentCollection extends Seqable)."
  
  ([op arity default] (fn [& args] (apply in-maps op arity default args)))
  ([op arity default & args]
     (assert (== arity (count args))
             (format "op has arity %d but %d arguments provided" arity (count args)))
     (reduce (partial map-apply default)
             (Pure. (curry op arity))
             (map #(nil-or % (Pure. default)) args))))

(defn- in-nested-maps* [nesting op arity default]
  (if (== nesting 1)
    (in-maps op arity default)
    (let [op (in-maps op arity default)]
      (recur (dec nesting) op arity (Pure. default)))))

(defn in-nested-maps
  "Apply op with the supplied arity and default in maps nested nesting levels deep."
  ([nesting op arity default] (fn [& args] (apply in-nested-maps nesting op arity default args)))
  ([nesting op arity default & args] (apply (in-nested-maps* nesting op arity default) args)))
