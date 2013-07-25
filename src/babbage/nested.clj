(ns babbage.nested
  (:require clojure.pprint
            [babbage.defaultmap :as defaultmap]
            [clojure.set :as set]
            [clojure.algo.generic.functor :as f])
  (:import [babbage.defaultmap DefaultMap]))

(defn curry
  "Return a curried version of f. N.B. the resulting function accepts
   arguments one at a time."
  [f arity]
  (if (== arity 0) (f) (fn [arg] (curry (partial f arg) (dec arity)))))

(defmacro nil-or [alt-1 alt-2]
  `(let [alt-1# ~alt-1]
     (if (nil? alt-1#)
       ~alt-2
       alt-1#)))

(defn- map-apply [default-arg funcs args]
  (cond
   (and (defaultmap/defaultmap? funcs) (defaultmap/defaultmap? args))
   (let [keys (set/union (set (keys funcs)) (set (keys args)))]
     (DefaultMap. ((defaultmap/get-default funcs) (defaultmap/get-default args))
                  (into {} (map (fn [k] [k ((get funcs k) (get args k))]) keys))
                  #{}))
   (defaultmap/defaultmap? funcs)
   (let [default-func (defaultmap/get-default funcs)
         f-keys (set/difference (set (keys funcs)) (set (keys args)))
         func-added (into {} (map (fn [k] [k ((get funcs k) default-arg)]) f-keys))]
     (DefaultMap. (default-func default-arg)
       (->> (map (fn [[k v]] [k ((get funcs k) (or v default-arg))]) args)
            (into {})
            (merge func-added))
       #{}))))

(defn in-maps
  "With three arguments, return a function which, when called with arity
   maps, applies op to the values of the map (as in fmap).

   With more than three arguments, immediately applies op.

   If any key in an argument map has a nil value, it will be replaced
   by the default; if any argument is nil, it will be replaced
   by (pure default). The exception is if *all* arguments are nil, in
   which case the result will be nil.

   (in-maps / 2 1 {:x 6} {:x 2}) --> {:x 3}

   Prefer in-nested-maps for computations at the fringes of maps containing maps containing ...

   Note that the non-nil arguments determine the results that will
   eventually be present; e.g., if one of the maps is {}, rather
   than nil, the result will be {}."  
  ([op arity default] (fn [& args] (apply in-maps op arity default args)))
  ([op arity default & args]
     (assert (== arity (count args))
             (format "op has arity %d but %d arguments provided" arity (count args)))
     (into {} (reduce (partial map-apply default)
                      (defaultmap/defaultmap (curry op arity))
                      (map #(nil-or % (defaultmap/defaultmap default)) args)))))

(defn- in-nested-maps* [nesting op arity default]
  (if (== nesting 1)
    (in-maps op arity default)
    (let [op (in-maps op arity default)]
      (recur (dec nesting) op arity (defaultmap/defaultmap default)))))

(defn in-nested-maps
  "Apply op with the supplied arity and default in maps nested nesting levels deep."
  ([nesting op arity default] (fn [& args] (apply in-nested-maps nesting op arity default args)))
  ([nesting op arity default & args] (apply (in-nested-maps* nesting op arity default) args)))
