(ns babbage.derefcolls
  (:require [potemkin]))

(defn derefable [o]
  (reify clojure.lang.IDeref
    (deref [_] o)))

(potemkin/def-map-type DerefMap [m]
  (get [_ k default-value]
       (if (contains? m k)
         (let [v (get m k)]
           @v)
         default-value))
  (assoc [_ k v]
    (DerefMap. (assoc m k (derefable v))))
  (dissoc [_ k]
          (DerefMap. (dissoc m k)))
  (keys [_]
        (keys m)))

(deftype DerefSeq [s]
  clojure.lang.Sequential
  clojure.lang.Counted
  (count [_] (count s))

  clojure.lang.ISeq
  (first [_] (when-let [f (first s)] @f))
  (next [_] (when-let [n (next s)] (DerefSeq. n)))
  (more [me] (let [x (next me)]
               (if (empty? x)
                 ()
                 x)))
  (cons [_ o] (DerefSeq. (cons s (derefable o))))

  clojure.lang.IPersistentCollection
  (empty [_] (DerefSeq. ()))
  (equiv [m o] (.equiv o (map deref s)))

  clojure.lang.IHashEq
  (hasheq [_] (.hasheq (map deref s)))

  clojure.lang.Seqable
  (seq [me] me)

  clojure.lang.IReduce
  (reduce [me f] (reduce f (map deref me)))
  (reduce [me f start] (reduce f start (map deref me)))

  clojure.lang.IObj
  (withMeta [me m] (DerefSeq. (.withMeta s m)))

  clojure.lang.IMeta
  (meta [_] (meta s))

  Object
  (hashCode [_] (.hashCode (map deref s)))
  (equals [_ x] (.equals x (map deref s)))
  (toString [_] (.toString s))
  )
