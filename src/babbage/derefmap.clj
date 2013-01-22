(ns babbage.derefmap)

(defn deref-seq [underlying]
  (when-first [[k v] underlying]
    (lazy-seq (cons [k @v] (deref-seq (rest underlying))))))

(deftype DerefMap [m]
  clojure.lang.IPersistentMap
  (assoc [_ k v] (DerefMap. (.assoc m k (delay v))))
  (assocEx [_ k v] (DerefMap. (.assocEx m k (delay v))))
  (without [_ k] (DerefMap. (.without m k)))
  clojure.lang.IPersistentCollection
  (count [_] (count m))
  (cons [_ o] (let [[k v] o] (DerefMap. (.cons m [k (delay v)]))))
  (empty [_] (.empty m))
  (equiv [_ o] (and (isa? (class o) DerefMap)
                    (.equiv m (.m o))))
  clojure.lang.Seqable
  (seq [_] (deref-seq (seq m)))
  clojure.lang.ILookup
  (valAt [_ k] (let [d (.valAt m k)]
                 (when-not (nil? d)
                   @d)))
  (valAt [_ k v] (if (.containsKey m k)
                  (deref (.valAt m k))
                   v))
  clojure.lang.Associative
  (containsKey [_ k] (.containsKey m k))
  (entryAt [_ k] (let [[k v] (.entryAt m k)]
                   [k @v]))
  java.lang.Iterable
  (iterator [this] (.iterator (seq this))))

