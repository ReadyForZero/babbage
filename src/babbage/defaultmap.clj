(ns babbage.defaultmap
  (:require [clojure.algo.generic.functor :as f]))

(set! *warn-on-reflection* true)

(deftype DefaultMap [default added deleted]
  clojure.lang.IPersistentMap
  (assoc [me key val] (DefaultMap. default (assoc added key val) (disj deleted key)))
  (assocEx [me key val] (assoc me key val))
  (without [me key] (DefaultMap. default (dissoc added key) (conj deleted key)))

  clojure.lang.Counted
  (count [me] (- (count added) (count deleted)))

  clojure.lang.Associative
  (containsKey [me key] (not (contains? deleted key)))
  (entryAt [me key] (when-not (contains? deleted key)
                      (if (contains? added key)
                        (clojure.lang.MapEntry. key (get added key))
                        (clojure.lang.MapEntry. key default))))

  clojure.lang.ILookup
  (valAt [me key] (let [entry (.entryAt me key)]
                    (if (nil? entry)
                      nil
                      (.val entry))))
  (valAt [me key not-found] (let [entry (.entryAt me key)]
                              (if (nil? entry)
                                not-found
                                (.val entry))))

  clojure.lang.IPersistentCollection
  (empty [me] (DefaultMap. default {} #{}))
  (equiv [me o] (and (map? o)
                     (= (seq o) (seq me))))
  (cons [me o] (if (instance? clojure.lang.MapEntry o)
                 (let [^clojure.lang.MapEntry o o]
                   (assoc me (.key o) (.val o))
                   (assoc me (first o) (second o)))))

  clojure.lang.Seqable
  (seq [me] (seq (reduce dissoc added deleted)))

  java.lang.Iterable
  (iterator [me] (.iterator ^clojure.lang.APersistentMap (reduce dissoc added deleted))))

(defmethod f/fmap DefaultMap
  [f ^DefaultMap m]
  (DefaultMap. (f (.default m)) (f/fmap f (.added m)) (.deleted m)))

(defn defaultmap [default]
  (DefaultMap. default {} #{}))

(defn defaultmap? [o]
  (instance? DefaultMap o))

(defn get-default [^DefaultMap o]
  (.default o))
