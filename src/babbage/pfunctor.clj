(ns babbage.pfunctor  
  (:require [babbage.util :as u]
            [babbage.mseq]
            [clojure.algo.generic.functor :as f])
  (:import [babbage.mseq MSeq]))

;; reducers-based implementations of fmap for various containers,
;; falling back to regular fmap.
;; makes a difference on very large containers ... which we may not
;; actually encounter:
;; babbage.pfunctor> (def d (let [keys (map (comp keyword str) (range 10000)) vals (range 10000)]
;;                            (into {} (map vector keys vals))))
;; #'babbage.pfunctor/d
;; babbage.pfunctor> (time (dotimes [_ 10] (pfmap inc d)))
;; "Elapsed time: 66.010341 msecs"
;; nil
;; babbage.pfunctor> (time (dotimes [_ 10] (f/fmap inc d)))
;; "Elapsed time: 100.195787 msecs"
;; nil

(u/if-ns-avail
  (require '[clojure.core.reducers :as r])
 (do
     (defmulti pfmap
       (fn [f s] (type s)))
     (defmethod pfmap clojure.lang.LazySeq
       [f v]
       (into (empty v) (r/map f v)))
     (defmethod pfmap clojure.lang.IPersistentList
       [f v]
       (into (empty v) (r/map f v)))
     (defmethod pfmap clojure.lang.IPersistentMap
       [f v]
       (into {} (r/map (fn [[k v]] [k (f v)]) v)))
     (defmethod pfmap clojure.lang.IPersistentVector
       [f v]
       (into (empty v) (r/map f v)))
     (defmethod pfmap MSeq [f ^MSeq mseq]
       (let [ms (.ms mseq)]
         (if (seq ms)
           (MSeq. (list* (pfmap f (first ms)) (rest ms)))
           (MSeq. []))))
     (defmethod pfmap :default
       [f v]
       (f/fmap f v)))
 (def pfmap f/fmap))

(defmethod f/fmap clojure.lang.LazySeq
  [f v]
  (map f v))
