;; Owner: wolfson@readyforzero.com
;; protocol for combining ops
(ns babbage.monoid
  (:require [clojure.set :as set])
  (:use [clojure.algo.generic.functor :only [fmap]]))

(defprotocol Monoid
  (<> [self other] "'add' two monoidal values of the same type.")
  (mempty [self] "return the zero element for this type.")
  (mempty? [self] "Is this the zero element?")
  (value [self] "return the wrapped value"))

(defn mk-monoid [op my-val zero]
  (reify Monoid
    (<> [self other]
      (cond (mempty? other) self
            (mempty? self) other
            :otherwise (mk-monoid op (op my-val (value other)) zero)))
    (mempty [self]
      (mk-monoid op zero zero))
    (mempty? [self]
      (or (nil? my-val)
          (= zero my-val)))
    (value [self]
      my-val)))

(defn monoid [op zero]  
  (fn ([& [my-val]] (mk-monoid op (if (nil? my-val) zero my-val) zero))))

(extend-protocol Monoid
  ;; the accumulators library treated maps as collections of values:
  ;; (<> {:x 6} [:y 5]) --> {:x 6 :y 5}
  ;; we, instead, treat maps as collections of named monoids.
  clojure.lang.IPersistentMap
  (<> [self other] (merge-with <> self other))
  (mempty [self] (fmap mempty self))
  (mempty? [self] (every? mempty? (vals self)))
  (value [self] (fmap value self))

  clojure.lang.IPersistentList
  (<> [self other] (apply list (concat self other)))
  (mempty [self] '())
  (mempty? [self] (empty? self))
  (value [self] self)

  clojure.lang.IPersistentSet
  (<> [self other] (set/union self other))
  (mempty [self] #{})
  (mempty? [self] (empty? self))
  (value [self] self)

  clojure.lang.IPersistentVector
  (<> [self other] (vec (concat self other)))
  (mempty [self] [])
  (mempty? [self] (empty? self))
  (value [self] self)

  clojure.lang.PersistentVector$ChunkedSeq
  (<> [self other] (concat self other))
  (mempty [self] ())
  (mempty? [self] (empty? self))
  (value [self] self)

  clojure.lang.LazySeq
  (<> [self other] (concat self other))
  (mempty [self] '())
  (mempty? [self] (empty? self))
  (value [self] self)

  clojure.lang.ArraySeq
  (<> [self other] (into (empty self) (concat self other)))
  (mempty [self] (empty self))
  (mempty? [self] (empty? self))
  (value [self] self)
  
  nil
  (<> [self other] other)
  (mempty [self] nil)
  (mempty? [self] true)
  (value [self] nil))
