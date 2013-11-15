;; Owner: wolfson@readyforzero.com
;; protocol for combining ops
(ns babbage.monoid
  (:require [clojure.set :as set]
            [potemkin])
  (:use [clojure.algo.generic.functor :only [fmap]]))

(defprotocol Monoid
  (<> [self other] "'add' two monoidal values of the same type.")
  (mempty [self] "return the zero element for this type.")
  (mempty? [self] "Is this the zero element?")
  (value [self] "return the wrapped value"))

(deftype MkMonoid [op my-val zero val-fn]
  Monoid
  (<> [self other]
    (cond (mempty? self) other
          (mempty? other) self
          :otherwise (let [^MkMonoid other other]
                       (MkMonoid. op (op my-val (.my-val other)) zero val-fn))))
  (mempty [self] (MkMonoid. op zero zero val-fn))
  (mempty? [self]
    (or (nil? my-val)
        (= zero my-val)))
  (value [self] (if val-fn (val-fn my-val) my-val)))

(def mk-monoid ->MkMonoid)

(defn monoid [op zero & [val-fn]]
  (fn ([my-val] (mk-monoid op (if (nil? my-val) zero my-val) zero val-fn))
    ([] (mk-monoid op zero zero val-fn))))

(defn delegate [v f]
  (MkMonoid. <> v (mempty v) f))

(deftype Fun [f]
  Monoid
  (<> [self other]
    (if (nil? other) self
        (let [^Fun other other
              of (.f other)]
          (Fun. (fn [x] (<> (f x) (of x)))))))
  (mempty [self] (Fun. identity))
  (mempty? [self] false) ;; no good way to test for this.
  (value [self] f))

(extend-protocol Monoid
  ;; the accumulators library treated maps as collections of values:
  ;; (<> {:x 6} [:y 5]) --> {:x 6 :y 5}
  ;; we, instead, treat maps as collections of named monoids.
  clojure.lang.IPersistentMap
  (<> [self other] (merge-with <> self other))
  (mempty [self] (fmap mempty self))
  (mempty? [self] (every? mempty?
                          (vals self)))
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
