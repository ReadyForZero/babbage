;; Owner: wolfson@readyforzero.com
;; protocol for combining ops
(ns babbage.monoid
  (:refer-clojure :exclude [max min])
  (:require [clojure.set :as set]
            [incanter.core :as incanter])
  (:use [clojure.algo.generic.functor :only [fmap]]))

(defprotocol Monoid
  (<> [self other] "'add' two monoidal values of the same type.")
  (mempty [self] "return the zero element for this type.")
  (mempty? [self] "Is this the zero element?")
  (value [self] "return the wrapped value"))

(defn- mk-monoid [op my-val zero]
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
  (fn ([& [my-val]] (mk-monoid op (or my-val zero) zero))))

(def max (monoid clojure.core/max (/ -1 0.0))) ;; negative and
(def min (monoid clojure.core/min (/ 1 0.0)))  ;; positive infinity
(def sum (monoid + 0))
(def prod (monoid * 1))
(def snd (monoid (fn [s o] o) nil))
(def fst (monoid (fn [s o] s) nil))

(def rows (monoid incanter/bind-rows []))
(def cols (monoid incanter/bind-columns []))

(defn dependence [dependent independents]
  {:dependent (rows [dependent])
   :independents (rows independents)})

(defn count-binned [o]
  (if (nil? o) {} {o (sum 1)}))

(defn- interval-< [i1 i2]
  (< (second i1) (first i2)))

(defn copy-meta [o1 o2]
  (with-meta o2 (meta o1)))

(defn add-histogram [h1 h2]
  ;; there is a gap if the upper bound of h1 is less than the lower
  ;; bound of h2, or the upper bound of h2 is less than the lower
  ;; bound of h1.
  (let [width (-> h1 meta :width)
        h2width (-> h2 meta :width)]
    (assert (= width h2width) "cannot add histograms with unequal box widths")
    (if (interval-< (apply max-key second (keys h2)) (apply min-key first (keys h1)))
      (recur h2 h1)
      (copy-meta
       h1
       (merge-with + h1
                   (if (interval-< (apply max-key second (keys h1)) (apply min-key first (keys h2)))
                     (let [target (apply clojure.core/max (map second (keys h1)))
                           start (apply clojure.core/min (map first (keys h2)))]
                       (loop [h2 h2 last start]
                         (if (= last target)
                           h2
                           (recur (assoc h2 [(- last width) last] 0) (- last width)))))
                     h2))))))

(defn histogram [width]
  (fn [& [my-val]]
    (let [my-interval (when my-val (int (- my-val (mod my-val width))))]
      (mk-monoid add-histogram
                 (with-meta 
                   (if my-val
                     {[my-interval (+ my-interval width)] 1}
                     {})
                   {:width width})
                 {}))))

(defn add-vector-space [& vs]
  (assert (apply = (map #(map count %) vs)) "Vectors must be of the same dimensionality.")
  (with-meta
    (apply concat vs)
    {:maxes  (apply map clojure.core/max (keep #(-> % meta :maxes) vs))
     :mins   (apply map clojure.core/min (keep #(-> % meta :mins) vs))
     :sums   (apply map + (keep #(-> % meta :sums) vs))
     :counts (reduce + 0 (keep #(-> % meta :counts) vs))}))

(defn make-vector-space-value
  "Given a vector or point, 'v', constructs a single-element vector space with that point."
  [v]
  (with-meta (vector v) {:maxes v
                         :mins v
                         :sums v
                         :counts 1}))

(defn vector-space [& [v]]
  (mk-monoid add-vector-space
             (make-vector-space-value v)
             [nil]))

;; following http://izbicki.me/blog/gausian-distributions-are-monoids
(defrecord Gaussian [n m m2]
  Monoid
  (<> [self other]
    (if (mempty? other)
      self
      (let [{on :n om :m om2 :m2} other
            n' (+ n on)
            m' (/ (+ (* n m) (* on om)) n')
            m2' (- (+ m2 (* n (Math/pow m 2)) om2 (* on (Math/pow om 2)))
                   (* n' (Math/pow m' 2)))]
        (Gaussian. n' m' m2'))))
  (mempty [self] nil)
  ;; a Gaussian record created with the gaussian fn always has a count
  ;; of at least 1, so is never empty.
  (mempty? [self] false)
  (value [self] {:count n :mean m
                 :variance (if (not= 1 n) (/ m2 (- n 1)) 0)}))

(defn gaussian [& [v]]
  (when v (Gaussian. 1 (double v) 0)))

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

  clojure.lang.LazySeq
  (<> [self other] (concat self other))
  (mempty [self] '())
  (mempty? [self] (empty? self))
  (value [self] self)
  
  nil
  (<> [self other] other)
  (mempty [self] nil)
  (mempty? [self] true)
  (value [self] nil))
