(ns babbage.mseq
  (:require [babbage.monoid :as m]
            [clojure.algo.generic.functor :as f]))

;; A structure that carries around a count and a total and calculates
;; an mean based on them is, in combination with a zero-count
;; sentinel (or a delayed computation of the mean), a monoid, but:
;; 
;; 1. if we're carrying a count and a total around in a map anyway,
;;    there's no reason for the "mean" structure to carry them
;;    around too;
;; 2. again, in such a case, the <> operation discards the calculated
;;    mean at each step and needs to recalculate it, so we should
;;    only really bother with it when we're done.
;;
;; We could get around 2 by only calculating the mean on demand, as
;; in:
;;
;; (defrecord Mean [total count]
;;   Monoid
;;   (<> [self other]
;;     (cond (mempty? self) other
;;           (mempty? other) self
;;           :else (mean (+ count (:count other))
;;                          (+ total (:total other)))))
;;   (mempty [self] (mean 0 0))
;;   (value [self] (when-not (zero? count) (/ total (double count))))))
;; (def mean ->Mean)
;;
;; However, if we're already tracking the count and total
;; independently, this duplicates work. Moreover, not everything we're
;; interested in calculating might be amenable to being reconstrued as
;; a monoid, without lots of wasted calculations. So, we define
;; sequence-like monoid (a monoid-like sequence?) for which <>
;; operates only on the first element, and which constructs its
;; overall value by folding the value of its first monoidal element
;; with the result of calling the functions that constitute the
;; remainder of its elements:

(defrecord MSeq [ms]
  ;; the rests of ms and (:ms other) are assumed to be identical.
  m/Monoid
  (<> [self other]
    (if-let [s (first ms)]
      (if-let [o (first (:ms other))]
        (->MSeq (list* (m/<> s o) (rest ms)))
        self)
      other))
  (mempty [self] (->MSeq (list* (m/mempty (first ms)) (rest ms))))
  (mempty? [self] (or (empty? ms) ;; empty? is true of nil
                      (m/mempty? (first ms))))
  (value [self]
    (when-not (empty? ms)
      (let [v (m/value (first ms))]
        (reduce (fn [acc f] (f acc)) v (rest ms))))))

(defmethod f/fmap MSeq [f mseq]
  (let [ms (:ms mseq)]
    (if (seq ms)
      (MSeq. (list* (f/fmap f (first ms)) (rest ms)))
      (MSeq. []))))