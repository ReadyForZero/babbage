;; Owner: wolfson@readyforzero.com
;; stats-related stuff that depends on previous calculations
(ns babbage.dependent
  (:require [babbage.monoid :as m]
            [clojure.algo.generic.functor :as f]
            [clojure.set :as set]
            [incanter.charts :as charts]
            [incanter.stats :as istats])
  (:use babbage.util        
        [trammel.core :only [defconstrainedfn]]))

;; a structure that carries around a count and a total and calculates
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

(defnmeta mean {:requires #{:count :sum}} [m]
  (when-let [count (get m :count)]
    (when-not (zero? count)
      (/ (get m :sum) (double count)))))

(defnmeta count-unique {:requires #{:count-binned}} [m]
  (count (get m :count-binned)))

(defnmeta histogram-plot {:requires #{:histogram}} [m]
  (let [h* (-!> m :histogram)
        h* (sort-by ffirst h*)]
    (charts/bar-chart (map first h*) (map second h*))))

(defnmeta bin-histogram-plot {:requires #{:count-binned}} [m]
  (let [h* (-!> m :count-binned)]
    (charts/bar-chart (keys h*) (vals h*))))

(defnmeta scatter-plot {:requires #{:vector-space}} [m]
  (let [r (charts/xy-plot)]
    (doseq [points (-!> m :vector-space)]
      (apply charts/add-points* r points))
    r))

(defn pearson-compute [means vs]
  (letfn [(compute-stats [pairs v]
            (for [p pairs]
              (let [x-index (:x p)
                    x1 (- (nth v x-index) (nth means x-index))
                    y-index (:y p)
                    y1 (- (nth v y-index) (nth means y-index))]
                {p {:x1 x1
                    :y1 y1
                    :x1y1 (* x1 y1)
                    :x2 (* x1 x1)
                    :y2 (* y1 y1)}})))]
    (let [pairs (for [i (range (count means)) j (range (inc i) (count means))] {:x i :y j})
          sufficients (->> vs
                           (mapcat (partial compute-stats pairs))
                           ;; Do a 2-level deep merge, so that the sufficient stats in the second map are added.
                           (reduce (fn [memo m] (merge-with (partial merge-with +) memo m)) {}))]
      (->> (for [[pair values] sufficients]
             (let [{:keys [x y]} pair]
               {(keyword (str "r-" x "-" y))
                (/ (-> values :x1y1)
                   (* (-> values :x2 Math/sqrt) (-> values :y2 Math/sqrt)))}))
           (into {})))))

(defnmeta pearson {:requires #{:vector-space}} [m]
  (let [{:keys [sums counts]} (-!> m :vector-space meta)
        means (map #(/ % counts) sums)]
    (pearson-compute means (-!> m :vector-space))))

;; here be ugly haxx.
;; see: https://github.com/liebke/incanter/issues/104
(defn linear-model [y x]
  (letfn [(correct-scalar-abs [x]
            (if (number? x)
              (if (< x 0) (*' -1 x) x)
              (map correct-scalar-abs x)))]
    (with-redefs [istats/scalar-abs correct-scalar-abs]
      (istats/linear-model y x))))

(defnmeta linreg {:requires #{:dependence}} [m]
  (linear-model (-> m :dependence :dependent)
                (-> m :dependence :independents)))

(defn ratio
  [of to]
  (let [requirements (if (keyword? to) #{of to} #{of})
        tofn (if (number? to) (fn [_] to) (fn [m] (get m to)))]
    (with-meta
      (fn [m] (when-let [denom (double (tofn m))] (/ (get m of) denom)))
      {:requires requirements})))
