;; Owner: wolfson@readyforzero.com
;; stats-related stuff that depends on previous calculations
(ns babbage.dependent
  (:require [babbage.monoid :as m]
            babbage.mseq
            [clojure.algo.generic.functor :as f]
            [clojure.set :as set])
  (:import babbage.mseq.MSeq)
  (:use babbage.util        
        [trammel.core :only [defconstrainedfn]]))

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

(defn ratio
  [of to]
  (let [requirements (if (keyword? to) #{of to} #{of})
        tofn (if (number? to) (fn [_] to) (fn [m] (get m to)))]
    (with-meta
      (fn [m] (when-let [denom (double (tofn m))] (/ (get m of) denom)))
      {:requires requirements})))
