;; Elemental accumulators.

(ns babbage.provided.core
  (:require [babbage.provided.histogram :as histogram])
  (:use [babbage.core :only [defstatfn statfn]]
        [babbage.monoid :only [monoid]]
        babbage.util
        [trammel.core :only [defconstrainedfn]])
  (:refer-clojure :exclude [max min count set list complement]))

(def m-min (monoid clojure.core/min Double/POSITIVE_INFINITY))
(defstatfn min m-min)

(def m-max (monoid clojure.core/max Double/NEGATIVE_INFINITY))
(defstatfn max m-max)

(def m-prod (monoid * 1))
(defstatfn prod m-prod)

(def m-sum (monoid + 0)) ;; Sum monoid, used for next two stats.
(defstatfn sum m-sum)
(defstatfn count (fn [x] (m-sum (if (nil? x) 0 1))))

;; These collections are extended by MSeq.
(defstatfn list (fn [x] [x])) 
(defstatfn set (fn [x] #{x}))

;; "mean" is dependent on other computed statistics, and has the
;; computation function defined instead of a monoid.
(defnmeta d-mean {:requires #{:count :sum}} [m]
  (when-let [count (get m :count)]
    (when-not (zero? count)
      (/ (get m :sum) (double count)))))

(defstatfn mean d-mean :requires [count sum])


;; Keep track of counts for each item. This result of this is like 'frequencies'.
(defstatfn count-binned
  (fn [o]
    (if (nil? o) (sorted-map) (sorted-map o (m-sum 1)))))


;; Keeping track of unique counts is dependent on keeping track of the seen ones.
(defnmeta d-count-unique {:requires #{:count-binned}} [m]
  (.count (get m :count-binned)))

(defstatfn count-unique d-count-unique :requires count-binned :name :unique)


;; Keep track of a ratio of one measure to another. This does not use defnmeta because the
;; requirements are determined from the arguments.
(defn d-ratio
  [of to]
  (let [requirements (if (keyword? to) #{of to} #{of})
        tofn (if (number? to) (fn [_] to) (fn [m] (get m to)))]
    (with-meta
      (fn [m] (when-let [denom (double (tofn m))] (/ (get m of) denom)))
      {:requires requirements})))

(defconstrainedfn ratio
  [of to & [ratio-name]]
  [(keyword? of) (or (number? to) (keyword? to)) (or (nil? ratio-name) (keyword? ratio-name))]
  (let [to-s (if (number? to) (str to) (name to))
        key (or ratio-name (keyword (str (name of) "-to-" to-s)))]
    (statfn ratio (d-ratio of to) :name key)))

(defn histogram
  "Given a width, returns a function that results in a histogram, where buckets are of 'width' width."
  [width]
  (let [h (histogram/m-histogram width)]
    (statfn histogram h)))
