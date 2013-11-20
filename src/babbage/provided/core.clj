;; Elemental accumulators.

(ns babbage.provided.core
  (:require [babbage.monoid :as monoid :refer [monoid]]
            [babbage.provided.histogram :as histogram]
            [babbage.provided.gaussian]
            [clojure.core :as clj]
            [clojure.set :as s])
  (:import [babbage.provided.gaussian Gaussian])
  (:use [babbage.core :only [defstatfn statfn stats post consolidate by]]
        babbage.util
        [clojure.algo.generic.functor :only [fmap]]
        [trammel.core :only [defconstrainedfn]])
  (:refer-clojure :exclude [max min count set list first last]))

(set! *warn-on-reflection* true)

(def m-first (monoid (fn [a b] a) nil))
(def m-last (monoid (fn [a b] b) nil))

(defstatfn first m-first)
(defstatfn last m-last)

(def m-min (monoid clojure.core/min Double/POSITIVE_INFINITY))
(defstatfn min m-min)

(def m-max (monoid clojure.core/max Double/NEGATIVE_INFINITY))
(defstatfn max m-max)

(def m-prod (monoid * 1))
(defstatfn prod m-prod)

(def m-sum (monoid + 0)) ;; Sum monoid, used for next two stats.
(defstatfn sum m-sum)
(defstatfn count (fn [x] (m-sum (if (nil? x) 0 1))))

(deftype Any [b]
  monoid/Monoid
  (<> [self other] (if b self other))
  (mempty [self] (Any. false))
  (mempty? [self] (not b))
  (value [self] b))

(deftype All [b]
  monoid/Monoid
  (<> [self other] (if b other self))
  (mempty [self] (All. true))
  (mempty? [self] (boolean b))
  (value [self] b))

(defstatfn any ->Any)
(defstatfn all ->All)

(defstatfn list (fn [x] (when-not (nil? x) [x]))) 
(defstatfn set (fn [x] (when-not (nil? x) #{x})))

;; "mean" is dependent on other computed statistics, and has the
;; computation function defined instead of a monoid.
(deftype Mean [s c]
  monoid/Monoid
  (<> [self other] (if other
                     (let [^Mean other other]
                       (Mean. (+ s (.s other))
                              (+ c (.c other))))
                     self))
  (mempty [self] nil)
  (mempty? [self] (== (.c self) 0))
  (value [self] (when-not (== c 0)
                  (/ s  (double c)))))

(defn mean-m [x] (when x (Mean. x 1)))

(defstatfn mean mean-m)

;; Keep track of counts for each item. This result of this is like 'frequencies'.
(defstatfn count-binned
  (fn [o]
    (if (nil? o) (sorted-map) (sorted-map o (m-sum 1)))))

(def count-unique (post :count-unique clj/count set))

(defn count-binned-normalized [result-name count-name bin-name]
  {:requires [count-name bin-name]
   :name result-name
   :processor (fn [m] (when-let [c (count-name m)]
                       (assoc m result-name (fmap #(/ (double %) c) (bin-name m)))))})

(defn rate [ts-extractor]
  (post :rate (fn [m] (when (> (:count m) 1)
                       (/ (- (:last m) (:first m)) (double (dec (:count m))))))
        (consolidate (by ts-extractor last first) count)))

(def count-binned-normalized* (count-binned-normalized :count-binned-normalized
                                                       :count :count-binned))

(defn ratio [of to & [ratio-name]]
  (let [key (or ratio-name (keyword (str (name of) "-to-" (if (number? to) to (name to)))))]
    {:requires (if (number? to) [of] [of to])
     :name key
     :processor (if (number? to)
                  (let [to (double to)]
                    (fn [m] (assoc m key (/ (get m of) to))))
                  (fn [m] (assoc m key (/ (double (get m of)) (get m to)))))}))

(defn histogram
  "Given a width, returns a function that results in a histogram, where buckets are of 'width' width."
  [width]
  (let [h (histogram/m-histogram width)]
    (statfn histogram h)))

(defstatfn gaussian
  (fn [& [v _]]
    (when v (Gaussian. 1 (double v) 0))))
