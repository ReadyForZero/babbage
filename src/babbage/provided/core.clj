;; Elemental accumulators.

(ns babbage.provided.core
  (:require [babbage.monoid :as monoid :refer [monoid]]
            [babbage.provided.histogram :as histogram]
            [babbage.provided.gaussian]
            [clojure.core :as clj]
            [clojure.set :as s])
  (:import [babbage.provided.gaussian Gaussian])
  (:use [babbage.core :only [post consolidate by]]
        babbage.util
        [clojure.algo.generic.functor :only [fmap]]
        [trammel.core :only [defconstrainedfn]])
  (:refer-clojure :exclude [max min count set list first last]))

(set! *warn-on-reflection* true)

(def m-first (monoid (fn [a b] a) nil))
(def m-last (monoid (fn [a b] b) nil))

(defn ignore-index [mf]
  (fn [v i] (mf v)))

(def first {:name :first
            :monoid-fun (ignore-index m-first)})

(def last {:name :last
           :monoid-fun (ignore-index m-last)})

(def m-min (monoid clojure.core/min Double/POSITIVE_INFINITY))
(def min {:name :min
          :monoid-fun (ignore-index m-min)})

(def m-max (monoid clojure.core/max Double/NEGATIVE_INFINITY))
(def max {:name :max
          :monoid-fun (ignore-index m-max)})

(def m-prod (monoid * 1))
(def prod {:name :prod
           :monoid-fun (ignore-index m-prod)})

(def m-sum (monoid + 0)) ;; Sum monoid, used for next two stats.
(def sum {:name :sum
          :monoid-fun (ignore-index m-sum)})
(def count {:name :count
            :monoid-fun (fn [x _] (m-sum (if (nil? x) 0 1)))})

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

(def any {:name :any
          :monoid-fun (ignore-index ->Any)})

(def all {:name :all
          :monoid-fun (ignore-index ->All)})

(def list {:name :list
           :monoid-fun (fn [x _] (when-not (nil? x) [x]))})

(def set {:name :set
          :monoid-fun (fn [x _] (when-not (nil? x) #{x}))})


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

(def mean
  {:name :mean
   :monoid-fun (fn [x _] (when x (Mean. x 1)))})

(def count-binned
  {:name :count-binned
   :monoid-fun (fn [o _] (if (nil? o) (sorted-map) (sorted-map o (m-sum 1))))})

(def count-unique (post :count-unique clj/count set))

(defn count-binned-normalized [result-name count-name bin-name]
  {:requires [count-name bin-name]
   :name result-name
   :processor (fn [m] (when-let [c (count-name m)]
                       (assoc m result-name (fmap #(/ (double %) c) (bin-name m)))))})

(defn rate
  "Calculate the rate at which entities with a value occur in the
   input stream, using ts-extractor to find times of occurrences in
   the entities. If there are fewer than two entities, the rate will
   be nil. Assumes that the input stream is properly ordered.


   Ex.:

   > (require '[babbage.provided.core :as p])
   > (def fields {:x (stats :x p/count (p/rate :ts))
                  :y (stats :y p/count (p/rate :ts))})
   > (calculate fields [{:y 1 :x 1 :ts 10} {:x 2 :y 1 :ts 11} {:y 1 :ts 12} {:x 1 :ts 13}])
   {:all {:x {:count 3, :rate 1.5}, :y {:count 3, :rate 1.0}}}"

  
  [ts-extractor]
  (post :rate (fn [m] (when (> (:count m) 1)
                       (/ (- (:last m) (:first m)) (double (dec (:count m))))))
        (consolidate (by ts-extractor last first) count)))

(def count-binned-normalized* (count-binned-normalized :count-binned-normalized
                                                       :count
                                                       :count-binned))

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
    {:name :histogram
     :monoid-fun (ignore-index h)}))

(def gaussian {:name :gaussian
               :monoid-fun (fn [& [v _]]
                             (when v (Gaussian. 1 (double v) 0)))})
