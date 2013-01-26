(ns babbage.test.core
  (:refer-clojure :exclude [count set max list min complement])
  (:use [expectations]
        [babbage.core]))

(def values [{:v 1 :x 10 :low? true :status :good}
             {:v 4 :x 43 :low? false :status :good}
             {:v 11 :x 25 :low? false :status :bad}
             {:v 12 :x 53 :low? true :status :good}
             {:v 16 :x 35 :low? true :status :bad}
             {:v 20 :x 22 :low? false :status :good}])

(def v-mean (/ (double (apply + (map :v values))) (clojure.core/count values)))

(def v-extrema (stats :v min max mean))

(def status-sets {:is-good #(= (:status %) :good)
                  :is-bad #(= (:status %) :bad)})

(def low-sets {:low :low?
               :high (clojure.core/complement :low?)})

;; non-nested, no subsets.
(let [r (:all (calculate (sets) v-extrema values))]
  (expect v-mean (:mean r))
  (expect 1 (:min r))
  (expect 20 (:max r)))

;; count-binned
(let [r (:all (calculate (sets) (stats :status count-binned) values))]
  (expect {:good 4 :bad 2} (:count-binned r)))

;; map using new keys
(let [r (:all (calculate (sets) (stats :x (map-with-key :status :status-to-x mean max min)) values))]
  (expect 10 (-> r :status-to-x :good :min))
  (expect 30.0 (-> r :status-to-x :bad :mean)))

;; map using new values (same as above!):
(let [r (:all (calculate (sets) (stats :status (map-with-value :x :status-to-x mean max min)) values))]
  (expect 10 (-> r :status-to-x :good :min))
  (expect 30.0 (-> r :status-to-x :bad :mean)))

(def fields {:v v-extrema
             :x (stats :x mean (histogram 10) count (ratio :max :mean) max)})

(let [r (calculate (sets status-sets) fields values)
      with-subsets (calculate (-> status-sets
                                  sets
                                  (intersect :is-good :is-bad)                                  
                                  (union :is-good :is-bad)) fields values)]
  (expect {[10 20] 1
           [20 30] 2
           [30 40] 1
           [40 50] 1
           [50 60] 1} (-> r :all :x :histogram))

  (expect {[20 30] 1
           [30 40] 1} (-> r :is-bad :x :histogram))
  
  (expect (/ 7 (double 6)) (-> r :is-bad :x :max-to-mean))

  (expect 1 (-> r :is-good :v :min))

  ;; mutually exclusive
  (expect nil (:Sis-good-and-is-bad> with-subsets))

  ;; exhaustive
  (expect (:all with-subsets) (:Sis-good-or-is-badZ with-subsets)))

(let [r (calculate (-> (merge status-sets low-sets)
                       sets
                       (intersections [:low :high]
                                      [:is-good :is-bad]))
                   fields
                   values)]

  (expect 16 (-> r :Slow-and-is-badZ :v :max))
  (expect 1 (-> r :Slow-and-is-goodZ :v :min))
  (expect 11 (-> r :Shigh-and-is-badZ :v :min))

  (expect 1 (-> r :Shigh-and-is-badZ :x :count))
  )


;; Test that histogram can handle nil input.
(let [input [{:x 1} {:x 5} {:x 10}]]
  (expect {:all {:x {:histogram {}}}}
          (calculate
           (sets)
           ;; This stat represents an extraction function that always returns nil.
           {:x (stats (fn [_] nil) (histogram 2))}
           input)))
