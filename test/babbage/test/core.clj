(ns babbage.test.core
  (:refer-clojure :exclude [count set max list min complement])
  (:use [expectations]
        babbage.core
        babbage.provided.core))

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

(let [r (:all (calculate (stats :v count (by #(when (= :good (:status %)) %) :good-status count)) values))]
  (expect 6 (:count r))
  (expect 4 (-> r :good-status :count)))

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
  (expect nil (:Sis-good-and-is-badZ with-subsets))

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


;;;;;;;;;;;;; examples from readme are accurate!

(expect {:all {:the-result {:mean 1.5,
                            :count 2,
                            :sum 3}}}
        (calculate {:the-result (stats :x sum mean)}
                   [{:x 1} {:x 2}]))

(expect {:all {:x-result {:count 3, :mean 2.0,  :sum 6},
               :y-result {:count 2, :mean 12.5, :sum 25}}}
        (calculate
         {:x-result (stats :x sum mean)
          :y-result (stats :y mean)}
         [{:x 1 :y 10} {:x 2} {:x 3} {:y 15}]))

(expect {:all {:both {:count 4, :mean 7.75, :sum 31}}}
        (calculate
        {:both (stats #(+ (or (:x %) 0)
                          (or (:y %) 0))
                      mean)}
        [{:x 1 :y 10} {:x 2} {:x 3} {:y 15}]))

(expect {:all {:mean 16.6,
               :users {:unique 3, :count-binned {1 3, 3 1, 4 1}},
               :sum 83,
               :count 5}}
        (calculate
         (stats :sale mean sum (by :user_id :users count-unique))
         [{:sale 10 :user_id 1} {:sale 20 :user_id 4}
          {:sale 15 :user_id 1} {:sale 13 :user_id 3}
          {:sale 25 :user_id 1}]))

(expect {:all {:mean 16.6,
               :user->sales {3 {:mean 13.0, :count 1, :sum 13},
                             4 {:mean 20.0, :count 1, :sum 20},
                             1 {:mean (/ 50 3.0), :count 3, :sum 50}},
               :sum 83,
               :count 5}} (calculate
                           (stats :sale mean sum (map-with-key :user_id :user->sales mean sum))
                           [{:sale 10 :user_id 1} {:sale 20 :user_id 4}
                            {:sale 15 :user_id 1} {:sale 13 :user_id 3}
                            {:sale 25 :user_id 1}]))


(expect {:all   {:both {:mean 7.75, :sum 31, :count 4}},
         :has-y {:both {:mean 13.0, :sum 26, :count 2}}}
        (calculate
         (sets {:has-y :y})
         {:both (stats #(+ (or (:x %) 0)
                           (or (:y %) 0))
                       mean)}
         [{:x 1 :y 10} {:x 2} {:x 3} {:y 15}]))

(def my-sets (-> (sets {:has-y :y
                              :good :good?})
                       (complement :good)
                       (intersections :has-y
                         [:good :not-good])))

(def my-fields {:both (stats #(+ (or (:x %) 0) (or (:y %) 0)) mean)})

(expect {:Shas-y-and-not-goodZ {:both {:mean 16.0, :sum 16, :count 1}},
         :Shas-y-and-goodZ     {:both {:mean 5.0,  :sum 5,  :count 1}},
         :good                 {:both {:mean 6.0,  :sum 12, :count 2}},
         :has-y                {:both {:mean 10.5, :sum 21, :count 2}},
         :all                  {:both {:mean 8.0,  :sum 32, :count 4}},
         :not-good             {:both {:mean 10.0, :sum 20, :count 2}}}
        (calculate my-sets my-fields [{:x 1 :good? true :y 4}
                                      {:x 4 :good? false}
                                      {:x 7 :good? true}
                                      {:x 10 :good? false :y 6}]))
