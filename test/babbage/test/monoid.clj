(ns babbage.test.monoid
  (:refer-clojure :exclude [max min])
  (:require [babbage.provided.core :as b]
            [babbage.provided.histogram :as h])
  (:use expectations
        babbage.monoid))

;; I think most of these tests should become tests of the stat.
(expect 10 (value (reduce <> (map b/m-max [4 1 10]))))

(expect (/ -1 0.0) (value (b/m-max)))

(expect 10 (value (reduce <> (map b/m-sum [1 2 3 4]))))

(expect 24 (value (reduce <> (map b/m-prod [1 2 3 4]))))

(expect [1 2 3 4] (value (reduce <> (map vector [1 2 3 4]))))

(let [h (h/m-histogram 10)
      h2 (h/m-histogram 15)]
  (expect {} (value (h)))
  (expect {[10 20] 1
           [20 30] 0
           [30 40] 0
           [40 50] 2
           [50 60] 1} (value (reduce <> (map h [40 12 41 55]))))
  
  (expect AssertionError (<> (h 11) (h2 43))))

(expect {:sum 12
         :min 2
         :all [2 10]
         :max 10}
        (value (<> {:sum (b/m-sum 2)
                    :min (b/m-min 2)
                    :all [2]
                    :max (b/m-max 2)}
                   {:sum (b/m-sum 10)
                    :min (b/m-min 10)
                    :all [10]
                    :max (b/m-max 10)})))

;; nil is always an identity on left or right
(expect true (every? (partial = 4)
                     (mapcat (fn [f] [(value (<> (f 4) nil))
                                     (value (<> nil (f 4)))])
                             [b/m-max b/m-min b/m-sum b/m-prod])))


(def empty-things [[] '() #{} nil {}
                              (b/m-sum 0) (b/m-sum)
                              (b/m-max) (b/m-min)
                              ((h/m-histogram 10))
                              (b/m-prod 1)
                              (b/m-prod)])
;; emptiness
(expect true (every? mempty? empty-things))
;; a map whose values are empty is also empty.
(expect true (mempty? (zipmap (repeatedly gensym) empty-things)))

(given [empty full]
       (expect [(value empty) (value full) (value full)]
               [(value (<> empty empty))
                (value (<> full empty))
                (value (<> empty full))])
       [] [1]
       '() '(1)
       (b/m-sum 0) (b/m-sum 10)
       (b/m-max) (b/m-max 10)
       (b/m-min) (b/m-min 10)
       ((h/m-histogram 10)) ((h/m-histogram 10) 11)
       (b/m-prod) (b/m-prod 10))
