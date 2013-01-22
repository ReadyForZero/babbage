(ns babbage.test.monoid
  (:refer-clojure :exclude [max min])
  (:use expectations
        babbage.monoid))

(expect 10 (value (reduce <> (map max [4 1 10]))))

(expect (/ -1 0.0) (value (max)))

(expect 10 (value (reduce <> (map sum [1 2 3 4]))))

(expect 24 (value (reduce <> (map prod [1 2 3 4]))))

(expect 4 (value (reduce <> (map snd [1 2 3 4]))))

(expect 4 (value (reduce <> (map fst [4 3 2 1]))))

(expect [1 2 3 4] (value (reduce <> (map vector [1 2 3 4]))))

(let [h (histogram 10)
      h2 (histogram 15)]
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
        (value (<> {:sum (sum 2)
                    :min (min 2)
                    :all [2]
                    :max (max 2)}
                   {:sum (sum 10)
                    :min (min 10)
                    :all [10]
                    :max (max 10)})))

;; nil is always an identity on left or right
(expect true (every? (partial = 4)
                     (mapcat (fn [f] [(value (<> (f 4) nil))
                                     (value (<> nil (f 4)))])
                             [max min sum prod snd fst])))


(def empty-things [[] '() #{} nil {}
                              (sum 0) (sum)
                              (max) (min)
                              (snd) (fst)
                              ((histogram 10))
                              (prod 1) (prod)])
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
       (sum 0) (sum 10)
       (max) (max 10)
       (min) (min 10)
       (snd) (snd 10)
       (fst) (fst 10)
       ((histogram 10)) ((histogram 10) 11)
       (prod) (prod 10))

;; Found this online.
(let [r (babbage.dependent/pearson-compute [44 32.17 30.56]
                                           [[22 13 13] [23 27 27] [26 19 19]
                                            [26 8 8] [37 22 22] [39 28 28]
                                            [40 30 30] [44 34 34] [49 31 31]
                                            [50 25 25] [51 36 36] [52 41 41]
                                            [52 43 43] [53 36 36] [55 46 46]
                                            [56 44 44] [57 49 49] [60 47 18]])]
  (expect 0.7055068524919457 (-> r :r-0-2))
  (expect 0.8792153149778114 (-> r :r-0-1))
  (expect 0.831764676098623 (-> r :r-1-2)))
