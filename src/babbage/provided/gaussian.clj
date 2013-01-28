(ns babbage.provided.gaussian
  (:require babbage.monoid))

;; following http://izbicki.me/blog/gausian-distributions-are-monoids
(defrecord Gaussian [n m m2]
  Monoid
  (<> [self other]
    (if (mempty? other)
      self
      (let [{on :n om :m om2 :m2} other
            n' (+ n on)
            m' (/ (+ (* n m) (* on om)) n')
            m2' (- (+ m2 (* n (Math/pow m 2)) om2 (* on (Math/pow om 2)))
                   (* n' (Math/pow m' 2)))]
        (Gaussian. n' m' m2'))))
  (mempty [self] nil)
  ;; a Gaussian record created with the gaussian fn always has a count
  ;; of at least 1, so is never empty.
  (mempty? [self] false)
  (value [self] {:count n :mean m
                 :variance (if (not= 1 n) (/ m2 (- n 1)) 0)}))

(defstatfn gaussian
  (fn [& [v]]
    (when v (Gaussian. 1 (double v) 0))))