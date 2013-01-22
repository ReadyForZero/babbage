(ns babbage.test.graph
  (:use babbage.graph
        expectations))

;;;; the example from p 15 of the graph presentation
;;; support for separating function name from provided name
(defgraphfn count-xs :count [xs]
  (count xs))

(defgraphfn sum [xs]
  (apply + xs))

(defgraphfn sum-squared [xs]
  ;; can be used like an ordinary fn, too (another reason to avoid
  ;; punning on fn name...)
  (sum (map #(* % %) xs)))

(defgraphfn mean [count sum]
  (double (/ sum count)))

(defgraphfn mean2 [count sum-squared]
  (double (/ sum-squared count)))

(defgraphfn variance [mean mean2]
  (double (- mean2 (* mean mean))))

;; xs not provided; exception
(expect Exception (run-graph {} count-xs sum sum-squared mean mean2 variance))

(expect {:sum 10
         :count 4
         :sum-squared 30
         :mean 2.5
         :mean2 7.5
         :variance 1.25
         :xs [1 2 3 4]}
        (run-graph {:xs [1 2 3 4]} sum variance sum-squared count-xs mean mean2))

;; necessary intermediate fns not provided
(expect Exception (run-graph {:xs [1 2 3 4]} sum variance count-xs))
