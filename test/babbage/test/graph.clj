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

(let [expanded (macroexpand '(run-graph* {:xs [1 2 3 4]} sum mean mean2 sum-squared count-xs))]
  (expect 'let* (first expanded)))

(let [expanded (macroexpand '(run-graph* {:xs [1 2 3 4]}
                                         sum mean mean2 sum-squared
                                         (when true count-xs)))]
  ;; can't expand into a let form because run-graph* is too stupid to
  ;; know that (when true ...) == ...
  (expect 'babbage.graph/run-graph-strategy (first expanded)))

(expect (run-graph {:xs [1 2 3 4]} sum mean sum-squared mean2 count-xs)
        (run-graph* {:xs [1 2 3 4]} sum mean sum-squared mean2 count-xs))

(def called (atom #{}))
(defn track-calls [graphfn]
  (with-meta
    (fn [& args]
      (swap! called conj (-> graphfn meta :provides))
      (apply graphfn args))
    (meta graphfn)))

(with-redefs [called (atom #{})]
  (let [fns (map track-calls [sum mean mean2 sum-squared count-xs])
        r (apply run-graph-strategy {:lazy? true} {:xs [1 2 3 4]} fns)]
    (expect #{} @called)
    (expect 2.5 (:mean r))
    (expect #{:mean :sum :count} @called)
    (expect 7.5 (:mean2 r))
    (expect #{:mean :sum :count :mean2 :sum-squared} @called)))


;; multiple arities are not allowed, also how else can you test this
;; in expectations?
(try
  (macroexpand '(defgraphfn foo ([a] a) ([a b] (+ a b))))
  (expect false)
  (catch AssertionError a (expect true)))
;; but each way of specifying a single arity is allowed:
(defgraphfn foo ([a] a))
(defgraphfn foo* [a] a)

;; :as-less top-level destructuring doesn't work
(try (macroexpand '(defgraphfn foo [{:keys [a b c]}]))
     (expect false)
     (catch AssertionError a (expect true)))
(try (macroexpand '(defgraphfn foo [[a b c]]))
     (expect false)
     (catch AssertionError a (expect true)))
;; :as-ful top-level destructuring does work, even with nested
;; :as-less destructuring
(defgraphfn bar [{:keys [a b c] :as d}] d)
(expect [:d] (-> bar meta :requires))
(defgraphfn bar* [[a {:keys [b c]} :as d]] d)
(expect [:d] (-> bar* meta :requires))
