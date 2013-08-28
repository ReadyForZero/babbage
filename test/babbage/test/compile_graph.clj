(ns babbage.test.compile-graph
  (:use babbage.graph
        expectations))


(defgraphfn foo [a bar]
  (+ a bar))

(defgraphfn bar [baz]
  (inc baz))

(defgraphfn baz [foo b]
  (* foo b))

(defgraphfn quux [foo bar]
  [foo bar])

(expect Exception (run-graph {:a 1 :b 2} foo bar baz))

(expect {:foo 5
         :bar 4
         :baz 3
         :b 2
         :a 1} (run-graph {:a 1 :b 2 :baz 3} foo bar))

(expect Exception (compile-graph foo bar baz))

(def foobar (compile-graph foo bar))

;; arguments in alpha order, a then baz
;; results in alpha order, bar then foo
(expect [3 4] (foobar 1 2))

(expect {:foo 5
         :bar 4
         :baz 3
         :b 2
         :a 1} (run-graph {:a 1 :b 2 :baz 3} foobar))

(expect Exception (run-graph {:a 1 :b 2} foobar baz))

(expect {:quux [4 3]
         :bar 3
         :baz 2
         :a 1
         :foo 4} (run-graph {:a 1 :baz 2} foobar quux))

(expect {:quux [4 3]
         :bar 3
         :baz 2
         :a 1
         :foo 4} (run-graph {:a 1 :baz 2} foo bar quux))
