(ns babbage.test.compile-graph
  (:use babbage.graph
        expectations))


(defgraphfn foo [a bar at]
  (swap! at conj :foo)
  (+ a bar))

(defgraphfn bar [baz at]
  (swap! at conj :bar)
  (inc baz))

(defgraphfn baz [foo b at]
  (swap! at conj :baz)
  (* foo b))

(defgraphfn quux [foo bar at]
  (swap! at conj :quux)
  [foo bar])

(expect Exception (run-graph {:a 1 :b 2 :at (atom [])} foo bar baz))

(expect {:foo 5
         :bar 4
         :baz 3
         :b 2
         :a 1} (dissoc (run-graph {:a 1 :b 2 :baz 3 :at (atom [])} foo bar) :at))

(expect Exception (compile-graph foo bar baz))

(def foobar (compile-graph foo bar))
(def foobar-l (compile-graph-strategy {:lazy? true} foo bar))

;; arguments in alpha order, a then at then baz
;; results in alpha order, bar then foo
(expect [3 4] (foobar 1 (atom []) 2))

(let [a (atom [])
      r (foobar-l 1 a 2)]
  (expect [] @a)
  (expect 3 (first r))
  (expect [:bar] @a)
  (expect 4 (second r))
  (expect [:bar :foo] @a))

(expect {:foo 5
         :bar 4
         :baz 3
         :b 2
         :a 1} (dissoc (run-graph {:a 1 :b 2 :baz 3 :at (atom [])} foobar) :at))

(expect Exception (run-graph {:a 1 :b 2} foobar baz))

(expect {:quux [4 3]
         :bar 3
         :baz 2
         :a 1
         :foo 4} (dissoc (run-graph {:a 1 :baz 2 :at (atom [])} foobar quux) :at))

(expect {:quux [4 3]
         :bar 3
         :baz 2
         :a 1
         :foo 4} (dissoc (run-graph {:a 1 :at (atom []) :baz 2} foo bar quux) :at))

(let [a (atom [])
      r (run-graph-strategy {:lazy? true} {:a 1 :at a :baz 2} foobar-l quux)]
  (expect [] @a)
  (expect 3 (:bar r))
  (expect [:bar] @a)
  (expect [4 3] (:quux r))
  (expect [:bar :foo :quux] @a))

(expect AssertionError (run-graph {:a 1 :baz 2 :at (atom [])} foobar-l foo))
