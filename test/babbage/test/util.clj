(ns babbage.test.util
  (:use babbage.util
        expectations))

;; requires something not provided
(expect Exception (layers [{:requires '(a) :provides 'b}]))

;; circular
(expect Exception (layers [{:requires '(b) :provides 'a}
                           {:requires '(a) :provides 'c}
                           {:requires '(a) :provides 'b}]))

;; map set because order doesn't matter
(expect (map set [[{:requires nil :provides 'a}]
                  [{:requires '(a) :provides 'b}
                   {:requires '(a) :provides 'c}]
                  [{:requires '(a c) :provides 'd}]])
        (map set
             (layers [{:requires '(a c) :provides 'd}
                      {:requires '(a) :provides 'c}
                      {:requires nil :provides 'a}
                      {:requires '(a) :provides 'b}])))

(expect (map set
             [[{:requires nil :provides :xs}]
              [{:requires [:xs] :provides :count}
               {:requires [:xs] :provides :sum}
               {:requires [:xs] :provides :sum-squared}]
              [{:requires [:count :sum] :provides :mean}
               {:requires [:count :sum-squared] :provides :mean2}]
              [{:requires [:mean :mean2] :provides :variance}]])
        (map set
             (layers [{:requires nil :provides :xs}
                      {:requires [:xs] :provides :count}
                      {:requires [:mean :mean2] :provides :variance}
                      {:requires [:xs] :provides :sum}
                      {:requires [:count :sum] :provides :mean}
                      {:requires [:xs] :provides :sum-squared}
                      {:requires [:count :sum-squared] :provides :mean2}])))
