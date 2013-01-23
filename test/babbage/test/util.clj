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

(defn mk-node [[requires provides]]
  {:requires requires :provides provides})

(let [items (map mk-node [[[:periods :bpu] :bps]
                          [[:p] :bpu]
                          [[] :periods]
                          [[:periods] :p]])
      depmap (zipmap (map :provides items) items)]
  (expect false (boolean (circular? items depmap))))

(let [items (map mk-node [[[:a :b] :c]
                          [[:c :d] :e]
                          [[] :a]
                          [[:a :d] :b]
                          [[:a] :d]])
      _ (println items)
      depmap (zipmap (map :provides items) items)]
  (expect false (boolean (circular? items depmap))))


(let [items (map mk-node [[[:a :b] :c]
                          [[:c :d] :e]
                          [[] :a]
                          [[:a :d] :b]
                          [[:c] :d]])
      depmap (zipmap (map :provides items) items)]
  (expect true (boolean (circular? items depmap))))
