babbage
=======

Intelligent accumulation of statistics over a seq

Makes orthogonal:
1) Determining input to measurement computation
2) Specifying the measure
3) Computing the measure across multiple subsets

## Usage

```clojure
[babbage "0.1.0"]

;; In your ns statement:

(ns my.ns 
    (:use babbage.core))
```

The core interface is provided by the functions `stats`, `sets`, and
`calculate`. 

`stats` is used to declare the measures to calculate.

`sets` is used to declare the subsets of the input over which the
measures should be calculated.

`calculate` is used to actually make the thing go: it takes an
optional set specification as returned by `sets`, a measure
specification as returned by `stats` (or a map whose values are such
specifications), and a seq of inputs to calculate measures over.

## Multiple stats

The `stats` function takes an *extraction function* as its first
argument, and an arbitrary number of *measure functions* as its
remaining arguments.

In the simplest possible case, the extraction function is identity:

```clojure
user> (calculate (stats identity sum) [1 2 3 4])
{:all {:sum 10}} ;; :all refers to the result computed over all elements, not a subset
```

Frequently we will want to both name the result, and perform some kind
of operation on the elements of the input:

```clojure
user> (calculate {:the-result (stats :x sum)} [{:x 1} {:x 2}])
{:all {:the-result {:sum 3}}} 
```

Multiple measures can be computed in one pass:

```clojure
user> (->> [{:x 1} {:x 2}] 
     (calculate {:the-result (stats :x sum mean)})) ;; <-- Here we've added the mean as well.
{:all {:the-result {:mean 1.5, :count 2, :sum 3}}} ;; The 'count' measure is required by 'mean', 
                                                   ;; so it's automatically computed.
```

And we can compute multiple measures over multiple fields in one pass:

```clojure
;; Compute multiple measures over multiple fields in one pass.
user> (->> [{:x 1 :y 10} {:x 2} {:x 3} {:y 15}]
     (calculate {:x-result (stats :x sum mean) ;;
                 :y-result (stats :y mean)})) ;; <-- Here we're computing mean over :y also.
{:all {:x-result {:count 3, :mean 2.0, :sum 6},
       :y-result {:count 2, :mean 12.5, :sum 25}}}
```

"Extraction" functions can really perform arbitrary operations on
their inputs:

```clojure
user> (->> [{:x 1 :y 10} {:x 2} {:x 3} {:y 15}] 
     (calculate 
       {:x-result (stats :x sum mean) 
        :y-result (stats :y mean) 
        ;; Now accumulate the mean not of a field, but of a function we provide.
        :both (stats #(+ (or (:x %) 0) (or (:y %) 0)) mean)})) 
{:all {:x-result {:mean 2.0, :count 3, :sum 6},
       :y-result {:mean 12.5, :sum 25, :count 2},
       :both {:mean 7.75, :sum 31, :count 4}}}
```

Extraction functions will only be called once per item in the input seq.

## Multiple subsets

Simply compute the same measures across multiple subsets, in one pass.

`sets` takes a single argument, a map whose keys are names of sets and
whose values are predicates indicating whether a member of the input
seq belongs in the set:

```clojure
;; We take the previous example, and compute the same measures, but 
;; considering different subsets of elements.
user> (->> [{:x 1 :y 10} {:x 2} {:x 3} {:y 15}] 
     (calculate 
       (sets {:has-y #(-> % :y)}) ;; <-- Compute measures over just those 
                                  ;; elements that have y (in addition 
                                  ;; to all elements).
         {:x-result (stats :x sum mean) 
          :y-result (stats :y mean) 
          :both (stats #(+ (or (:x %) 0) (or (:y %) 0)) mean)}))
{:all   {:x-result {:mean 2.0, :count 3, :sum 6}, 
         :y-result {:mean 12.5, :sum 25, :count 2}, 
         :both {:mean 7.75, :sum 31, :count 4}}, 
 :has-y {:x-result {:mean 1.0, :count 1, :sum 1}, 
         :y-result {:mean 12.5, :sum 25, :count 2}, 
         :both {:mean 13.0, :sum 26, :count 2}}}
```

Given an initial predicate map, arbitrary complements, intersections,
and unions can be taken:

```clojure
;; More complex set operations:
user> (def my-sets (-> (sets {:has-y :y
                           :good :good?})
                    ;; add a set called :not-good
                    (complement :good)
                    ;; calculate the intersections of :has-y and :good, and :has-y and :not-good.
                    (intersections :has-y
                                   [:good :not-good])))                                   
#'user/my-sets
user> (def my-fields {:x-result (stats :x sum mean)
                      :y-result (stats :y mean)
                      :both (stats #(+ (or (:x %) 0) (or (:y %) 0)) mean)})
#'user/my-fields
user> (calculate my-sets my-fields [{:x 1 :good? true :y 4}
                                    {:x 4 :good? false}
                                    {:x 7 :good? true}
                                    {:x 10 :good? false :y 6}])
{:Shas-y-and-not-goodZ {:x-result {:mean 10.0, :count 1, :sum 10},
                        :y-result {:mean 6.0, :sum 6, :count 1},
                        :both {:mean 16.0, :sum 16, :count 1}},
 :Shas-y-and-goodZ     {:x-result {:mean 1.0, :count 1, :sum 1},
                        :y-result {:mean 4.0, :sum 4, :count 1},
                        :both {:mean 5.0, :sum 5, :count 1}},
 :good                 {:x-result {:mean 4.0, :count 2, :sum 8},
                        :y-result {:mean 4.0, :sum 4, :count 1},
                        :both {:mean 6.0, :sum 12, :count 2}},
 :has-y                {:x-result {:mean 5.5, :count 2, :sum 11},
                        :y-result {:mean 5.0, :sum 10, :count 2},
                        :both {:mean 10.5, :sum 21, :count 2}},
 :all                  {:x-result {:mean 5.5, :count 4, :sum 22},
                        :y-result {:mean 5.0, :sum 10, :count 2},
                        :both {:mean 8.0, :sum 32, :count 4}},
 :not-good             {:x-result {:mean 7.0, :count 2, :sum 14},
                        :y-result {:mean 6.0, :sum 6, :count 1},
                        :both {:mean 10.0, :sum 20, :count 2}}}
```

Predicate functions will only be called once per item in the input seq.

## Measure functions

(this section & subsections necessary?)

### Predefined measure functions 

TODO describe the existing ones

### Defining new measure functions

A measure function is built out of an underlying monoid, or a function
that depends on a measure function, and a public function that
associates the measure with a descriptive name into a map.

For simple functions the `defstatfn` macro in `babbage.core` and the
`monoid` function in `babbage.monoid` can be used to reduce
boilerplate. For instance, the following would record the first
non-nil value in the input sequence:

```clojure
user> (def m-fst (monoid (fn [a b] a) nil))
#'user/m-fst
user> (defstatfn fst m-fst)
#'user/fst
user> (calculate (stats :x fst sum) [{:x nil} {:x 2} {:x 3} {:x 4}])
{:all {:sum 9, :fst 2}}
```

## Efficient computation of inputs

When calculating nontrivial measures over real data, we will often
want to do some transformation of the data to make it tractable, or at
least to minimize verbosity and redundancy in the extraction or
set-definition functions. `babbage.graph` contains tools for
declaring dependency relations among computations and running such
computations efficiently, in the spirit of the
[Prismatic Graph library](http://blog.getprismatic.com/blog/2012/10/1/prismatics-graph-at-strange-loop.html)
and the [Flow library](https://github.com/stuartsierra/flow):

```clojure
user> (defgraphfn sum [xs]
        (apply + xs))
#'user/sum
user> (defgraphfn sum-squared [xs]
        (sum (map #(* % %) xs)))
#'user/sum-squared
user> (defgraphfn count-input :count [xs]
        (count xs))
#'user/count
user> (defgraphfn mean [count sum]
        (double (/ sum count)))
#'user/mean
user> (defgraphfn mean2 [count sum-squared]
        (double (/ sum-squared count)))
#'user/mean2
user> (defgraphfn variance [mean mean2]
        (- mean2 (* mean mean)))
#'user/variance
user> (run-graph {:xs [1 2 3 4]} sum variance sum-squared count-input mean mean2)
{:sum 10
 :count 4
 :sum-squared 30
 :mean 2.5
 :variance 1.25
 :mean2 7.5
 :xs [1 2 3 4]}
```

Functions defined with `defgraphfn` can be invoked like ordinary
Clojure functions defined with `defn` or `fn`, because that's what
they are; the only difference is that the functions carry some
metadata about their dependencies and their names around with them.

When called with `run-graph` (or its variations), the graph function
`mean` can refer to the results of the graph function `sum` simply by
including `sum` in its argument list.

Since (a) we might want to choose dynamically between multiple
functions that can provide the same output to further functions, and
(b) a name that describes a function's output might be undesirable for
the name of the function itself (because it might shadow a core
function, for instance), we can explicitly declare what name a graph
function's output should have when other graph functions want to refer
to it, as in `count-input` in the example above.

`run-graph` takes a map of initial input names and values and any
number of graph functions. It analyzes the dependency graph, throwing
an error in case of unavailable or circular dependencies, and
otherwise runs the functions provided with the input provided. By
default it runs its argument functions in parallel when possible, and
evaluates the results strictly. `run-graph-strategy` can be used to
force sequential evaluation, or lazy evaluation. `compile-graph` and
`compile-graph-strategy` can be used to analyze the dependency graph
once at run time, returning a function that can be called directly.
`run-graph*` and `run-graph-strategy*` can be used to attempt to
analyze the dependency graph at compile time, falling back to their
un-asterisked analogues if that is not possible.

## License

Copyright (c) ReadyForZero, Inc. All rights reserved.

The use and distribution terms for this software are covered by the Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)

## Contributors

[Ben Wolfson](http://github.com/bwo) (wolfson@readyforzero.com)
