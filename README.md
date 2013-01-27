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

## Multiple stats

Simply compute multiple measures in one pass.

```clojure

;; Simplest possible example, compute the sum of a seq.
#> (->> [{:x 1} {:x 2}] 
     (calculate {:the-result (stats :x sum)}))
{:all {:the-result {:sum 3}}} ;; :all refers to the result computed over all elements, not a subset.

;; Compute multiple measures over the same field in one pass.
#> (->> [{:x 1} {:x 2}] 
     (calculate {:the-result (stats :x sum mean)})) ;; <-- Here we've added the mean as well.
{:all {:the-result {:mean 1.5, :count 2, :sum 3}}} ;; The 'count' measure is required by 'mean', 
                                                   ;; so it's automatically computed.

;; Compute multiple measures over multiple fields in one pass.
#> (->> [{:x 1 :y 10} {:x 2} {:x 3} {:y 15}]
     (calculate {:x (stats :x sum mean) 
                 :y (stats :y mean)})) ;; <-- Here we're computing mean over :y also.
{:all {:x {:count 3, :mean 2.0, :sum 6},
       :y {:count 2, :mean 12.5, :sum 25}}}

;; Provide your own extraction functions.
#> (->> [{:x 1 :y 10} {:x 2} {:x 3} {:y 15}] 
     (calculate 
       {:x (stats :x sum mean) 
        :y (stats :y mean) 
        ;; Now accumulate the mean not of a field, but of a function we provide.
        :both (stats #(+ (or (:x %) 0) (or (:y %) 0)) mean)})) 
{:all {:x {:mean 2.0, :count 3, :sum 6}, 
       :y {:mean 12.5, :sum 25, :count 2}, 
       :both {:mean 7.75, :sum 31, :count 4}}}
```

## Multiple subsets

Simply compute the same measures across multiple subsets. Still in one pass.

```clojure

;; We take the previous example, and compute the same measures, but 
;; considering different subsets of elements.
#> (->> [{:x 1 :y 10} {:x 2} {:x 3} {:y 15}] 
     (calculate 
       (sets {:has-y #(-> % :y)}) ;; <-- Compute measures over just those 
                                  ;; elements that have y (in addition 
                                  ;; to all elements).
         {:x (stats :x sum mean) 
          :y (stats :y mean) 
          :both (stats #(+ (or (:x %) 0) (or (:y %) 0)) mean)}))

{:all   {:x {:mean 2.0, :count 3, :sum 6}, 
         :y {:mean 12.5, :sum 25, :count 2}, 
         :both {:mean 7.75, :sum 31, :count 4}}, 
 :has-y {:x {:mean 1.0, :count 1, :sum 1}, 
         :y {:mean 12.5, :sum 25, :count 2}, 
         :both {:mean 13.0, :sum 26, :count 2}}}

```

## License

Copyright (c) ReadyForZero, Inc. All rights reserved.

The use and distribution terms for this software are covered by the Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)

## Contributors

[Ben Wolfson](http://github.com/bwo) (wolfson@readyforzero.com)