(ns babbage.provided.histogram
  (:use [babbage.monoid :only [mk-monoid]]))

(defn- interval-< [i1 i2]
  (< (second i1) (first i2)))

(defn- copy-meta [o1 o2]
  (with-meta o2 (meta o1)))

(defn- m-add-histogram
  "The add function for the histogram monoid."
  [h1 h2]
  ;; there is a gap if the upper bound of h1 is less than the lower
  ;; bound of h2, or the upper bound of h2 is less than the lower
  ;; bound of h1.
  (let [width (-> h1 meta :width)
        h2width (-> h2 meta :width)]
    (assert (= width h2width) "cannot add histograms with unequal box widths")
    (if (interval-< (apply max-key second (keys h2)) (apply min-key first (keys h1)))
      (recur h2 h1)
      (copy-meta
       h1
       (merge-with + h1
                   (if (interval-< (apply max-key second (keys h1)) (apply min-key first (keys h2)))
                     (let [target (apply clojure.core/max (map second (keys h1)))
                           start (apply clojure.core/min (map first (keys h2)))]
                       (loop [h2 h2 last start]
                         (if (= last target)
                           h2
                           (recur (assoc h2 [(- last width) last] 0) (- last width)))))
                     h2))))))


(defn m-histogram
  "Given a width, returns a histogram monoid."
  [width]
  (let [precision (when (< width 1.0) (-> width str count (- 2)))
        multiplier (when precision (Math/pow 10 precision))]
    (letfn [(cut [n] (if (nil? precision) n
                         (-> n (* multiplier) int (/ multiplier))))]
      (fn [& [my-val]]
        (let [my-interval (when my-val (cut (- my-val (mod my-val width))))]
          (mk-monoid m-add-histogram
                     (with-meta
                       (if my-val
                         (sorted-map [(cut my-interval) (cut (+ my-interval width))] 1)
                         (sorted-map))
                       {:width width})
                     (sorted-map)))))))