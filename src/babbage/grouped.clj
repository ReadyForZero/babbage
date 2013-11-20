(ns babbage.grouped
  (:require [babbage.monoid :as m :refer [<>]]))

(deftype Group [before grouped after startindex runlength started]
  m/Monoid
  (<> [self other]
    (if (nil? other) self
        (let [^Group other other
              obefore (.before other)
              orun (.runlength other)
              ostarted (.started other)]
          (assert (== orun runlength))
          (cond
           (and started ostarted) (let [ab (+ (count after) (count obefore))]
                                    (if (zero? ab)
                                      (Group. before
                                              (into [] (concat grouped (.grouped other)))
                                              (.after other)
                                              startindex
                                              runlength
                                              true)
                                      (do (assert (== runlength ab))
                                          (Group. before
                                                  (into [] (concat
                                                            (conj grouped
                                                                  (reduce <> nil (concat after obefore)))
                                                            (.grouped other)))
                                                  (.after other)
                                                  startindex
                                                  runlength
                                                  true))))
           started (let [new-after (into [] (concat after obefore))]
                     (if (>= (count new-after) runlength)
                       (Group. before
                               (conj grouped (reduce <> (take runlength new-after)))
                               (drop runlength new-after)
                               startindex
                               runlength
                               true)
                       (Group. before grouped new-after startindex runlength true)))
           ostarted (let [new-before (into [] (concat before obefore))
                          c (count new-before)]
                      (if (>= c runlength)
                        (Group. (take (- c runlength) new-before)
                                (into [] (cons (reduce <> (drop (- c runlength) new-before)) (.grouped other)))
                                (.after other)
                                startindex
                                runlength
                                true)
                        (Group. new-before (.grouped other) (.after other) startindex runlength true)))
           :otherwise (let [group (concat before obefore)
                            c (count group)
                            [pfx group-cands] (split-with #(not (zero? (rem % runlength)))
                                                          (take c (iterate inc startindex)))
                            pfxc (count pfx)]
                        (if (>= (count group-cands) runlength)
                          (Group. (take pfxc group)
                                  [(reduce <> (take runlength (drop pfxc group)))]
                                  (drop (+ runlength pfxc) group)
                                  startindex
                                  runlength
                                  true)
                          (Group. group nil nil startindex runlength false)))))))
  (value [_] (map m/value (let [b (if (seq before) [(reduce <> before)] ())
                                a (if (seq after) [(reduce <> after)] ())]
                            (concat b grouped a))))
  (mempty [_] (Group. nil nil nil 0 runlength false))
  (mempty? [_] (and (nil? before) (nil? after) (nil? grouped))))

(defn group [m num index]
  (Group. [m] nil nil index num false))
