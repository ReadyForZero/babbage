(ns babbage.test.functor
  (:use [expectations]
        [babbage.functor])
  (:import babbage.functor.Pure))

(expect {:x 6} (in-maps + 3 0 {:x 1} {:x 2} {:x 3}))

(expect {:x {:y 6 :z 15}}
        (in-maps (in-maps + 3 0) 3 0
                 {:x {:y 1 :z 6}}
                 {:x {:y 2 :z 4}}
                 {:x {:y 3 :z 5}}))

(expect {:x {:z 15 :y 6}}
        (in-maps (in-maps + 5 0) 5 (new Pure 0)
                 {:x {:y 1 :z 6}}
                 {:x {:y 2 :z 4}}
                 {:x nil}
                 {:x {:y 0 :z nil}}
                 {:x {:y 3 :z 5}}))

(expect {:x {:z {:y 15}} :y {:x {:z 15}}}
        (in-nested-maps 3 + 5 0
                        {:x {:z {:y 1}} :y nil}
                        nil
                        {:x {:z {:y 10}} :y {:x {:z 5}}}
                        {:x {:z nil} :y {:x {:z 9}}}
                        {:x {:z {:y 4}} :y {:x {:z 1}}}))

;; wrong arity
(expect AssertionError (in-maps + 2 0 {:x 1}))
(expect AssertionError (in-maps + 2 0 {:x 1} {:x 2} {:x 3}))

;; all values nil --> nil result.
(expect {:x {:z {:y 10}} :y {:x nil}}
        (in-nested-maps 3 +
                        2 0
                        {:x {:z {:y 5}} :y {:x nil}}
                        {:x {:z {:y 5}} :y {:x nil}}))

(expect {:x {:z {:y 10}} :y nil}
        (in-nested-maps 3 +
                        2 0
                        {:x {:z {:y 5}} :y nil}
                        {:x {:z {:y 5}}}))

(expect {:x {:z {:y 10}}}
        (in-nested-maps 3 +
                        2 0
                        {:x {:z {:y 5}}}
                        {:x {:z {:y 5}}}))
