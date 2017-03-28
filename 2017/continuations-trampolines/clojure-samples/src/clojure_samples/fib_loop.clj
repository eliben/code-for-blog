;;; Iterative fibonacci.
;;;
;;; Eli Bendersky [http://eli.thegreenplace.net]
;;; This code is in the public domain.
(ns clojure-samples.fib-loop)

(defn fib_iterative
  [n]
  (loop [n n
         accum1 1
         accum2 1]
    (if (< n 2)
      accum1
      (recur (- n 1) (+ accum1 accum2) accum1))))

(fib_tail 10)
