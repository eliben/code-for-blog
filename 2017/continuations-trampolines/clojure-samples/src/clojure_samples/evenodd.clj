;;; Example of even/odd in Clojure; recursive and trampolined.
;;;
;;; Eli Bendersky [http://eli.thegreenplace.net]
;;; This code is in the public domain.
(ns clojure-samples.evenodd)

(declare is-even?)

(defn is-odd?
  [n]
  (if (= n 0)
    false
    (is-even? (- n 1))))

(defn is-even?
  [n]
  (if (= n 0)
    true
    (is-odd? (- n 1))))

(map is-even? '(1 2 3 4))

(declare is-even-thunked?)

(defn is-odd-thunked?
  [n]
  (if (= n 0)
    false
    #(is-even-thunked? (- n 1))))

(defn is-even-thunked?
  [n]
  (if (= n 0)
    true
    #(is-odd-thunked? (- n 1))))

(trampoline is-even-thunked? 2)
(trampoline is-even-thunked? 3)
