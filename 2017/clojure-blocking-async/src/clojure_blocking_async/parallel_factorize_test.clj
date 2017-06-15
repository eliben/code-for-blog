;;; Tests for factorization code.
;;;
;;; Eli Bendersky [http://eli.thegreenplace.net]
;;; This code is in the public domain.
(ns clojure-blocking-async.parallel-factorize-test
  (:use clojure-blocking-async.parallel-factorize)
  (:use clojure.test))

(deftest test-single-factorize
  (is (= [3 5] (factorize 15))))

(def factorizers
  [serial-factorizer
   (fn [nums] (async-go-factorizer nums 4))
   (fn [nums] (async-thread-factorizer nums 4))
   (fn [nums] (async-with-pipeline nums 4))
   rfold
   ])

(def testpairs [
                [[15 7] {15 [3 5], 7 [7]}]
                [[4 11 25 30] {4 [2 2], 11 [11], 25 [5 5], 30 [2 3 5]}]
                [[982451653] {982451653 [982451653]}]
                ])

(deftest test-factorizers
  (doseq [factorizer factorizers
          [input result] testpairs]
    (is (= result (factorizer input)) (str factorizer))) )

(run-tests)
