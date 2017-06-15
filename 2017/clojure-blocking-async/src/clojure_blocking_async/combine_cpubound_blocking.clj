;;; Benchmark combining blocking with CPU-bound tasks.
;;;
;;; Eli Bendersky [http://eli.thegreenplace.net]
;;; This code is in the public domain.
(ns clojure-blocking-async.combine-cpubound-blocking
  (:require [clojure.core.async :as async]))

(defn factorize
  "Naive factorization function; takes an integer n and returns a vector of
  factors."
  [n]
  (if (< n 2)
    []
    (loop [factors []
           n n
           p 2]
      (cond (= n 1) factors
            (= 0 (mod n p)) (recur (conj factors p) (quot n p) p)
            (>= (* p p) n) (conj factors n)
            (> p 2) (recur factors n (+ p 2))
            :else (recur factors n (+ p 1))))))

(defn receive-n
  "Receive n items from the given channel and return them as a vector."
  [c n]
  (loop [i 0
         res []]
    (if (= i n)
      res
      (recur (inc i) (conj res (async/<!! c))))))

;;; This number takes ~230 msec to factorize.
(def mynum (* 1548587 982451653))

(defn launch-go-blocking-and-compute
  [nblock ncompute]
  (let [c (async/chan)]
    (dotimes [i nblock]
      (async/go
        (Thread/sleep 250)
        (async/>! c i)))
    (dotimes [i ncompute]
      (async/go
        (async/>! c (factorize mynum))))
    (receive-n c (+ nblock ncompute))))

(defn launch-thread-blocking-and-compute
  [nblock ncompute]
  (let [c (async/chan)]
    (dotimes [i nblock]
      (async/thread
        (Thread/sleep 250)
        (async/>!! c i)))
    (dotimes [i ncompute]
      (async/thread
        (async/>!! c (factorize mynum))))
    (receive-n c (+ nblock ncompute))))

;;; The higher nblock goes, the slower the go-blocks version becomes; the
;;; runtime of the threaded version is proportonal mainly to the number of
;;; computes going in parallel.
(def nblock 64)
(def ncompute 16)

(dotimes [_ 6]
  (println "go")
  (time (launch-go-blocking-and-compute nblock ncompute)))

(dotimes [_ 6]
  (println "thread")
  (time (launch-thread-blocking-and-compute nblock ncompute)))
