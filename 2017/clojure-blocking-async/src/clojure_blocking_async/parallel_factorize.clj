;;; Parallel factorization (CPU-intensive) using a few methods.
;;;
;;; Eli Bendersky [http://eli.thegreenplace.net]
;;; This code is in the public domain.
(ns clojure-blocking-async.parallel-factorize
  (:require [clojure.core.reducers :as r])
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

;;; Each 'factorizer' function takes a sequence of numbers and and returns a map
;;; of {n: [factors]}

(defn serial-factorizer
  "Simple serial factorizer."
  [nums]
  (zipmap nums (map factorize nums)))

(defn receive-n-maps
  "Receive n items from the given channel and merge them all into one map."
  [c n]
  (loop [i 0
         res {}]
    (if (= i n)
      res
      (recur (inc i) (conj res (async/<!! c))))))

(defn async-go-factorizer
  "Parallel factorizer for nums, launching n go blocks."
  [nums n]
  ;;; Push nums into an input channel; spin up n go-blocks to read from this
  ;;; channel and add numbers to an output channel.
  (let [in-c (async/chan)
        out-c (async/chan)]
    (async/onto-chan in-c nums)
    (dotimes [i n]
      (async/go-loop []
        (when-let [nextnum (async/<! in-c)]
          (async/>! out-c {nextnum (factorize nextnum)})
          (recur))))
    (receive-n-maps out-c (count nums))))

(defn async-thread-factorizer
  "Same as async-go-factorizer, but with thread instead of go."
  [nums n]
  (let [in-c (async/chan)
        out-c (async/chan)]
    (async/onto-chan in-c nums)
    (dotimes [i n]
      (async/thread
        (loop []
          (when-let [nextnum (async/<!! in-c)]
            (async/>!! out-c {nextnum (factorize nextnum)})
            (recur)))))
    (receive-n-maps out-c (count nums))))

(defn async-with-pipeline
  "Parallel factorizer using async/pipeline."
  [nums n]
  (let [in-c (async/chan)
        out-c (async/chan)]
    (async/onto-chan in-c nums)
    (async/pipeline n out-c (map #(hash-map % (factorize %))) in-c)
    (receive-n-maps out-c (count nums))))

(defn conjmap
  ([xs x] (conj xs x))
  ([] {}))

(defn rfold
  "Parallel factorizer using r/fold."
  [nums]
  (r/fold conjmap (r/map #(hash-map % (factorize %)) nums)))

(def mynum (* 29 982451653))
(println "Time to factorize " mynum)
(time (factorize mynum))

;;; Without (vec ...), r/fold will be slow because take returns a lazy seq.
(def nums (vec (take 1000 (repeat mynum))))

;(dotimes [_ 5]
  ;;(time (serial-factorizer nums))
  ;;(time (async-go-factorizer nums 16))
  ;;(time (r/fold conjmap (r/map #(hash-map % (factorize %)) nums)))
  ;(time (async-with-pipeline nums 4))
  ;(flush)
  ;)
;(time (async-thread-factorizer nums 20))
;(async-go-factorizer [121 142 157 191] 2)
