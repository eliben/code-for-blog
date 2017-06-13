;;; Finding thread pool size by launching sleeps.
;;; 
;;; Eli Bendersky [http://eli.thegreenplace.net]
;;; This code is in the public domain.
(ns clojure-blocking-async.find-pool-size
  (:require [clojure.core.async :as async]))

(defn receive-n
  "Receive n items from the given channel and return them as a vector."
  [c n]
  (loop [i 0
         res []]
    (if (= i n)
      res
      (recur (inc i) (conj res (async/<!! c))))))

(defn launch-n-go-blocks
  [n]
  (let [c (async/chan)]
    (dotimes [i n]
      (async/go
        (Thread/sleep 10)
        (async/>! c i)))
    (receive-n c n)))

(defn launch-n-threads
  [n]
  (let [c (async/chan)]
    (dotimes [i n]
      (async/thread
        (Thread/sleep 10)
        (async/>!! c i)))
    (receive-n c n)))

(doseq [i (range 1 19)]
  (printf "Launching %2s -> %s" 
          i
          (with-out-str 
            (time (launch-n-threads i)))))
