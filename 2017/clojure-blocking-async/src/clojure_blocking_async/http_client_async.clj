;;; Concurrent HTTP client using the asynchronous API of clj-http in conjunction
;;; with go-blocks.
;;;
;;; Eli Bendersky [http://eli.thegreenplace.net]
;;; This code is in the public domain.
(ns clojure-blocking-async.http-client-async
  (:require [clojure.core.async :as async])
  (:require [clj-http.client]))

(def url-template "https://github.com/eliben/pycparser/pull/%d")

(defn go-async-generator
  [c start n]
  (doseq [i (range start (+ start n))]
    (clj-http.client/get
      (format url-template i)
      {:async? true}
      (fn [response]
        (async/go (async/>! c response)))
      ;; Exception callback.
      (fn [exc]
        (throw exc)))))

(defn get-multiple
  [generator-fn start n]
  (let [c (async/chan)]
    (generator-fn c start n)
    (loop [i 0
           res []]
      (if (= i n)
        res
        (recur (inc i) (conj res (async/<!! c)))))))

(def start 10)
(def num-results 20)

(let [results (time (get-multiple go-async-generator start num-results))]
  (printf "Have %d results%n" (count results)))
