;;; Concurrent HTTP client using go-blocks and threads.
;;;
;;; Inspired by: http://martintrojer.github.io/clojure/2013/07/07/coreasync-and-blocking-io
;;;
;;; Eli Bendersky [http://eli.thegreenplace.net]
;;; This code is in the public domain.
(ns clojure-blocking-async.http-client
  (:require [clojure.core.async :as async])
  (:require [clj-http.client]))

(def url-template "https://github.com/eliben/pycparser/pull/%d")

(defn blocking-get-page [i]
  (clj-http.client/get (format url-template i)))

(defn verify
  []
  (let [r (blocking-get-page 22)]
    (time
     (clojure.string/includes? (:body r) "integer typedefs"))))

(defn get-multiple
  [generator-fn start n]
  (let [c (async/chan)]
    (generator-fn c start n)
    (loop [i 0
           res []]
      (if (= i n)
        res
        (recur (inc i) (conj res (async/<!! c)))))))

(defn go-blocking-generator
  [c start n]
  (doseq [i (range start (+ start n))]
    (async/go (async/>! c (blocking-get-page i)))))

(defn thread-blocking-generator
  [c start n]
  (doseq [i (range start (+ start n))]
    (async/thread (async/>!! c (blocking-get-page i)))))

(def start 10)
(def num-results 20)
;(time (count (get-multiple go-blocking-generator 10 num-results)))
(time (count (get-multiple thread-blocking-generator start num-results)))

(prn (verify))
