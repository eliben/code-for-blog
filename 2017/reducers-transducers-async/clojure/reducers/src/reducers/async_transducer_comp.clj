;;; Composing transducers for transforming channels.
;;; 
;;; Eli Bendersky [http://eli.thegreenplace.net]
;;; This code is in the public domain. 
(ns reducers.async-transducer-comp
  (:require [clojure.core.async :as async]))

(defn square [x] (* x x))

(def xform
  (comp
    (filter even?)
    (filter #(< % 10))
    (map square)
    (map inc)))

(def c (async/chan 1 xform))

(async/go
  (async/onto-chan c [5 6 8 12 15]))
(loop [n (async/<!! c)]
  (when n
    (println n)
    (recur (async/<!! c))))
