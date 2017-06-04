;;; Composing transducers for transforming channels.
;;; 
;;; Eli Bendersky [http://eli.thegreenplace.net]
;;; This code is in the public domain. 
(ns reducers.async-transducer
  (:require [clojure.core.async :as async]))

(defn square [x] (* x x))

(def xform
  (comp
    (filter even?)
    (filter #(< % 10))
    (map square)
    (map inc)))

