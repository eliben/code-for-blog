;;; Code for basic benchmarking of reducers.
;;; 
;;; Eli Bendersky [http://eli.thegreenplace.net]
;;; This code is in the public domain. 

(ns reducers.benchmark
  (:require [clojure.core.reducers :as r]))

;(def s (range 0 9999999))
;(time (reduce + 0 (map inc (filter even? s))))
;(time (reduce + 0 (r/map inc (r/filter even? s)))))

(def s (range 0 999))
(reduce (fn [acc item] (if (even? item)
                         (+ acc (inc item))
                         acc))
        0 s)
