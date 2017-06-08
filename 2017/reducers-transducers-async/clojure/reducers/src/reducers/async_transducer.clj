;;; Pipeline with core.async, showing off transducers.
;;; 
;;; Eli Bendersky [http://eli.thegreenplace.net]
;;; This code is in the public domain. 
(ns reducers.async-transducer
  (:require [clojure.core.async :as async]))

;;; Version 1 is similar to the squaring pipeline example in Go, described in:
;;; https://blog.golang.org/pipelines
(defn gen-1
  [& nums]
  (let [c (async/chan)]
    (async/go
      (doseq [n nums]
        (async/>! c n))
      (async/close! c))
    c))

(defn sq-1
  [cin]
  (let [cout (async/chan)]
    (async/go-loop [n (async/<! cin)]
      (if n
        (do
          (async/>! cout (* n n))
          (recur (async/<! cin)))
        (async/close! cout)))
    cout))

(defn main-1
  []
  (let [c-gen (gen-1 2 3 4 5)
        c-sq (sq-1 c-gen)]
    (loop [n (async/<!! c-sq)]
      (when n
        (println n)
        (recur (async/<!! c-sq))))))

;;; Version 2 uses Clojure core.async built-ins to-chan and map< to make the
;;; pipeline more succinct. Note that with the advent of transducers, map< is
;;; now deprecated.
(defn gen-2
  [& nums]
  (async/to-chan nums))

(defn sq-2
  [cin]
  (async/map< #(* % %) cin))

(defn main-2
  []
  (let [c-gen (gen-2 2 3 4 5)
        c-sq (sq-2 c-gen)]
    (loop [n (async/<!! c-sq)]
      (when n
        (println n)
        (recur (async/<!! c-sq))))))

;;; Version 3 uses transducers. We no longer need a separate sq channel, as we
;;; can attach a squaring transducer onto the generating channel.

(defn main-3
  []
  (let [c-sq (async/chan 1 (map #(* % %)))]
    (async/onto-chan c-sq [2 3 4 5])
    (loop [n (async/<!! c-sq)]
      (when n
        (println n)
        (recur (async/<!! c-sq))))))

(main-3)
