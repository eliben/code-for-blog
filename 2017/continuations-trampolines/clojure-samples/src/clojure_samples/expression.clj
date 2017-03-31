;;; A mathematical expression with CPS.
;;;
;;; Eli Bendersky [http://eli.thegreenplace.net]
;;; This code is in the public domain.
(ns clojure-samples.expression)

(defn expr
 []
  (* 2 (+ 3 4)))

(expr)

(defn end-cont [value] (println value))

(defn apply-cont
  [cont value]
  (cont value))

(defn expr-cps
  [cont]
  (apply-cont cont (+ 3 4)))

(defn make-double-cont
  [saved-cont]
  (fn [value]
    (apply-cont saved-cont (* 2 value))))

;;; Composed
(expr-cps (make-double-cont end-cont))

;;; Inline, without using make-double-cont
(expr-cps (fn [value] (apply-cont end-cont (* 2 value))))

(defn make-plus100-cont
  [saved-cont]
  (fn [value]
    (apply-cont saved-cont (+ value 100))))

;;; Composed
(expr-cps (make-double-cont (make-plus100-cont end-cont)))

;;; Inline, without using make-double-cont and make-plus100-cont
(expr-cps (fn [value] (apply-cont
                        (fn [value] (apply-cont end-cont (+ value 100)))
                        (* 2 value))))

(defn real-end-cont [value] value)

(expr-cps (fn [value]
            ((fn [value] (real-end-cont (+ value 100))) (* 2 value))))
