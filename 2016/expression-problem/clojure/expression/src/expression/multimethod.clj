; Solving the expression problem with multimethods. Each operation is a
; multimethod, and we add methods per type supported by the operation.
;
; Eli Bendersky [http://eli.thegreenplace.net]
; This code is in the public domain.
(ns expression.multimethod)

(defrecord Constant [value])
(defrecord BinaryPlus [lhs rhs])

(defmulti evaluate class)

(defmethod evaluate Constant
  [c] (:value c))

(defmethod evaluate BinaryPlus
  [bp] (+ (evaluate (:lhs bp)) (evaluate (:rhs bp))))

(defmulti stringify class)

(defmethod stringify Constant
  [c] (str (:value c)))

(defmethod stringify BinaryPlus
  [bp]
  (clojure.string/join " + " [(stringify (:lhs bp))
                              (stringify (:rhs bp))]))

; ----

(def c1 (Constant. 1.1))
(def c2 (Constant. 2.2))
(def p1 (BinaryPlus. c1 c2))
(def p2 (BinaryPlus. p1 c2))

(evaluate c1)
(evaluate p1)
(evaluate p2)

(stringify p1)
(stringify p2)
