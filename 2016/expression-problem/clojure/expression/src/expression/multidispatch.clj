; "Solving" the expression problem with classical multiple dispatch. apply-op is
; a single multimethod that dispatches on an operation / type pair (the
; operation is just a keyword).
;
; This approach has more theoretical than practical value - demonstrating how
; the problem maps to multiple dispatch. For realistic scenarios the
; expression.multimethod and expression.protocols approaches are preferable.
;
; Eli Bendersky [http://eli.thegreenplace.net]
; This code is in the public domain.
(ns expression.multidispatch)

(defrecord Constant [value])
(defrecord BinaryPlus [lhs rhs])

(defmulti apply-op
  (fn [op ty] [op (class ty)]))

(defmethod apply-op [:evaluate Constant]
  [_ c] (:value c))

(defmethod apply-op [:evaluate BinaryPlus]
  [op bp] (+ (apply-op op (:lhs bp)) (apply-op op (:rhs bp))))

(defmethod apply-op [:stringify Constant]
  [_ c] (str (:value c)))

(defmethod apply-op [:stringify BinaryPlus]
  [op bp]
  (clojure.string/join " + " [(apply-op op (:lhs bp))
                              (apply-op op (:rhs bp))]))

; Testing

(def c1 (Constant. 1.1))
(def c2 (Constant. 2.2))
(def p1 (BinaryPlus. c1 c2))
(def p2 (BinaryPlus. p1 c2))

(apply-op :evaluate c1)
(apply-op :evaluate p1)
(apply-op :evaluate p2)

(apply-op :stringify p1)
(apply-op :stringify p2)
