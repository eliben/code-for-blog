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

; Testing

(def c1 (Constant. 1.1))
(def c2 (Constant. 2.2))
(def p1 (BinaryPlus. c1 c2))
(def p2 (BinaryPlus. p1 c2))

(evaluate c1)
(evaluate p1)
(evaluate p2)

(stringify p1)
(stringify p2)

; Extending with a new type - FunctionCall

(defrecord FunctionCall [func argument])

(defmethod evaluate FunctionCall
  [fc] ((:func fc) (evaluate (:argument fc))))

(defmethod stringify FunctionCall
  [fc] (str (clojure.repl/demunge (str (:func fc)))
            "("
            (stringify (:argument fc))
            ")"))

; Testing

(defn twice
  [x] (* x 2))

(def fc (->FunctionCall twice p2))
(evaluate fc)
(stringify fc)
