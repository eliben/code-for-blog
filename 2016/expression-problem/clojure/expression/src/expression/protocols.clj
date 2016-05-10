; Solving the expression problem with protocols. Each operation is a protocol
; and we extend types to support this protocol.
;
; Eli Bendersky [http://eli.thegreenplace.net]
; This code is in the public domain.
(ns expression.protocols)

(defrecord Constant [value])
(defrecord BinaryPlus [lhs rhs])

(defprotocol Evaluatable
  (evaluate [this]))

(defprotocol Stringable
  (stringify [this]))

(extend-type Constant
  Evaluatable
    (evaluate [this] (:value this))
  Stringable
    (stringify [this] (str (:value this))))

(extend-type BinaryPlus
  Evaluatable
    (evaluate [this] (+ (evaluate (:lhs this)) (evaluate (:rhs this))))
  Stringable
    (stringify [this]
      (clojure.string/join " + " [(stringify (:lhs this))
                                  (stringify (:rhs this))])))

; Also have an alternative to use extend-protocol here...
; the core is 'extend' - a true matrix of type/op

(def c1 (Constant. 1.1))
(def c2 (Constant. 2.2))
(def p1 (BinaryPlus. c1 c2))
(def p2 (BinaryPlus. p1 c2))

(evaluate p2)
(stringify p2)

; "Adding" a new data type - the built-in double type and showing how to extend
; all protocols for it.
(extend-type java.lang.Double
  Evaluatable
    (evaluate [this] this)
  Stringable
    (stringify [this] (str this)))

(def f1 11.5)
(def p3 (BinaryPlus. c1 f1))

(evaluate p3)
(stringify p3)

; Adding a new protocol and implementing it for the existing data types.
(defprotocol Serializable
  (serialize [this]))

(extend-protocol Serializable
  Constant
    (serialize [this] [(type this) (:value this)])
  BinaryPlus
    (serialize [this] [(type this)
                       (serialize (:lhs this))
                       (serialize (:rhs this))]))

(serialize p2)
