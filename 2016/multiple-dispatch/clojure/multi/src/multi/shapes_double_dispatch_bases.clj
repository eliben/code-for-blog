; Variation of shapes-double-dispatch but with "base class" functionality
; similar to what we have in C++ / CLOS.
;
; Eli Bendersky [http://eli.thegreenplace.net]
; This code is in the public domain.
(ns multi.shapes-double-dispatch-bases)

(defrecord Shape [kind])
(defrecord Rectangle [kind])
(defrecord Ellipse [kind])
(defrecord Triangle [kind])

(derive ::rectangle ::shape)
(derive ::ellipse ::shape)
(derive ::triangle ::shape)

; Helper constructors
(defn make-shape [] (Shape. ::shape))
(defn make-rectangle [] (Rectangle. ::rectangle))
(defn make-ellipse [] (Ellipse. ::ellipse))
(defn make-triangle [] (Triangle. ::triangle))

(defmulti intersect
  (fn [a b]
    [(:kind a) (:kind b)]))

(defmethod intersect [::rectangle ::ellipse]
  [r e] (printf "Rectangle x Ellipse [names r=%s, e=%s]\n"
                (class r) (class e)))

(defmethod intersect [::rectangle ::rectangle]
  [r1 r2] (printf "Rectangle x Rectangle [names r1=%s, r2=%s]\n"
                  (class r1) (class r2)))

(defmethod intersect [::rectangle ::shape]
  [r s] (printf "Rectangle x Shape [names r=%s, s=%s]\n"
                (class r) (class s)))

(intersect (make-rectangle) (make-ellipse))
(intersect (make-rectangle) (make-rectangle))
(intersect (make-rectangle) (make-shape))

; This now works since multimethod dispatch uses isa? for matching, and
; triangle's kind isa? shape's kind.
(intersect (make-rectangle) (make-triangle))
