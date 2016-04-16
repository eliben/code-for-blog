; Variation of shapes-double-dispatch but with "base class" functionality
; and maps instead of records.
;
; Eli Bendersky [http://eli.thegreenplace.net]
; This code is in the public domain.
(ns multi.shapes-double-dispatch-map-bases)

(derive ::rectangle ::shape)
(derive ::ellipse ::shape)
(derive ::triangle ::shape)

; Helper constructors to create new "instances" of shapes. In a real program
; they could take parameters that would be assigned to fields.
(defn make-shape [] {:kind ::shape})
(defn make-rectangle [] {:kind ::rectangle})
(defn make-ellipse [] {:kind ::ellipse})
(defn make-triangle [] {:kind ::triangle})

(defmulti intersect
  (fn [a b]
    [(:kind a) (:kind b)]))

(defmethod intersect [::rectangle ::ellipse]
  [r e] (printf "Rectangle x Ellipse"))

(defmethod intersect [::rectangle ::rectangle]
  [r1 r2] (printf "Rectangle x Rectangle"))

(defmethod intersect [::rectangle ::shape]
  [r s] (printf "Rectangle x Shape"))

(intersect (make-rectangle) (make-ellipse))
(intersect (make-rectangle) (make-rectangle))
(intersect (make-rectangle) (make-shape))
(intersect (make-rectangle) (make-triangle))
