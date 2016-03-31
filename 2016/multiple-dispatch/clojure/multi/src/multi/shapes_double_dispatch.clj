; Eli Bendersky [http://eli.thegreenplace.net]
; This code is in the public domain.
(ns multi.shapes-double-dispatch)

(deftype Shape [])
(deftype Rectangle [])
(deftype Ellipse [])
(deftype Triangle [])

(defmulti intersect
  (fn [a b]
    [(class a) (class b)]))

(defmethod intersect [Rectangle Ellipse]
  [r e] (printf "Rectangle x Ellipse [names r=%s, e=%s]\n"
                (class r) (class e)))

(defmethod intersect [Rectangle Rectangle]
  [r1 r2] (printf "Rectangle x Rectangle [names r1=%s, r2=%s]\n"
                  (class r1) (class r2)))

(defmethod intersect [Rectangle Shape]
  [r s] (printf "Rectangle x Shape [names r=%s, s=%s]\n"
                (class r) (class s)))

(intersect (Rectangle.) (Ellipse.))
(intersect (Rectangle.) (Rectangle.))
(intersect (Rectangle.) (Shape.))

; This is an error: Triangle is in no way related to Shape, and no method is
; defined to dispatch [Rectangle Triangle]
;(intersect (Rectangle.) (Triangle.))
