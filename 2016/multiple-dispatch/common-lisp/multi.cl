; I tested this code with GNU CLISP 2.49 running on x64 Ubuntu.
;
; Eli Bendersky [http://eli.thegreenplace.net]
; This code is in the public domain.
(defclass Shape () ())

(defclass Rectangle (Shape) ())

(defclass Ellipse (Shape) ())

(defclass Triangle (Shape) ())

(defmethod name ((shape Shape))
  (type-of shape))

(defmethod intersect ((r Rectangle) (e Ellipse))
  (format t "Rectangle x Ellipse [names r=~a, e=~a]~&" (name r) (name e)))

(defmethod intersect ((r1 Rectangle) (r2 Rectangle))
  (format t "Rectangle x Rectangle [names r1=~a, r2=~a]~&" (name r1) (name r2)))

(defmethod intersect ((r Rectangle) (s Shape))
  (format t "Rectangle x Shape [names r=~a, s=~a]~&" (name r) (name s)))

(setf r1 (make-instance 'Rectangle))
(setf r2 (make-instance 'Rectangle))
(setf e1 (make-instance 'Ellipse))
(setf t1 (make-instance 'Triangle))

(intersect r1 e1)
(intersect r1 r2)
(intersect r1 t1)
