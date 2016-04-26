;;; :before method combinations in CLOS.
;;;
;;; Eli Bendersky [http://eli.thegreenplace.net]
;;; This code is in the public domain.
(defclass Shape () ())
(defclass Rectangle (Shape) ())
(defclass Ellipse (Shape) ())
(defclass Triangle (Shape) ())

(defgeneric scale (shape factor)
  (:documentation "Scale a shape by a factor"))

(defmethod scale ((r Rectangle) factor)
  (format t "Scale Rectangle by ~a~&" factor))

(defmethod scale ((e Ellipse) factor)
  (format t "Scale Ellipse by ~a~&" factor))

(defmethod scale :before ((s Shape) factor)
  (format t ":before preprocessor on ~a scaled by ~a~&" (type-of s) factor))

(setf r (make-instance 'Rectangle))
(setf e (make-instance 'Ellipse))

(scale r 20)
(scale e 30)
