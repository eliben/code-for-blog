;;; call-next-method example with CLOS.
;;;
;;; Eli Bendersky [http://eli.thegreenplace.net]
;;; This code is in the public domain.
(defclass Shape () ())
(defclass Rectangle (Shape) ())
(defclass Ellipse (Shape) ())
(defclass Triangle (Shape) ())

(defgeneric scale (shape factor)
  (:documentation "Scale a shape by a factor"))

(defmethod scale ((s Shape) factor)
  (format t "Scale Shape by ~a~&" factor))

(defmethod scale ((r Rectangle) factor)
  (format t "Scale Rectangle by ~a~&" factor)
  (call-next-method))

(setf r (make-instance 'Rectangle))

(scale r 20)
