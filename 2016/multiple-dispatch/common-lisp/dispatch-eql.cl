;;; Eql-based dispatch with CLOS.
;;;
;;; Eli Bendersky [http://eli.thegreenplace.net]
;;; This code is in the public domain.
(defclass Shape () ())
(defclass Rectangle (Shape) ())
(defclass Ellipse (Shape) ())
(defclass Triangle (Shape) ())

(defgeneric scale (shape factor)
  (:documentation "Scale a shape by a factor"))

(defmethod scale ((shape Shape) factor)
  (format t "Scale shape by ~a~&" factor))

(defmethod scale ((shape Shape) (factor (eql 0)))
  (format t "Scale shape by a zero~&"))

(setf s (make-instance 'Shape))

(scale s 42)
(scale s 0)
