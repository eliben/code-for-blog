;;; 'list' method combination multimethods.
;;;
;;; Eli Bendersky [http://eli.thegreenplace.net]
;;; This code is in the public domain.
(defclass Shape () ())
(defclass Rectangle (Shape) ())
(defclass Ellipse (Shape) ())
(defclass Triangle (Shape) ())

(defgeneric getstuff (shape)
  (:method-combination list))

(defmethod getstuff list ((s Shape))
  '(shape stuff))

(defmethod getstuff list ((r Rectangle))
  '(rectangle stuff))

(print (getstuff (make-instance 'Rectangle)))
