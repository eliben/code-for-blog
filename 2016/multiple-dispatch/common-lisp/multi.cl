;;; Simple type-based multiple dispatch demonstrated with Common Lisp.
;;;
;;; I tested this code with GNU CLISP 2.49 running on x64 Ubuntu.
;;;
;;; $ clisp multi.cl
;;;
;;; Eli Bendersky [http://eli.thegreenplace.net]
;;; This code is in the public domain.

(defclass Shape () ())
(defclass Rectangle (Shape) ())
(defclass Ellipse (Shape) ())
(defclass Triangle (Shape) ())

(defgeneric intersect (x y)
  (:documentation "Shape intersection")
  (:method (x y)
    (error "Cannot interesect these shapes")))

(defmethod intersect ((r Rectangle) (e Ellipse))
  (format t "Rectangle x Ellipse [names r=~a, e=~a]~&"
          (type-of r) (type-of e)))

(defmethod intersect ((r1 Rectangle) (r2 Rectangle))
  (format t "Rectangle x Rectangle [names r1=~a, r2=~a]~&"
          (type-of r1) (type-of r2)))

(defmethod intersect ((r Rectangle) (s Shape))
  (format t "Rectangle x Shape [names r=~a, s=~a]~&"
          (type-of r) (type-of s)))

(defmethod intersect ((r Ellipse) (s Shape))
  (format t "Ellipse x Shape [names r=~a, s=~a]~&"
          (type-of r) (type-of s)))

;; Create some objects
(setf r1 (make-instance 'Rectangle))
(setf r2 (make-instance 'Rectangle))
(setf e1 (make-instance 'Ellipse))
(setf t1 (make-instance 'Triangle))

;; Do intersects
(intersect r1 e1)
(intersect r1 r2)
(intersect r1 t1)

(format t "Intersection with mapcar~&")
(mapcar #'intersect (list r1 r2 e1) (list r2 e1 t1))

;; Higher-order function invocation
(defun domap (func lst1 lst2)
  (mapcar func lst1 lst2))

(print (domap #'cons (list r1 r2 e1) (list r2 e1 t1)))

(domap #'intersect (list r1 r2 e1) (list r2 e1 t1))
