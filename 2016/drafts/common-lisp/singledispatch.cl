;;; Single dispatch with CLOS.
;;;
;;; Eli Bendersky [http://eli.thegreenplace.net]
;;; This code is in the public domain.
(defclass Person () ())

(defmethod frobnicate ((p Person) record spreadsheet)
  (format t "~a ~a ~a~&" (type-of p) (type-of record) (type-of spreadsheet)))

(defmethod interplex (solution (p Person))
  (format t "~a ~a~&" (type-of solution) (type-of p)))

(setf p1 (make-instance 'Person))

(frobnicate p1 '(some record) '(some spreadsheet))
(interplex '(juju) p1)
