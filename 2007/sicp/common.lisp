(defun square (x) 
  (* x x))

(defun cube (x)
  (* x x x))

(defun average (a b)
  (/ (+ a b) 2))

(defun quotient (a b)
  (multiple-value-bind (q r) (floor a b) q))

(defun for-each (func items)
  (dolist (item items)
    (funcall func item)))

(defun distinct? (items)
  (cond ((null items) t)
        ((null (cdr items)) t)
        ((member (car items) (cdr items)) nil)
        (t (distinct? (cdr items)))))

(setq *random-state* (make-random-state t))

; (setf custom:*trace-indent* 2)
; (setf custom:*report-error-print-backtrace* t)

(defmacro deflex (var val &optional (doc nil docp))    
  "Define a top level (global) lexical VAR with
  initial value VAL, which is assigned 
  unconditionally as with DEFPARAMETER. If a DOC
  string is provided, it is attached to both the 
  name |VAR| and the name *STORAGE-FOR-DEFLEX-VAR-|VAR|* 
  as a documentation string of kind 'VARIABLE. 
  The new VAR will have lexical scope and thus may be
  shadowed by LET bindings without affecting its 
  dynamic (global) value."
  (let* ((s0 (symbol-name '#:*storage-for-deflex-var-))
         (s1 (symbol-name var))
         (s2 (symbol-name '#:*))
         (s3 (symbol-package var))
         (backing-var 
          (intern (concatenate 'string s0 s1 s2) s3)))
    ; Note: The DEFINE-SYMBOL-MACRO must be the last 
    ; thing we do so that the value of the form is the 
    ; symbol VAR.
    ; (print (concatenate 'string s0 s1 s2))
    (if docp
      `(progn
         (defparameter ,backing-var ,val ,doc)
         (setf (documentation ',var 'variable) ,doc)
         (define-symbol-macro ,var ,backing-var))
      `(progn
         (defparameter ,backing-var ,val)
         (define-symbol-macro ,var ,backing-var))))) 

; CLISP predefines with-gensyms
;
; (defmacro with-gensyms ((&rest names) &body body)
  ; `(let ,(loop for n in names collect `(,n (make-symbol ,(string n))))
     ; ,@body))

(defmacro ppme (form &environment env)
  (progn
    (write (macroexpand-1 form env)
           :length nil
           :level nil
           :circle nil
           :pretty t
           :gensym nil
           :right-margin 83
           :case :downcase)
    nil))

(defun smallest-divisor (n)
  (find-divisor n 2))

(defun next-divisor (n)
  (if (= n 2)
    3
    (+ n 2)))

(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (t (find-divisor n (next-divisor test-divisor)))))

(defun divides? (a b)
  (= (rem b a) 0))

(defun prime? (n)
  (= n (smallest-divisor n)))


; (defmacro qq (e)
  ; (list 'list e (symbol-value e)))

(defmacro qq (e)
  `(list ',e ,e))

(setf brick 100)

; (format t "~a~%" (qq brick))

; (ppme (qq brick))
; (format t "~a~%" (qq brick))
