; Basic example of single dispatch on built-in and custom types.
;
; Eli Bendersky [http://eli.thegreenplace.net]
; This code is in the public domain.
(ns multi.single-dispatch)

; Single-dispatch multimethod, dispatching on the class of the argument.
(defmulti describe-thing class)

; Define dispatcher methods for built-in Long and String values.
(defmethod describe-thing java.lang.Long
  [thing] (println "a Long:" (str thing)))

(defmethod describe-thing java.lang.String
  [thing] (println "a String" (str thing)))

; Define a custom class and add a dispatcher for it.
(defrecord Person [name phone])

(defmethod describe-thing Person
  [thing] (println "a Person with name" (:name thing)))

(describe-thing 12)
(describe-thing "william")
(describe-thing (Person. "Joe" 42))
