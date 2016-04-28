; Single dispatch on a map/record keyword rather than on type.
;
; Eli Bendersky [http://eli.thegreenplace.net]
; This code is in the public domain.
(ns multi.single-dispatch-mapkey)

; Single-dispatch multimethod, dispatching on the value of a key in a map.
;(defmulti promotion-due :position)

; Alternative formulation
(defmulti promotion-due
  (fn [emp]
    (:position emp)))

(defmethod promotion-due :engineer
  [emp] (> (:lines-of-code emp) 100000))

(defmethod promotion-due :manager
  [emp] (> (:num-reports emp) 10))

; Works with records
(defrecord Employee [name position num-reports lines-of-code])

(promotion-due (Employee. "jim" :manager 12 0))
(promotion-due (Employee. "sue" :engineer 0 98000))

; And works with maps!
(def joe {:name "joe", :position :manager, :num-reports 9})
(def tim {:name "tim", :position :engineer, :lines-of-code 124000})

(promotion-due joe)
(promotion-due tim)
