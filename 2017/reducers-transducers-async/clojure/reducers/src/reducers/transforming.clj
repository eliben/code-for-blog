(ns reducers.transforming)

(defn mapping-transform
  [mapf]
  (fn [reducingf]
    (fn [acc item]
      (reducingf acc (mapf item)))))

(reduce ((mapping-transform #(* % %)) +) 0 [1 2 3 4 5 6])

;;; Instead of summing up the transformed sequence, conj it together into a
;;; new vector.
(reduce ((mapping-transform #(* % %)) conj) [] [1 2 3 4 5 6])

(defn filtering-transform
  [predicate]
  (fn [reducingf]
    (fn [acc item]
      (if (predicate item)
        (reducingf acc item)
        acc))))

(reduce ((filtering-transform even?) +) 0 [1 2 3 4 5 6])

(reduce ((filtering-transform even?)
           ((mapping-transform inc) +)) 0 (range 0 10))
