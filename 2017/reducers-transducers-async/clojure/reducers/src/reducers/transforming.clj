(ns reducers.transforming)

(defn mapping-transform
  [mapf]
  (fn [reducingf]
    (fn [acc item]
      (reducingf acc (mapf item)))))

(defn add
  [x y]
  (prn (str "Adding " x "+" y))
  (+ x y))

(reduce ((mapping-transform #(* % %)) +) [1 2 3 4 5 6])
