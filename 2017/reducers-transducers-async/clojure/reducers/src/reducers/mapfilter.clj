(ns reducers.mapfilter)

(reduce (fn [acc item] (conj acc (inc item))) [] [1 2 3 4 5])

(reduce (fn [acc item] (if (even? item)
                         (conj acc item)
                         acc))
        [] [1 2 3 4 5])
