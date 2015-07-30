(ns wonderland-number.finder)

(defn splitty
  [num times]
  (->
   (* num times)
   (str)
   (clojure.string/split #"")
   (sort)))

(defn check-num
  [num]
  (apply = (map #(splitty num %) (range 1 7))))

(defn wonderland-number []
  (some #(if (check-num %) %)  (range 100000 999999)))
