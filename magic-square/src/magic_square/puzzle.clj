(ns magic-square.puzzle)

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn make-a-square [values]
  (mapv #(vec %) (partition 3 (shuffle values))))

(defn sumrows [m]
  (map #(reduce + %) m))

(defn sumcols [m]
  [(reduce + (map first m))
   (reduce + (map second m))
   (reduce + (map last m))])

(defn sumdiagonals [m]
  [(+ (get-in m [0 0]) (get-in m [1 1]) (get-in m [2 2]))
   (+ (get-in m [2 0]) (get-in m [1 1]) (get-in m [0 2]))])

(defn check-square [square]
  (and
   (=  (set (sumrows square))
       (set (sumcols square))
       (set (sumdiagonals square)))
   (=  1
       (count (set (sumrows square)))
       (count (set (sumcols square)))
       (count (set (sumdiagonals square))))))

;; BRUTE FARCE
(defn magic-square [values]
  (let [sqr (atom (make-a-square values))]
    (while (not (check-square @sqr))
      (reset! sqr (make-a-square values)))
    @sqr))
