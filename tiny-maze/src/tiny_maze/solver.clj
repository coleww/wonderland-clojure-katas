(ns tiny-maze.solver)

(defn findit [maze targ]
  (let [the-maze (flatten maze)]
    (nth the-maze (->
                   (map #(:content %) the-maze)
                   (.indexOf targ)))))

(defn map-cell
  [cell x y]
  {:x x
   :y y
   :content cell
   :g 0
   :h 0
   :f 0
   :parent nil})

(defn map-row
  [row y]
  (map-indexed #(map-cell %2 %1 y) row))

(defn map-maze
  [maze]
  (map-indexed #(map-row %2 %1) maze))

(defn get-neighbours
  [cell maze]
  (let [x (:x cell)
        y (:y cell)
        x-max (count (first maze))
        y-max (count maze)
        diffs [[0 1] [0 -1] [1 0] [-1 0]]
        offs (map #(vector (+ x (first %)) (+ y (second %))) diffs)
        in-offs (filter #(and (< -1 (second %) y-max) (< -1 (first %) x-max)) offs)]
    (map #(nth (nth maze (second %)) (first %)) in-offs)))

(defn get-g-cost [parent target]
  ; if its diagonal, 14, horiz/vert, 10
  (+ (:g parent) (if (= 1 (Math/abs (+ (- (:y parent) (:y target))  (- (:x parent) (:x target)))))
          10
          14)))

(defn get-h-cost [current exit]
  (* 10 (+ (Math/abs (- (:x current) (:x exit))) (Math/abs (- (:y current) (:y exit))))))

(defn is-in-list [list item]
  (some #(and (= (:x item) (:x %)) (= (:y item) (:y %))) list))

(defn solve-maze [maze]
  (let [the-maze (map-maze maze)
        start (findit the-maze :S)
        end (findit the-maze :E)
        open-list (atom [start])
        closed-list (atom [])
        found-it (atom false)]
    ; until we find it, or run out of elements in the open-list
    (while (and (not @found-it) (pos? (count @open-list)))
      (do
        ; sort the open-list by f score
        (swap! open-list #(sort-by :f %))

        (let [current (first @open-list)
              neighbours (get-neighbours current the-maze)]
          ;; shift off first element of the open list
          (swap! open-list rest)
          ;; add it to the closed list
          (swap! closed-list conj current)

          (doseq [el neighbours]
            ; if this el is not in the closed-list, and it is not a wall
            (if (and (not (is-in-list @closed-list el)) (not= 1  (:content el)))
              (if (is-in-list @open-list el)
                 ; if it is in the open list
                (do
                  (if (< (get-g-cost current el) (:g el))
                    (let [g (get-g-cost current el)
                        h (:h el)
                        f (+ g h)
                        updated-el (->
                                    el
                                    (assoc :parent current)
                                    (assoc :f f)
                                    (assoc :g g))
                          new-list (filter #(or  (not=  (:x %) (:x updated-el)) (not= (:y %) (:y updated-el))  ) @open-list)]

                      (reset! open-list (cons updated-el new-list)))))
                (do
                  ; g cost is 10 for horiz/vert, 14 for diag  movement from parent + the g cost of parent
                  (let [g (get-g-cost current el)
                        h (get-h-cost el end)
                        f (+ g h)
                        updated-el (->
                                    el
                                    (assoc :parent current)
                                    (assoc :f f)
                                    (assoc :g g)
                                    (assoc :h h)
                                    )]
                    (if (= (:content el) :E) (reset! found-it updated-el) )
                    (swap! open-list conj updated-el)))))))))


    (let [paths (atom [])
          maaze (atom the-maze)]

      (while (boolean @found-it)
        (swap! paths conj @found-it)
        (reset! found-it (:parent @found-it)))

      (reset! maaze (mapv #(mapv (fn [x] x)  %) @maaze))

      (while (pos? (count @paths))
        (let [car (first @paths)]

          (->>
           (update-in @maaze [(:y car) (:x car) :content] (fn [x] :x))

           (reset! maaze))

          (swap! paths rest)))
      (mapv (fn [ro] (mapv (fn [c] (:content c)) ro)) @maaze))))
