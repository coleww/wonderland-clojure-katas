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
  (vec (map-indexed #(map-cell %2 %1 y) row)))

(defn map-maze
  [maze]
  (vec (map-indexed #(map-row %2 %1) maze)))

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
  (+ (:g parent) (if (= 1 (Math/abs (+ (- (:y parent) (:y target))  (- (:x parent) (:x target)))))
                   10
                   14)))

(defn get-h-cost [current exit]
  (* 10 (+ (Math/abs (- (:x current) (:x exit))) (Math/abs (- (:y current) (:y exit))))))

(defn is-in-list [list item]
  (some #(and (= (:x item) (:x %)) (= (:y item) (:y %))) list))

(defn handle-open-el [el current open-list ]
  (if (< (get-g-cost current el) (:g el))
    (let [g (get-g-cost current el)
          h (:h el)
          f (+ g h)
          updated-el (->
                      el
                      (assoc :parent current)
                      (assoc :f f)
                      (assoc :g g))
          new-list (filter #(or (not= (:x %) (:x updated-el)) (not= (:y %) (:y updated-el))) @open-list)]
      (reset! open-list (cons updated-el new-list)))))

(defn handle-new-el [el current open-list end found-it]
  (let [g (get-g-cost current el)
        h (get-h-cost el end)
        f (+ g h)
        updated-el (->
                    el
                    (assoc :parent current)
                    (assoc :f f)
                    (assoc :g g)
                    (assoc :h h))]
    (if (= (:content el) :E)
      (reset! found-it updated-el) )
    (swap! open-list conj updated-el)))

(defn run-the-algo [the-maze open-list closed-list end found-it]
  (swap! open-list #(sort-by :f %))

  (let [current (first @open-list)
        neighbours (get-neighbours current the-maze)]
    (swap! open-list rest)
    (swap! closed-list conj current)

    (doseq [el neighbours]
      (if (and (not (is-in-list @closed-list el)) (not= 1 (:content el)))
        (if (is-in-list @open-list el)
          (handle-open-el el current open-list)
          (handle-new-el el current open-list end found-it))))))

(defn reconstruct-path [the-maze found-it]
  (let [paths (atom [])
        solved-maze (atom the-maze)]
    (while (boolean @found-it)
      (swap! paths conj @found-it)
      (reset! found-it (:parent @found-it)))
    (while (pos? (count @paths))
      (let [curr (first @paths)]
        (swap! solved-maze update-in [(:y curr) (:x curr) :content] (fn [x] :x))
        (swap! paths rest)))
    (mapv (fn [row] (mapv (fn [cell] (:content cell)) row)) @solved-maze)))

(defn solve-maze [maze]
  (let [the-maze (map-maze maze)
        open-list (atom [(findit the-maze :S)])
        closed-list (atom [])
        end (findit the-maze :E)
        found-it (atom false)]
    (while (and (not @found-it) (pos? (count @open-list)))
      (run-the-algo the-maze open-list closed-list end found-it))
    (reconstruct-path the-maze found-it)))
