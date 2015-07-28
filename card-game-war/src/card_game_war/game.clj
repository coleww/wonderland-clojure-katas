(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn convert-card
  [key]
  (if (= (type key) java.lang.Long)
    key
    (key {:jack 10 :queen 11 :king 12 :ace 13})))

(defn play-round [player1-card player2-card]
  (let [one (assoc player1-card 1 (convert-card (get player1-card 1)))
        two (assoc player2-card 1 (convert-card (get player2-card 1)))]

    (if (= (get one 1) (get two 1))
      ;; TIE BBBBREAKER
      (if (< (.indexOf suits (get player1-card 0)) (.indexOf suits (get player2-card 0)))
        1
        0)
      (if (< (get one 1) (get two 1))
        1
        0))))



(defn play-game [player1-cards player2-cards]
  (let [results (map-indexed #(play-round %2 (nth player2-cards %1)) player1-cards)
        ones (filter #(= % 0) results)
        twos (filter #(= % 1) results)]
    (if (< (count ones) (count twos))
      1
      0
      )))
