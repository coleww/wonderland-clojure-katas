(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn is-one-letter-away [w1 w2]
  (let [woop (clojure.string/split w2 #"")]
    (= 1  (count (filter #(not %) (map-indexed #(= (nth woop %1) %2) (clojure.string/split w1 #"")))))))

(defn get-possibilities [word1 words]
  (filter #(if (=  (count word1) (count %)) (is-one-letter-away word1 %) false) words))

(defn wordMap [words]
  (reduce #(assoc %1 (keyword %2) (get-possibilities %2 words) ) {} words))

(defn doublets [word1 word2]
  (let [the-map (wordMap words)
        to-search (atom [[word1]])
        visited (atom [])
        doit (atom true)
        it (atom [])]
    (while (and @doit (pos? (count @to-search)))
      (let [curry (first @to-search)
            current (last curry)
            nexts ((keyword current) the-map)]
        (if (= current word2)
          (do
            (reset! it curry)
            (reset! doit false)) ;; IF FOUND! WE DID IT! WE ARE DONEZO!
          (do
            (swap! to-search rest)
            (if (and  (pos? (count nexts)) (= -1 (.indexOf @visited current))  )
              (do
                (swap! to-search concat (map #(conj curry %)  nexts))
                (swap! visited conj current))
              )))))
    @it))
