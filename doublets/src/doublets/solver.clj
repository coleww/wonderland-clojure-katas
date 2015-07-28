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

(defn doublets [word1 word2]
  (let [nexts (get-possibilities word1 words)]
    (if (= 0 (count nexts))
      nil
      (if (= -1 (.indexOf nexts word2))
        ;; not reached w2 yet, recurse
        (conj nil (doublets (nth nexts 0)  word2) word1)
        word2
        ))
    )


  )
