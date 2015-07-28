(ns doublets.solver-test
  (:require [clojure.test :refer :all]
            [doublets.solver :refer :all]))

(deftest solver-test
  (testing "one word away helper"
    (is (= true (is-one-letter-away "head" "heal")))
    (is (= false (is-one-letter-away "heed" "heal")))
    )


  (testing "with word links found"
    (is (= ["head" "heal" "teal" "tell" "tall" "tail"]
           (doublets "head" "tail")))

    (is (= ["door" "boor" "book" "look" "lock"]
           (doublets "door" "lock")))

    (is (= ["bank" "bonk" "book" "look" "loon" "loan"]
           (doublets "bank" "loan")))

    (is (= ["wheat" "cheat" "cheap" "cheep" "creep" "creed" "breed" "bread"]
           (doublets "wheat" "bread"))))

  (testing "with no word links found"
    (is (= []
           (doublets "ye" "freezer")))))
