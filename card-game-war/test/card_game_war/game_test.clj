(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))


;; fill in  tests for your game
(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (is (= (play-round [:spade 2] [:spade 1]) 0)))
  (testing "queens are higher rank than jacks"
    (is (= (play-round [:heart :queen] [:club :jack]) 0)))
  (testing "kings are higher rank than queens"
    (is (= (play-round [:heart :queen] [:club :king]) 1)))
  (testing "aces are higher rank than kings"
    (is (= (play-round [:heart :ace] [:club :king]) 0)))
  (testing "if the ranks are equal, clubs beat spades"
    (is (= (play-round [:spade :jack] [:club :jack]) 1)))
  (testing "if the ranks are equal, diamonds beat clubs"
    (is (= (play-round [:diamond 2] [:club 2]) 0)))
  (testing "if the ranks are equal, hearts beat diamonds"
    (is (= (play-round [:heart :queen] [:diamond :queen]) 0))))

(deftest test-play-game
  (testing "the player loses when they run out of cards"
    (is (=  1 (play-game (take 26 cards) (drop 26 cards))))))
