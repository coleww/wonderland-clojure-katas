(ns tiny-maze.solver-test
  (:require [clojure.test :refer :all]
            [tiny-maze.solver :refer :all]))

(deftest test-solve-maze
  (testing "can find way to exit with 3x3 maze"
    (let [maze [[:S 0 1]
                [1  0 1]
                [1  0 :E]]
          sol [[:x :x 1]
               [1  :x 1]
               [1  :x :x]]]
      (is (= sol (solve-maze maze)))))

    (testing "can find way to exit with 4x4 maze"
    (let [maze [[:S 0 0 1]
                [1  1 0 0]
                [1  0  0 1]
                [1  1  0 :E]]
          sol [[:x :x :x 1]
                [1  1 :x 0]
                [1  0 :x 1]
                [1  1  :x :x]]]
     (is (= sol (solve-maze maze))))))

(deftest test-map-cell
  (testing "mapping a single cell"
    (is (= (map-cell :S 5 7) {:x 5 :y 7 :content :S :g 0 :h 0 :f 0 :parent nil}))))

(deftest test-map-row
  (testing "mapping a row"
    (is (= (map-row ["D" "O" "A"] 1) '({:x 0 :y 1 :content "D" :g 0 :h 0 :f 0 :parent nil} {:x 1 :y 1 :content "O" :g 0 :h 0 :f 0 :parent nil} {:x 2 :y 1 :content "A" :g 0 :h 0 :f 0 :parent nil})))))

(deftest test-map-maze
  (testing "mapping a row"
    (is (= (map-maze [["D" "O" "A"] [1 2 3]]) '(({:x 0 :y 0 :content "D" :g 0 :h 0 :f 0 :parent nil} {:x 1 :y 0 :content "O" :g 0 :h 0 :f 0 :parent nil} {:x 2 :y 0 :content "A" :g 0 :h 0 :f 0 :parent nil})({:x 0 :y 1 :content 1 :g 0 :h 0 :f 0 :parent nil} {:x 1 :y 1 :content 2 :g 0 :h 0 :f 0 :parent nil} {:x 2 :y 1 :content 3 :g 0 :h 0 :f 0 :parent nil}))))))

(deftest test-get-neighbours
  (testing "getting neighbours"
    (is (= (get-neighbours {:x 1 :y 1} (map-maze [["D" "O" "A"] [1 2 3] [4 5 6]]))    '({:x 2, :y 2, :content 6, :g 0, :h 0, :f 0, :parent nil} {:x 1, :y 2, :content 5, :g 0, :h 0, :f 0, :parent nil} {:x 1, :y 0, :content "O", :g 0, :h 0, :f 0, :parent nil} {:x 2, :y 1, :content 3, :g 0, :h 0, :f 0, :parent nil} {:x 0, :y 1, :content 1, :g 0, :h 0, :f 0, :parent nil} {:x 2, :y 0, :content "A", :g 0, :h 0, :f 0, :parent nil} {:x 0, :y 2, :content 4, :g 0, :h 0, :f 0, :parent nil} {:x 0, :y 0, :content "D", :g 0, :h 0, :f 0, :parent nil})))))


(deftest test-get-g-cost
  (testing "getting Gs"
    (is (= (get-g-cost {:x 0 :y 0 :g 100} {:x 1 :y 1})  114))
    (is (= (get-g-cost {:x 0 :y 0 :g 100} {:x 0 :y 1})  110))))

(deftest test-get-h-cost
  (testing "getting Hs"
    (is (= (get-h-cost {:x 0 :y 0} {:x 10 :y 10}) 200))))

(deftest test-is-in-list
  (testing "checking if item is in a list"
    (is (= (is-in-list [{:x 1 :y 1}] {:x 1 :y 1}) true))
    (is (= (is-in-list [{:x 1 :y 0}] {:x 1 :y 1}) nil))))

(deftest test-findit
  (testing "finding a thing in the stuff"
    (is (= (findit (map-maze [["D" "O" "A"] [1 2 3]]) 1) {:x 0 :y 1 :content 1 :g 0 :h 0 :f 0 :parent nil}))))
