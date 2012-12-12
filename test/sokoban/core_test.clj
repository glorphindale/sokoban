(ns sokoban.core-test
  (:use clojure.test)
  (:import [sokoban.logic World Game]))

(use '[sokoban.logic :as sl])

(defn test-world []
  (World.
       #{[0 0] [0 1] [0 2]
        [1 0]       [1 2] 
        [2 0]       [2 2]
        [3 0]       [3 2]
        [4 0] [4 1] [4 2]}
       []
       []
       [1 1]))

(deftest basic
  (testing "Basic stuff"
           (is (= [1 0] (sl/offset-coords [2 0] :n)))
  ))

(deftest test-start
  (let [world (test-world)]

    (testing "World existence"
        (is (not (empty? (:walls world)))))
    (testing "Movement"
        (is (sl/can-player-move? world :s))
        (is (not (sl/can-player-move? world :n))))


    ))

