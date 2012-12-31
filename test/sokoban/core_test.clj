(ns sokoban.core-test
  (:use clojure.test)
  (:import [sokoban.logic World Game]))

(use '[sokoban.logic :as sl])

(def test-world
  (World.
       #{ [0 0] [0 1] [0 2] [0 3] [0 4] [0 5]
          [1 0]                         [1 5] 
          [2 0]                         [2 5]
          [3 0]                         [3 5]
          [4 0] [4 1] [4 2] [4 3] [4 4] [4 5]}
       #{[3 3]}
       #{[2 2]}
       [1 1]))

(def test-level
  "######\n#@   #\n#  o #\n#   . #\n######")

(deftest basic
  (testing "Basic stuff"
           (is (= [1 0] (sl/offset-coords [2 0] :n)))
  ))

(deftest parsing
  (testing "Parsing"
    (is (= (World. #{[0 0]} #{[0 1]} #{[0 2]} [0 3])
           (sl/parse-level "#.o@")))
    (is (= test-world (sl/parse-level test-level)))
  ))

