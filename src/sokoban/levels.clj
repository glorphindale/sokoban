(ns sokoban.levels
  (:require [clojure.string :as string]
            [sokoban.logic :as logic]))

(defn index [world-lines]
  (apply concat
    (for [row (map-indexed vector (string/split-lines world-lines))]
      (let [[idx line] row]
        (map-indexed #(vec [idx %1 %2]) line)))))

(defn transform-type [indexed-level ctype]
    (->> indexed-level
        (filter (fn [[_ _ c]] (= ctype c)))
        (map (fn [[x y _]] [x y]))))

(defn parse-level [level-str level-name]
  (let [indexed-str (index level-str)
        walls (transform-type indexed-str \#)
        zombies (transform-type indexed-str \.)
        statues (transform-type indexed-str \o)
        player (transform-type indexed-str \@)]
    ; TODO add parsing of "statue on a zombie" case
    (logic/new-world (set walls) (set zombies) (set statues) (apply concat player) level-name)))

(def test-level
  "######\n#@   #\n#  oo#\n#  ..#\n######")

(def default-levels
  [(parse-level test-level "Test level")
   (parse-level "  ##### \n###   # \n#.@o  # \n### o.# \n#.##o # \n# # . ##\n#o *oo.#\n#   .  #\n########" "Wikipedia sample")])
                
