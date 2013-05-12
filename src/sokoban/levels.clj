(ns sokoban.levels
  (:require [clojure.string :as string]
            [sokoban.logic :as logic]
            [clojure.java.io :as io]))

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
        statues (transform-type indexed-str \$)
        player (transform-type indexed-str \@)]
    ; TODO add parsing of "statue on a zombie" case
    (logic/new-world (set walls) (set zombies) (set statues) (apply concat player) level-name)))

(def test-level
  "######\n#@   #\n#  $$#\n#  ..#\n######")

(def default-levels
  [(parse-level test-level "Test level")
   (parse-level "  ##### \n###   # \n#.@$  # \n### $.# \n#.##$ # \n# # . ##\n#$ *$$.#\n#   .  #\n########" "Wikipedia sample")])

(defn get-level-files [dir-name]
  (filter #(-> (io/file %) .getName (.endsWith "lvl"))
          (file-seq (io/file dir-name))))

(defn get-fs-levels
  ([] (get-fs-levels "./levels"))
  ([dir-name] (map #(-> % slurp (parse-level (.getName %)))
                   (get-level-files dir-name))))

(defn get-all-levels []
  (concat default-levels (get-fs-levels)))