(ns sokoban.levels
  (:require [clojure.string :as string]
            [sokoban.logic :as logic]
            [clojure.java.io :as io]))

(defn index [world-lines]
  (apply concat
    (for [row (map-indexed vector (string/split-lines world-lines))]
      (let [[idx line] row]
        (map-indexed #(vec [idx %1 %2]) line)))))

(defn transform-type [indexed-level ctypes]
    (->> indexed-level
        (filter (fn [[_ _ c]] (contains? ctypes c)))
        (map (fn [[x y _]] [x y]))))

(defn parse-level [level-str level-name]
  (let [indexed-str (index level-str)
        walls (transform-type indexed-str #{\#})
        zombies (transform-type indexed-str #{\. \*})
        statues (transform-type indexed-str #{\$ \*})
        player (transform-type indexed-str #{\@}) ]
    (logic/new-world (set walls) (set zombies) (set statues) (apply concat player) level-name)))

(def test-level
  "######\n#@   #\n#  $$#\n# *..#\n######")

(def default-levels
  [(parse-level test-level "Test level")
   (parse-level "  ##### \n###   # \n#.@$  # \n### $.# \n#.##$ # \n# # . ##\n#$ *$$.#\n#   .  #\n########" "Wikipedia sample")
   ])

(defn get-level-files [dir-name ext]
  (filter #(-> (io/file %) .getName (.endsWith ext))
          (file-seq (io/file dir-name))))

; Collection parsing
(defn clean-file [file]
  (let [content (slurp file)]
    (filter not-empty (clojure.string/split content #"\r\n"))))

(defn get-parts [content]
  (partition-by
     #(.startsWith % ";") content))

(defn levels-from-collection [filename]
  (let [parts (-> filename clean-file get-parts)
        filtered-parts (filter #(not (.startsWith (nth % 0) ";")) parts)
        raw-levels (map #(clojure.string/join "\r\n" %) filtered-parts)]
    (map-indexed
       (fn [idx item] (parse-level item (str "(" filename ") level " (inc idx))))
       raw-levels)))
; /Collection parsing

(defn get-collection-levels []
  (apply concat
    (map levels-from-collection (get-level-files "./levels" ".slc"))))

(defn get-fs-levels
  ([] (get-fs-levels "./levels"))
  ([dir-name] (map #(-> % slurp (parse-level (.getName %)))
                   (get-level-files dir-name ".lvl"))))


(defn get-all-levels []
  (concat default-levels (get-fs-levels) (get-collection-levels)))
