(ns sokoban.levels
  (:require [clojure.string :as string]
            [sokoban.rules :as rules]
            [clojure.java.io :as io]))

(defn index
  "Enumerate every character in the level with its [x y] position. Return [x y character] sequence."
  [world-lines]
  (apply concat
    (for [[row line] (map-indexed vector (string/split-lines world-lines))]
        (map-indexed #(vec [%1 row %2]) line))))

(defn transform-type
  "indexed-level is a sequence of [x y type] entries, ctypes is a set of types.
    Return positions of entries that match specified types."
  [indexed-level ctypes]
    (->> indexed-level
        (filter (fn [[_ _ c]] (contains? ctypes c)))
        (map (fn [[x y _]] [x y]))))

(defn parse-level [level-str level-name]
  "Take textual represenation of sokoban level and return World filled with entities."
  (let [indexed-str (index level-str)
        walls (transform-type indexed-str #{\#})
        zombies (transform-type indexed-str #{\. \* \+})
        statues (transform-type indexed-str #{\$ \*})
        player (transform-type indexed-str #{\@ \+}) ]
    (rules/new-world (set walls) (set zombies) (set statues) (set player) level-name)))

(def test-level
  "######\n#+   #\n#$  $#\n#  *.#\n######")

(def default-levels
  [(parse-level test-level "Test level")
   (parse-level "  ##### \n###   # \n#.@$  # \n### $.# \n#.##$ # \n# # . ##\n#$ *$$.#\n#   .  #\n########" "Wikipedia sample")
   ])

(defn files-by-ext [dir-name ext]
  (filter #(-> % .getName (.endsWith ext))
          (file-seq (io/file dir-name))))

; Collection parsing
(defn clean-file [file]
  (let [content (slurp file)]
    (filter not-empty (clojure.string/split content #"\r\n"))))

(defn get-parts [content]
  (->> content
       (partition-by #(.startsWith % ";"))
       (filter #(not (.startsWith (first %) ";")))))

(defn get-level-name [coll-path idx]
  (let [[_ coll-name] (re-find #".+\\(.+).slc" (str coll-path))]
    (str "(" coll-name ") level " idx)))

(defn levels-from-collection [filename]
  (let [parts (-> filename clean-file get-parts)
        raw-levels (map #(clojure.string/join "\r\n" %) parts)]
    (map-indexed
       (fn [idx level] (parse-level level (get-level-name filename (inc idx))))
       raw-levels)))
; /Collection parsing

(defn get-collection-levels []
  (apply concat
    (map levels-from-collection (files-by-ext "./levels" ".slc"))))

(defn get-fs-levels
  ([] (get-fs-levels "./levels"))
  ([dir-name] (map #(-> % slurp (parse-level (.getName %)))
                   (files-by-ext dir-name ".lvl"))))

(defn get-all-levels []
  (concat default-levels (get-fs-levels) (get-collection-levels)))