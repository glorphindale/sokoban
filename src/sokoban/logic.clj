(ns sokoban.logic
  (:require [clojure.set :as css]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(defrecord Game [world input continue])
(defrecord World [walls zombies statues player])

(defn new-world []
  (World. #{} #{} #{} []))

(defn index [world-lines]
  (apply concat
    (for [row (map-indexed vector (string/split-lines world-lines))]
      (let [[idx line] row]
        (map-indexed #(vec [idx %1 %2]) line)))))

(defn transform-type [indexed-level ctype]
    (->> indexed-level
        (filter (fn [[_ _ c]] (= ctype c)))
        (map (fn [[x y _]] [x y]))))

(defn parse-level [level-str]
  (let [indexed-str (index level-str)
        walls (transform-type indexed-str \#)
        zombies (transform-type indexed-str \.)
        statues (transform-type indexed-str \o)
        player (transform-type indexed-str \@)]
    (World. (set walls) (set zombies) (set statues) (apply concat player))))

(def test-level
  "######\n#@   #\n#  o #\n#   .#\n######")

(defn new-game []
  (Game. (parse-level test-level) nil [true]))

(defn dir-to-offset [dir]
  (case dir
    :w [0 -1]
    :e [0 1]
    :n [-1 0]
    :s [1 0]))
 
(defn offset-coords [[x y] dir]
  (let [[dx dy] (dir-to-offset dir)]
    [(+ x dx) (+ y dy)]))

(defn process-statues [statues next-pos next-next-pos]
  (if (contains? statues next-pos)
    (-> statues
      (disj next-pos)
      (conj next-next-pos))
    statues))

(defn get-next-state [world movement-dir]
  (let [{:keys [walls player statues zomibes]} world
        next-pos (offset-coords player movement-dir)
        next-next-pos (offset-coords next-pos movement-dir)]
    (-> world
       (update-in [:statues] process-statues next-pos next-next-pos)
       (assoc :player next-pos))
  ))

(defn matched-statues [world]
  (let [{:keys [zombies statues]} world]
    (css/intersection zombies statues)))

(defn win? [world]
  (let [zombies (:zombies world)
        statues-on-zombies (matched-statues world)]
        (= (count zombies) (count statues-on-zombies))))

(defn is-state-valid? [world]
  (let [{:keys [walls player statues zombies]} world
        player-on-wall (css/intersection walls #{player})
        statues-on-walls (css/intersection walls statues)]
    (and
      (= 0 (count player-on-wall))
      (= 0 (count statues-on-walls)))
    ))
  
(defn move-player [world dir]
  (let [next-state (get-next-state world dir)]
    (if (is-state-valid? next-state)
      next-state
      world
    )))
