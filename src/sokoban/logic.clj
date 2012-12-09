(ns sokoban.logic)

(defrecord Game [world input continue player-pos])

(defn new-world []
  ["#####" "#...#" "#...#" "#.sz#" "#####"])

(defn new-game []
  (new Game (new-world) nil [true] [1 1]))

(defn dir-to-offset [dir]
  (case dir
    :w [-1 0]
    :e [1 0]
    :n [0 -1]
    :s [0 1]))
 
(defn offset-coords [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn get-dest [world [x y]]
  (if (or (< x 0) (< y 0))
    \#
    (nth (nth world x) y)))

(defn can-move? [world player-pos dir]
  (let [dest-pos (offset-coords (dir-to-offset dir) player-pos)
        dest-content (get-dest world dest-pos)]
    (case dest-content
      \# false
      \s true
      \. true
      false)))
  
(defn move-player [player-pos world dir]
  (let [dest-pos (offset-coords (dir-to-offset dir) player-pos)]
    (if (can-move? world player-pos dir)
      dest-pos
      player-pos)))
