(ns sokoban.logic)

(defrecord Game [world input continue player-pos])

(defn new-world []
  ["#####" "#...#" "#...#" "#...#" "#####"])

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

(defn move-player [player-pos world dir]
  (let [[new-x new-y] (offset-coords (dir-to-offset dir) player-pos)
        dest-content (nth (nth world new-y) new-x)]
    (if (= dest-content \#)
      player-pos
      [new-x new-y])))
