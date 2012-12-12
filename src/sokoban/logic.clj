(ns sokoban.logic)

(defrecord Game [world input continue])
(defrecord World [walls zombies statues player])

(defn new-world []
  (World.
       []
       []
       []
       []))

(defn parse-level [_]
  (World.
       [[0 0] [0 1] [0 2]
        [1 0]       [1 2]
        [2 0]       [2 2] [2 3]
        [3 0]             [3 3]
        [4 0] [4 1] [4 2] [4 3]]
       []
       []
       [1 1]))

(defn new-game []
  (Game. (parse-level nil) nil [true]))

(defn dir-to-offset [dir]
  (case dir
    :w [0 -1]
    :e [0 1]
    :n [-1 0]
    :s [1 0]))
 
(defn offset-coords [[x y] dir]
  (let [[dx dy] (dir-to-offset dir)]
    [(+ x dx) (+ y dy)]))

(defn get-dest [world [x y]]
  (if (or (< x 0) (< y 0))
    \#
    ([x y] (:walls world))))

(defn can-player-move? [world dir]
  (let [walls (set (:walls world))
        player-pos (:player world)
        dest-pos (offset-coords player-pos dir)
        is-wall? (contains? walls dest-pos)]
    (not is-wall?)
    ))
  
(defn move-player [player-pos world dir]
  (let [dest-pos (offset-coords player-pos dir)]
    (if (can-player-move? world dir)
      dest-pos
      player-pos)))
