(ns sokoban.logic
  (:require [clojure.set :as cs]))

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
       #{[0 0] [0 1] [0 2]
        [1 0]       [1 2]
        [2 0]       [2 2] [2 3] [2 4] [2 5]
        [3 0]                         [3 5]
        [4 0] [4 1] [4 2] [4 3] [4 4] [4 5]}
       #{[3 4]}
       #{[3 2]}
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

(defn is-state-valid? [world]
  (let [{:keys [walls player statues zombies]} world
        player-on-wall (cs/intersection walls #{player})
        statue-on-wall (cs/intersection walls statues)]
    (and
      (= 0 (count player-on-wall))
      (= 0 (count statue-on-wall)))
    ))
  
(defn move-player [world dir]
  (let [next-state (get-next-state world dir)]
    (if (is-state-valid? next-state)
      next-state
      world
    )))
