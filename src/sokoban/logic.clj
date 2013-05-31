(ns sokoban.logic
  (:require [clojure.set :as css]
            [clojure.java.io :as io]))

(defrecord Game [world input continue ui levels selected-level])
(defrecord World [walls zombies statues player level-name])

; TODO handle player similar to other entities as a set
(defn new-world
  ([] (World. #{} #{} #{} [] nil))
  ([walls zombies statues player level-name] (World. walls zombies statues player level-name)))

(defn new-game [levels]
  (Game. nil nil [true] :starting levels 0))

(defn dir-to-offset
  "Take direction and return offset in the following coordinate system:
   .--→ x axis
   |
   ↓
  y axis"
  [dir]
  (case dir
              :n [0 -1]
    :w [-1 0]           :e [1 0]
              :s [0 1]
  ))

(defn get-bounds [world]
  (let [max-x (apply max (map first (:walls world)))
        max-y (apply max (map second (:walls world)))]
    [max-x max-y]))

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
       (assoc :player next-pos))))

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
      (= 0 (count statues-on-walls))
      (= (count statues) (count zombies)) ; See if any statues share space
      )))

(defn move-player [world dir]
  (let [next-state (get-next-state world dir)]
    (if (is-state-valid? next-state)
      next-state
      world
    )))
