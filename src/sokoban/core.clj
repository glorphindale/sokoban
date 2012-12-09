(ns sokoban.core
    (:require [lanterna.screen :as s]))

(defrecord Game [world input continue player-pos])

(defn draw-game [screen {:keys [world player-pos] :as game}]
  (s/clear screen)
  (doseq [line-idx (range 0 (count world))
          :let [line (nth world line-idx)
                screen-line-idx (+ 0 line-idx)]]
    (s/put-string screen 0 screen-line-idx line))
  (s/put-string screen (nth player-pos 0) (nth player-pos 1) "@")
  (s/redraw screen))

(defn get-input [game screen]
  (assoc game :input (s/get-key-blocking screen)))

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
      [new-x new-y])
  ))

(defn process-input [game input]
  (let [world (:world game)]
  (case input
    :escape (assoc game :continue [])
    \h (update-in game [:player-pos] move-player world :w)
    \j (update-in game [:player-pos] move-player world :s)
    \k (update-in game [:player-pos] move-player world :n)
    \l (update-in game [:player-pos] move-player world :e)
    game
  )))

(defn run-game [game screen]
  (loop [{:keys [input continue] :as game} game]
    (when-not (empty? continue)
      (draw-game screen game)
      (if (nil? input)
        (recur (get-input game screen))
        (recur (process-input (dissoc game :input) input))))))

(defn new-world []
  ["#####" "#...#" "#...#" "#...#" "#####"])

(defn new-game []
  (new Game (new-world) nil [true] [1 1]))

(defn main 
  ([screen-type] (main screen-type false))
  ([screen-type block?]
    (letfn [(go []
              (let [screen (s/get-screen screen-type)]
                (s/in-screen screen
                              (run-game (new-game) screen))))]
    (if block?
      (go)
      (future (go))))))

(defn -main [& args]
  (let [args (set args)
        screen-type (cond
                      (args ":swing") :swing
                      (args ":text")  :text
                      :else           :auto)]
    (main screen-type true)))

