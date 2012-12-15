(ns sokoban.ui
    (:require [lanterna.screen :as s]
              [sokoban.logic :as logic]))

(defn draw-game [screen game]
  (let [world (:world game)
        player (:player world)
        win (logic/win? world)]
          (s/clear screen)
          (doseq [[x y] (:walls world)]
            (s/put-string screen y x "#"))
          (doseq [[x y] (:zombies world)]
            (s/put-string screen y x "z"))
          (doseq [[x y] (:statues world)]
            (s/put-string screen y x "o"))
          (doseq [[x y] (logic/matched-statues world)]
            (s/put-string screen y x "*"))
          (s/put-string screen (nth player 1) (nth player 0) "@")
          (if win
            (s/put-string screen 10 10 "Victory"))
          (s/redraw screen)))

(defn get-input [game screen]
  (assoc game :input (s/get-key-blocking screen)))

(defn process-input [game input]
  (let [world (:world game)]
  (case input
    :escape (assoc game :continue [])
    \h (update-in game [:world] logic/move-player :w)
    \j (update-in game [:world] logic/move-player :s)
    \k (update-in game [:world] logic/move-player :n)
    \l (update-in game [:world] logic/move-player :e)
    game
  )))

(defn run-game [game screen]
  (loop [{:keys [input continue] :as game} game]
    (when-not (empty? continue)
      (draw-game screen game)
      (if (nil? input)
        (recur (get-input game screen))
        (recur (process-input (dissoc game :input) input))))))

(defn main 
  ([screen-type] (main screen-type false))
  ([screen-type block?]
    (letfn [(go []
              (let [screen (s/get-screen screen-type)]
                (s/in-screen screen
                             (run-game (logic/new-game) screen))))]
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

