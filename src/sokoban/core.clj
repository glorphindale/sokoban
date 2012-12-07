(ns sokoban.core
    (:require [lanterna.screen :as s]))

(defrecord UI [kind])
(defrecord World [level])
(defrecord Game [world uis input])

(defmulti draw-ui
  (fn [ui game screen]
    (:kind ui)))

(defmethod draw-ui :start [ui game screen]
  (s/put-string screen 0 0 "Welcome to the Sokoban!")
  (s/put-string screen 0 1 "Press enter to start the game, anything else to lose."))

(defmethod draw-ui :win [ui game screen]
  (s/put-string screen 0 0 "Congratulations, you win!")
  (s/put-string screen 0 1 "Press escape to exit, anything else to restart."))

(defmethod draw-ui :lose [ui game screen]
  (s/put-string screen 0 0 "Sorry, better luck next time.")
  (s/put-string screen 0 1 "Press escape to exit, anything else to go."))

(defn draw-world [screen world]
  (doseq [line-idx (range 0 (count world))
          :let [line (nth world line-idx)
                screen-line-idx (+ 1 line-idx)]]
  	(s/put-string screen 0 screen-line-idx line)))

(defmethod draw-ui :game [ui game screen]
  (s/put-string screen 0 0 "Press escape to exit")
  (draw-world screen (->> game :world :level)))

(defn draw-game [game screen]
  (s/clear screen)
  (doseq [ui (:uis game)]
    (draw-ui ui game screen))
  (s/redraw screen))

(defmulti process-input
  (fn [game input]
    (:kind (last (:uis game)))))

(defmethod process-input :start [game input]
  (if (= input :enter)
    (assoc game :uis [(new UI :game)])
    (assoc game :uis [(new UI :lose)])))

(defmethod process-input :win [game input]
  (if (= input :escape)
    (assoc game :uis [])
    (assoc game :uis [(new UI :start)])))

(defmethod process-input :lose [game input]
  (if (= input :escape)
    (assoc game :uis [])
    (assoc game :uis [(new UI :start)])))

(defmethod process-input :game [game input]
  (if (= input :escape)
    (assoc game :uis [])))

(defn get-input [game screen]
  (assoc game :input (s/get-key-blocking screen)))

(defn run-game [game screen]
  (loop [{:keys [input uis] :as game} game]
    (when-not (empty? uis)
      (draw-game game screen)
      (if (nil? input)
        (recur (get-input game screen))
        (recur (process-input (dissoc game :input) input))))))

(defn new-world []
  ["#####" "#...#" "#...#" "#...#" "#####"])

(defn new-game []
  (new Game
       (new World (new-world))
       [(new UI :start)]
       nil))

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

