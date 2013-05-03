(ns sokoban.ui
    (:require [lanterna.screen :as s]
              [sokoban.logic :as logic]))

(defmulti draw-ui
  (fn [screen game]
    (:ui game)))

(defmethod draw-ui :starting [screen game]
  ; TODO add proper selection screen
  (s/clear screen)
  (s/put-string screen 0 0 "Level selection")
  (s/redraw screen))

(defmethod draw-ui :victory [screen game]
  ; TODO add proper victory screen
  (s/clear screen)
  (s/put-string screen 10 10 "Victory!")
  (s/redraw screen))

(defmethod draw-ui :playing [screen game]
  (let [world (:world game)
        player (:player world)]
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
          (s/redraw screen)))

(defmulti process-input
  (fn [game input]
    (:ui game)))

(defmethod process-input :starting [game input]
  ;; TODO add proper selection controls
  (case input
    :escape (assoc game :continue [])
    (assoc game :ui :playing)))

(defmethod process-input :victory [game input]
  (case input
    :escape (assoc game :continue [])
    (assoc game :ui :starting)))

(defmethod process-input :playing [game input]
  (let [ui (:ui game)
        world (:world game)
        win (logic/win? world)]
    (if win
      (assoc game :ui :victory)
      (case input
        :escape (assoc game :continue [])
        \h (update-in game [:world] logic/move-player :w)
        \j (update-in game [:world] logic/move-player :s)
        \k (update-in game [:world] logic/move-player :n)
        \l (update-in game [:world] logic/move-player :e)
        game
      ))))

(defn get-input [game screen]
  (assoc game :input (s/get-key-blocking screen)))

(defn run-game [game screen]
  (loop [{:keys [input continue] :as game} game]
    (when-not (empty? continue)
      (draw-ui screen game)
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

