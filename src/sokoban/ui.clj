(ns sokoban.ui
  (:require [lanterna.screen :as s]
            [clojure.string :as string]
            [sokoban.logic :as logic]
            [sokoban.levels :as levels]))

(def offset [10 10])

; Helper methods
(defn put-string [screen coords offset text]
  (let [[x y] coords
        [ox oy] offset]
    (s/put-string screen (+ x ox) (+ y oy) text)))

; Note the juxtaposition - lanterna uses [y x] positions
; and levels are presented in [x y] format
(defn draw-type [screen type-symbol coordinates [off-x off-y] color]
  (doseq [[x y] coordinates]
    (s/put-string screen (+ y off-y) (+ x off-x) type-symbol {:fg color})))

(defn draw-help [screen]
  (let [[cols rows] (s/get-size screen)
        last-row (dec rows)
        last-last-row (dec last-row)
        help-message "Use h/j/k/l to move up/left/right/down, r to restart level, q for level selection, ESC to exit"
        tut-message "Move yourself (@) to push statues ($) onto zombies (z) to stay alive!"
        help-len (count help-message)
        col-pos (int (- (/ cols 2) (/ help-len 2)))]
    (s/put-string screen col-pos last-last-row help-message {:fg :grey})
    (s/put-string screen col-pos last-row tut-message {:fg :grey})))

(defn get-screen-center [screen]
  (let [[cols rows] (s/get-size screen)
        x (int (/ cols 2))
        y (int (/ rows 2))]
    [x y]))

; Note the juxtaposition - lanterna uses [y x] positions
; and levels are presented in [x y] format
(defn get-level-offset [screen-center world-center]
  (let [[b-x b-y] world-center
        [c-x c-y] screen-center]
    [(- c-y (int (/ b-y 2))) (- c-x (int (/ b-x 2))) ]))

; Actual UI drawing
(defmulti draw-ui
  (fn [screen game]
    (:ui game)))

(defmethod draw-ui :starting [screen game]
  (let [{:keys [selected-level]} game]
    (s/clear screen)
    (put-string screen [0 0] offset "Level selection:")
    (doseq [[idx level] (map-indexed vector (:levels game))]
      (if (= idx selected-level)
        (put-string screen [0 (inc idx)] offset (string/join " " ["*" idx (:level-name level)]))
        (put-string screen [0 (inc idx)] offset (string/join " " [" " idx (:level-name level)]))))
    (s/redraw screen)))

(defmethod draw-ui :victory [screen game]
  ; TODO add proper victory screen
  (s/clear screen)
  (s/put-string screen 10 10 "Victory!")
  (s/redraw screen))

(defmethod draw-ui :playing [screen game]
  (let [world (:world game)
        player (:player world)
        player-pos [[(nth player 0) (nth player 1)]]
        matched-statues (logic/matched-statues world)
        level-offset (get-level-offset (get-screen-center screen) (logic/get-bounds world))]
    (s/clear screen)
    (draw-type screen "#" (:walls world) level-offset :grey)
    (draw-type screen "z" (:zombies world) level-offset :red)
    (draw-type screen "$" (:statues world) level-offset :green)
    (draw-type screen "*" matched-statues level-offset :blue)
    (draw-type screen "@" player-pos level-offset :yellow)
    (draw-help screen)
    (s/redraw screen)))

; Input processing
(defn try-select-level [next-level game]
  (let [min-level 0
        max-level (count (:levels game))]
    (if (<= min-level next-level (- max-level 1))
      (assoc game :selected-level next-level)
      game)))

(defn start-game [game]
  (let [{:keys [levels selected-level]} game]
    (-> game
      (assoc :world (nth levels selected-level))
      (assoc :ui :playing))))

(defmulti process-input
  (fn [game input]
    (:ui game)))

(defmethod process-input :starting [game input]
  (let [selected-level (:selected-level game)]
    (case input
      \j (try-select-level (inc selected-level) game)
      \k (try-select-level (dec selected-level) game)
      :escape (assoc game :continue [])
      :enter (start-game game)
      game)))

(defmethod process-input :victory [game input]
  (case input
    :escape (assoc game :continue [])
    (-> game
      (assoc :ui :starting)
      (dissoc :world))))

(defmethod process-input :playing [game input]
  ; TODO add "get to level selection screen" button
  (let [ui (:ui game)
        world (:world game)
        ; logic/win? check should occur after we process current input
        win (logic/win? world)]
    (if win
      (assoc game :ui :victory)
      (case input
        :escape (assoc game :continue [])
        \h (update-in game [:world] logic/move-player :w)
        \j (update-in game [:world] logic/move-player :s)
        \k (update-in game [:world] logic/move-player :n)
        \l (update-in game [:world] logic/move-player :e)
        \r (assoc game :world (nth (:levels game) (:selected-level game)))
        \q (assoc game :ui :starting)
        game
        ))))

; Main loop
(defn get-input [game screen]
  (assoc game :input (s/get-key-blocking screen)))

(defn run-game [game screen]
  (draw-ui screen game)
  (loop [{:keys [continue] :as game} game]
    (when-not (empty? continue)
      (let [input (s/get-key-blocking screen)
            new-game (process-input game input)]
        (draw-ui screen new-game)
        (recur new-game)))))

(defn main
  ([screen-type] (main screen-type false))
  ([screen-type block?]
   (letfn [(go []
             (let [screen (s/get-screen screen-type)]
               (s/in-screen screen
                            (run-game (logic/new-game (levels/get-all-levels)) screen))))]
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

