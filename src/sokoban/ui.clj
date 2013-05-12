(ns sokoban.ui
  (:require [lanterna.screen :as s]
            [clojure.string :as string]
            [sokoban.logic :as logic]
            [sokoban.levels :as levels]))

(def offset [10 10])

(defmulti draw-ui
  (fn [screen game]
    (:ui game)))

(defn put-string [screen coords offset text]
  (let [[x y] coords
        [ox oy] offset]
    (s/put-string screen (+ x ox) (+ y oy) text)))

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
  ; TODO fix bug: game requires additional key press to advance here
  (s/clear screen)
  (s/put-string screen 10 10 "Victory!")
  (s/redraw screen))

(defn draw-type [screen type-symbol coordinates [off-x off-y] color]
  (doseq [[x y] coordinates]
    (s/put-string screen (+ y off-y) (+ x off-x) type-symbol {:fg color})))

(defmethod draw-ui :playing [screen game]
  (let [world (:world game)
        player (:player world)]
    (s/clear screen)
    (draw-type screen "#" (:walls world) offset :grey)
    (draw-type screen "z" (:zombies world) offset :green)
    (draw-type screen "$" (:statues world) offset :red)
    (draw-type screen "*" (logic/matched-statues world) offset :grey)
    (draw-type screen "@" [[(nth player 0) (nth player 1)]] offset :yellow)
    (s/redraw screen)))

(defmulti process-input
  (fn [game input]
    (:ui game)))

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

(defmethod process-input :starting [game input]
  (let [selected-level (:selected-level game)]
    (case input
      \j (try-select-level (+ selected-level 1) game)
      \k (try-select-level (- selected-level 1) game)
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

