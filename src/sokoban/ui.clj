(ns sokoban.ui
  (:require [lanterna.screen :as s]
            [clojure.string :as string]
            [sokoban.logic :as logic]
            [sokoban.levels :as levels]))

(def levels-per-page 10)

(defrecord Game [world input continue ui levels selected-level])
(defn new-game [levels]
  (Game. nil nil [true] :selection levels 0))

; Helper methods
(defn put-string [screen coords offset text]
  (let [[x y] coords
        [ox oy] offset]
    (s/put-string screen (+ x ox) (+ y oy) text)))

(defn guess-box [text-lines]
  (let [max-y (count text-lines)
        max-x (apply max (map #(.length %) text-lines))]
    [max-x max-y]))

(defn box-center [[max-x max-y]]
  (let [center-x (/ max-x 2)
        center-y (/ max-y 2)]
    (map int [center-x center-y])))

(defn get-text-offset [screen-center text-center]
  (let [[s-x s-y] screen-center
        [t-x t-y] text-center]
    [(- s-x t-x) (- s-y t-y)]))

(defn prepare-selection-text [game]
  (let [{:keys [selected-level levels]} game
        total-levels (count levels)
        current-page (quot selected-level levels-per-page)
        total-pages (inc (quot total-levels levels-per-page))
        selected-level-offset (rem selected-level levels-per-page)
        skip-levels-before (* current-page levels-per-page)
        skip-levels-after (min (+ skip-levels-before levels-per-page) (count (:levels game)))
        indexed-levels (vec (map-indexed vector (:levels game)))
        levels-to-show (subvec indexed-levels skip-levels-before skip-levels-after)]
    (concat ["Level selection:"]
      (map (fn [[idx level]] (string/join " " ["[" idx "]" (:level-name level)])) levels-to-show)
      [(str "page " (inc current-page) "/" total-pages)])
    ))

; Actual UI drawing
(defn draw-symbol [screen type-symbol coordinates [off-x off-y] color]
  (doseq [[x y] coordinates]
    (s/put-string screen (+ x off-x) (+ y off-y) type-symbol {:fg color})))

(defn draw-help [screen messages]
  (doseq [[idx msg] (map-indexed vector messages)]
    (let [[cols rows] (s/get-size screen)
          row (+ (- rows (count messages)) idx)
          msg-len (count msg)
          col-pos (int (- (/ cols 2) (/ msg-len 2)))]
      (s/put-string screen col-pos row msg {:fg :grey}))))

; There are three screens - level selection, game and victory.
(defmulti draw-ui
  (fn [screen game]
    (:ui game)))

(defmethod draw-ui :selection [screen game]
  (let [{:keys [selected-level levels]} game
        levels-text (prepare-selection-text game)
        lines (map-indexed vector levels-text)
        selected-level-offset (rem selected-level levels-per-page)
        text-center (box-center (guess-box levels-text))
        screen-center (box-center (s/get-size screen))
        offset (get-text-offset screen-center text-center)
        [_ rows] (s/get-size screen)
        last-row (dec rows)]
    (s/clear screen)
    (doseq [[line text] lines]
      (put-string screen [0 line] offset text))
    (s/move-cursor screen (+ 2 (first offset)) (+ (inc selected-level-offset) (second offset)))
    (draw-help screen ["Use j/k/PageUp/PageDown to select a level, Enter to play, ESC to exit"])
    (s/redraw screen)))

(defmethod draw-ui :victory [screen game]
  (let [lines ["Victory!" "Press any key to return to the level selection screen"]
        text-center (box-center (guess-box lines))
        screen-center (box-center (s/get-size screen))
        offset (get-text-offset screen-center text-center)]
    ; TODO add proper victory screen
    (s/clear screen)
    (doseq [[line text] (map-indexed vector lines)]
        (put-string screen [0 line] offset text))
    (s/redraw screen)))

;help-message
;tut-message

(defmethod draw-ui :playing [screen game]
  (let [world (:world game)
        player (:player world)
        matched-statues (logic/matched-statues world)
        level-center (box-center (logic/get-bounds world))
        screen-center (box-center (s/get-size screen))
        offset (get-text-offset screen-center level-center)]
    (s/clear screen)
    (s/put-string screen 0 0 (str "Level: " (:level-name world)))
    (draw-symbol screen "#" (:walls world) offset :grey)
    (draw-symbol screen "z" (:zombies world) offset :red)
    (draw-symbol screen "$" (:statues world) offset :green)
    (draw-symbol screen "*" matched-statues offset :blue)
    (draw-symbol screen "@" player offset :yellow)
    (draw-help screen ["Use h/j/k/l to move up/left/right/down, r to restart level, q for level selection, ESC to exit"
                       "Move yourself (@) to push statues ($) onto zombies (z) to stay alive!"])
    (s/move-cursor screen 0 1)
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

(defmethod process-input :selection [game input]
  (let [selected-level (:selected-level game)]
    (case input
      \j (try-select-level (inc selected-level) game)
      \k (try-select-level (dec selected-level) game)
      :page-down (try-select-level (+ selected-level levels-per-page) game)
      :page-up (try-select-level (- selected-level levels-per-page) game)
      :escape (assoc game :continue [])
      :enter (start-game game)
      game)))

(defmethod process-input :victory [game input]
  (case input
    :escape (assoc game :continue [])
    (-> game
      (assoc :ui :selection)
      (dissoc :world))))

(defmethod process-input :playing [game input]
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
        \q (assoc game :ui :selection)
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
                            (run-game (new-game (levels/get-all-levels)) screen))))]
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

