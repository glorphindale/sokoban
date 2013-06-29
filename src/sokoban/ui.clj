(ns sokoban.ui
  (:require [lanterna.screen :as s]
            [clojure.string :as string]
            [sokoban.rules :as rules]
            [sokoban.levels :as levels]
            [sokoban.game :as game]))

(def levels-per-page 10)

; Helper methods
(defn put-string
  ([screen coords offset text]
    (put-string screen coords offset text :white :black))
  ([screen coords offset text fg-color bg-color]
    (let [[x y] coords
          [ox oy] offset]
      (s/put-string screen (+ x ox) (+ y oy) text {:fg fg-color :bg bg-color}))))

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
(defn draw-text-centered
  ([screen lines center-each-line]
    (let [text-center (box-center (guess-box lines))
          screen-center (box-center (s/get-size screen))
          [o-x o-y] (get-text-offset screen-center text-center)
          [cols _] (s/get-size screen)]
      (s/clear screen)
      (doseq [[line text] (map-indexed vector lines)]
        (let [col-pos (int (- (/ cols 2) (/ (count text) 2)))]
          (if center-each-line
            (put-string screen [0 line] [col-pos o-y] text)
            (put-string screen [0 line] [o-x o-y] text))))))
  ([screen lines] (draw-text-centered screen lines true)))

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

; There are four screens - splash, level selection, game and victory.
(defmulti draw-ui
  (fn [screen game]
    (:ui game)))

(defmethod draw-ui :splash [screen game]
  (let [lines ["You wake up at the cemetery."
               "It's getting dark, and all of the tombstones are moved."
               "Zombies start to rise from their graves."
               "You must put tombstones back, or... Brainzzzzzz..."
               "Welcome to the"]
        zombies "SOKOBAN OF THE DEAD"
        text-center (box-center (guess-box lines))
        screen-center (box-center (s/get-size screen))
        offset (get-text-offset screen-center text-center)]
    (s/clear screen)
    (draw-text-centered screen lines)
    ; There is no way around this ugliness until we have full-fledged draw-arbitrary-text-with-colors function
    (put-string screen [(inc (count (last lines))) (count lines)] offset zombies :green :red)
    (s/redraw screen)))

(defmethod draw-ui :selection [screen game]
  (let [{:keys [selected-level levels]} game
        levels-text (prepare-selection-text game)
        selected-level-offset (rem selected-level levels-per-page)
        text-center (box-center (guess-box levels-text))
        screen-center (box-center (s/get-size screen))
        offset (get-text-offset screen-center text-center)]
    (s/clear screen)
    (draw-text-centered screen levels-text false)
    (s/move-cursor screen (+ 2 (first offset)) (+ (inc selected-level-offset) (second offset)))
    (draw-help screen ["Use j/k/PageUp/PageDown to select a level, Enter to play, ESC to exit"])
    (s/redraw screen)))

(defmethod draw-ui :victory [screen game]
  (let [lines ["Victory!"
               (str "Level completed in " (:steps game) " steps")
               "Press any key to return to the level selection screen"]]
    (s/clear screen)
      (draw-text-centered screen lines)
    (s/redraw screen)))

(defmethod draw-ui :playing [screen game]
  (let [world (:world game)
        player (:player world)
        matched-statues (rules/matched-statues world)
        level-center (box-center (rules/get-bounds world))
        screen-center (box-center (s/get-size screen))
        offset (get-text-offset screen-center level-center)]
    (s/clear screen)
    (s/put-string screen 0 0 (str "Level: " (:level-name world)))
    (s/put-string screen 0 1 (str "Steps so far: " (:steps game)))
    (draw-symbol screen "#" (:walls world) offset :grey)
    (draw-symbol screen "z" (:zombies world) offset :red)
    (draw-symbol screen "$" (:statues world) offset :green)
    (draw-symbol screen "*" matched-statues offset :blue)
    (draw-symbol screen "@" player offset :yellow)
    (draw-help screen ["Use h/j/k/l to move up/left/right/down"
                       "r to restart level, q for level selection, u to undo previous moves, ESC to exit"
                       "Move yourself (@) to push statues ($) onto zombies (z) to stay alive!"])
    (s/move-cursor screen 0 2)
    (s/redraw screen)))

(defmulti process-input
  (fn [game input]
    (:ui game)))

(defmethod process-input :splash [game _]
  (game/level-selection game))

(defmethod process-input :selection [game input]
  (let [selected-level (:selected-level game)]
    (case input
      \j (game/try-select-level game (inc selected-level))
      \k (game/try-select-level game (dec selected-level))
      :page-down (game/try-select-level game (+ selected-level levels-per-page))
      :page-up (game/try-select-level game (- selected-level levels-per-page))
      :escape (game/quit-game game)
      :enter (game/start-game game)
      game)))

(defmethod process-input :victory [game input]
  (case input
    :escape (game/quit-game game)
    (game/level-selection game)))

(defmethod process-input :playing [game input]
  (let [ui (:ui game)
        world (:world game)
        ; rules/win? check should occur after we process current input
        win (rules/win? world)]
    (if win
      (game/victory game)
      (case input
        :escape (game/quit-game game)
        \h (game/apply-move game :w)
        \j (game/apply-move game :s)
        \k (game/apply-move game :n)
        \l (game/apply-move game :e)
        \u (game/undo-move game)
        \r (game/restart-game game)
        \q (game/quit-level game)
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
                            (run-game (game/new-game) screen))))]
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