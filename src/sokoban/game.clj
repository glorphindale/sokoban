(ns sokoban.game
  (:require [sokoban.rules :as rules]
            [sokoban.levels :as levels]))

(defrecord Game [levels selected-level world history input continue ui steps])
(defn new-game
  ([] (new-game (levels/get-all-levels)))
  ([levels] (Game. levels 0 nil nil nil [true] :selection 0)))

(defn start-game [game]
  (let [{:keys [levels selected-level]} game
        new-level (nth levels selected-level)]
    (-> game
      (assoc :world new-level)
      (assoc :history [new-level])
      (assoc :ui :playing)
      (assoc :steps 0))))

(defn restart-game [game]
  (start-game game))

(defn apply-move [game move]
  (let [world (:world game)
        next-world (rules/move-player world move)
        history (:history game)]
    (if (not= world next-world)
      (-> game
          (assoc :world next-world)
          (assoc :history (conj history next-world))
          (assoc :steps (inc (:steps game))))
      game)))

(defn undo-move [game]
  (let [history (:history game)
        prev-history (pop history)
        prev-world (peek prev-history)]
    (if (empty? prev-history)
      game
      (-> game
          (assoc :world prev-world)
          (assoc :history prev-history)
          (assoc :steps (dec (:steps game)))))))

(defn level-selection [game]
  (-> game
      (assoc :ui :selection)
      (dissoc :world)))

(defn quit-game [game]
  (assoc game :continue []))

(defn quit-level [game]
  (assoc game :ui :selection))

(defn victory [game]
  (assoc game :ui :victory))

(defn try-select-level [game next-level]
  (let [min-level 0
        max-level (count (:levels game))]
    (if (<= min-level next-level (- max-level 1))
      (assoc game :selected-level next-level)
      game)))