(ns sokoban.game
  (:require [sokoban.rules :as rules]
            [sokoban.levels :as levels]))

(defrecord Game [world history input continue ui levels selected-level])
(defn new-game
  ([] (new-game (levels/get-all-levels)))
  ([levels] (Game. nil nil nil [true] :selection levels 0)))

(defn start-game [game]
  (let [{:keys [levels selected-level]} game
        new-level (nth levels selected-level)]
    (-> game
      (assoc :world new-level)
      (assoc :history [new-level])
      (assoc :ui :playing))))

(defn restart-game [game]
  (let [idx (:selected-level game)]
    (assoc game :world
      (nth (:levels game) idx))))

(defn apply-move [game move]
  (let [next-world (rules/move-player (:world game) move)
        history (:history game)]
    (-> game
        (assoc :world next-world)
        (assoc :history (conj history next-world)))))

(defn undo-move [game]
  (let [history (:history game)
        prev-history (pop history)
        prev-world (peek prev-history)]
    (if (empty? prev-history)
      game
      (-> game
          (assoc :world prev-world)
          (assoc :history prev-history)))))

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