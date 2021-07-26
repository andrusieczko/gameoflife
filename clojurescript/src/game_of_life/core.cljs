(ns game-of-life.core
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def game-settings
  {:rows 30
   :cols 30})

(defn board [{rows :rows cols :cols}]
  (mapv (fn [_] (mapv (fn [_] false) (range cols))) (range rows)))

(defn initial-state
  [game-settings]
  {:board (board game-settings)
   :settings {}
   :game-id nil})

(def game
  (atom (initial-state game-settings)))

(defn set-cell!
  [x y value]
  (do
    (println x y value)
   (swap! game assoc :board (assoc-in (:board @game) [x y] value))))

(defn get-cell
  [x y]
  (get-in (:board @game) [x y]))

(defn id
  [row col]
  (str "cell-" row "-" col))

(defn init-table!
  [{rows :rows cols :cols}]
 (let [app (js/document.querySelector "#app")
       table (js/document.createElement "table")
       tbody (js/document.createElement "tbody")]
   (.setAttribute table "id" "game")
   (.append app table)
   (.append table tbody)

   (dorun
     (for [row (range rows)]
       (let [tr (js/document.createElement "tr")]
         (.append tbody tr)
         (dorun
           (for [col (range cols)]
             (let [td (js/document.createElement "td")]
               (.setAttribute td "id" (id row col))
               (.append tr td)))))))
   ))

(if-not (.querySelector js/document "table")
 (init-table! game-settings))

(defn render-table
  [{rows :rows cols :cols}]
  (dorun
    (for [row (range rows)]
      (dorun
        (for [col (range cols)]
          (if (get-in (:board @game) [row col])
            (.add (. (js/document.querySelector (str "#" (id row col))) -classList) "alive")
            (.remove (. (js/document.querySelector (str "#" (id row col))) -classList) "alive")))))))


(defn cell-next-state
  [board x y]
  (let [neighbour-transactions (->> (for [i (range -1 2)
                                          j (range -1 2)]
                                      [i j])
                                 (remove (fn [[x y]] (= 0 x y))))
        count (->> neighbour-transactions
                   (map (fn [[i j]] (get-in board [(+ x i) (+ y j)])))
                   (remove nil?)
                   (map #(if (true? %) 1 0))
                   (reduce + 0))]
    (if (get-in board [x y])
      (or (= 2 count) (= 3 count))
      (= 3 count))))

(defn next-state
  [board {rows :rows cols :cols}]
  (mapv (fn [row] (mapv (fn [col] (cell-next-state board row col)) (range cols))) (range rows)))

(defn toggle-cell! [e]
  (let [id (.getAttribute (. e -target) "id")
        [_ x y] (str/split id "-")]
    (set-cell! (edn/read-string x) (edn/read-string y) (not (get-cell x y)))
    (render-table game-settings)
    ))

(.addEventListener (js/document.querySelector "#game") "click" toggle-cell!)


(defn random
  [{rows :rows cols :cols}]
  (mapv (fn [_] (mapv (fn [_] (< (rand) 0.2)) (range cols))) (range rows)))

(defn start!
  []
  (let [game-id (.setInterval js/window (fn [] (do
                                                 (swap! game assoc :board (next-state (:board @game) game-settings))
                                                 (render-table game-settings))) 300)]
    (swap! game assoc :game-id game-id)))

(defn init-random!
  []
  (swap! game assoc :board (random game-settings))
  (render-table game-settings))

(init-random!)
(start!)