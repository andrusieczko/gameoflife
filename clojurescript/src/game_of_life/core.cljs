(ns game-of-life.core
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def game-settings
  {:rows 30
   :cols 30
   :random-factor 0.2
   :interval 300})

(defn board [{rows :rows cols :cols}]
  (mapv (fn [_] (mapv (fn [_] false) (range cols))) (range rows)))

(defn initial-state
  [game-settings]
  {:board (board game-settings)
   :game-settings game-settings
   :game-id nil})

(def state                                                   
  (atom (initial-state game-settings)))

(defn set-cell!
  [x y value]
  (swap! state assoc :board (assoc-in (:board @state) [x y] value)))

(defn get-cell
  [x y]
  (get-in (:board @state) [x y]))

(defn id
  [row col]
  (str "cell-" row "-" col))

(defn init-table!
  [{rows :rows cols :cols}]
 (let [tbody (js/document.querySelector "#game tbody")]
   (dorun
     (for [row (range rows)]
       (let [tr (js/document.createElement "tr")]
         (.append tbody tr)
         (dorun
           (for [col (range cols)]
             (let [td (js/document.createElement "td")]
               (.setAttribute td "id" (id row col))
               (.append tr td)))))))))

(if-not (.querySelector js/document "table tr")
 (init-table! game-settings))

(defn render-table!
  [board {rows :rows cols :cols}]
  (dorun
    (for [row (range rows)]
      (dorun
        (for [col (range cols)]
          (if (get-in board [row col])
            (.add (. (js/document.querySelector (str "#" (id row col))) -classList) "alive")
            (.remove (. (js/document.querySelector (str "#" (id row col))) -classList) "alive")))))))


(defn neighbour-translations
  []
  (->> (for [i (range -1 2)
             j (range -1 2)]
         [i j])
       (remove (fn [[x y]] (= 0 x y)))))

(defn cell-next-state
  [board x y]
  (let [count (->> (neighbour-translations)
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
    (render-table! (:board @state) game-settings)
    ))


(defn random
  [{rows :rows cols :cols random-factor :random-factor}]
  (mapv (fn [_] (mapv (fn [_] (< (rand) random-factor)) (range cols))) (range rows)))

(defn start!
  [state]
  (let [game-id (.setInterval js/window (fn []
                                          (do
                                            (swap! state assoc :board (next-state (:board @state) game-settings))
                                            (render-table! (:board @state) game-settings))) (get-in @state [:game-settings :interval]))]
    (swap! state assoc :game-id game-id)))

(defn stop!
  [state]
  (.clearInterval js/window (:game-id @state)))

(defn init-random!
  [state]
  (swap! state assoc :board (random (:game-settings @state))))

(defn clear!
  [state]
  (swap! state assoc :board (board game-settings)))

(defn set-interval!
  [state interval]
  (swap! state assoc-in [:game-settings :interval] interval))

(defn set-random-factor!
  [state random-factor]
  (swap! state assoc-in [:game-settings :random-factor] random-factor))

(init-random! state)
(start! state)
(render-table! (:board @state) game-settings)



(.addEventListener (js/document.querySelector "#game") "click" toggle-cell!)

(.addEventListener (js/document.querySelector "#start") "click" (fn [_] (start! state)))
(.addEventListener (js/document.querySelector "#stop") "click" (fn [_] (stop! state)))
(.addEventListener (js/document.querySelector "#random") "click" (fn [_] (init-random! state) (render-table! (:board @state) game-settings)))
(.addEventListener (js/document.querySelector "#clear") "click" (fn [_] (clear! state) (render-table! (:board @state) game-settings)))
(.addEventListener (js/document.querySelector "#randomFactor") "change"
                   (fn [_]
                     (set-random-factor! state (. (js/document.querySelector "#randomFactor") -value))))

(.addEventListener (js/document.querySelector "#refreshInterval") "keyup"
                   (fn [e]
                     (set-interval! state (. (. e -target) -value))
                     (stop! state)
                     (start! state)))