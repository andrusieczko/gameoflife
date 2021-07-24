(ns game-of-life.core
  (:require [clojure.edn :as edn
             ]))

(require '[clojure.string :as str])

(def game-settings
  {:rows 30
   :cols 30})

(defn board [{rows :rows cols :cols}]
  (mapv (fn [_] (mapv (fn [_] false) (range cols))) (range rows)))

(defn initial-state
  [game-settings]
  {:board (board game-settings)
   :settings {}})

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


(let [app (js/document.querySelector "#app")
      table (js/document.createElement "table")
      tbody (js/document.createElement "tbody")]
  (.setAttribute table "id" "game")
  (.append app table)
  (.append table tbody)

(defn build-table [{rows :rows cols :cols}]
  (dorun
   (for [row (range rows)]
     (let [tr (js/document.createElement "tr")]
       (.append tbody tr)
       (dorun
         (for [col (range cols)]
           (let [td (js/document.createElement "td")]
             (.setAttribute td "id" (str "cell-" col "-" row))
             (.append tr td)))))))))

(build-table game-settings)

(defn render-table
  [{rows :rows cols :cols} board]
  (dorun
    (for [row (range rows)]
      (dorun
        (for [col (range cols)]
          (if (get-in board [row col])
            (.add (. (js/document.querySelector (str "#cell-" row "-" col)) -classList) "alive")
            (.remove (. (js/document.querySelector (str "#cell-" row "-" col)) -classList) "alive")))))))


(defn toggle-cell! [e]
  (let [id (.getAttribute (. e -target) "id")
        [_ x y] (str/split id "-")]
    (set-cell! (edn/read-string x) (edn/read-string y) (not (get-cell x y)))
    (render-table game-settings (:board @game))
    ))

(.addEventListener (js/document.querySelector "#game") "click" toggle-cell!)
