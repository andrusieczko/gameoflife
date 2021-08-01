(ns game-of-life.core
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [goog.dom :as dom]))

(def game-settings
  {:rows          30
   :cols          30
   :random-factor 0.2
   :interval      300})

(defn board [{rows :rows cols :cols}]
  (mapv (fn [_] (mapv (fn [_] false) (range cols))) (range rows)))

(def state
  {:board                (atom (board game-settings))
   :game-settings        (atom game-settings)
   :game-id              (atom nil)
   :number-of-iterations (atom 0)})

(defn set-cell!
  [x y value]
  (swap! (:board state) assoc-in [x y] value))

(defn get-cell
  [x y]
  (get-in @(:board state) [x y]))

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

(defn update-iterations!
  []
  (let [iterations @(:number-of-iterations state)
        iterations-span (js/document.querySelector "#iterations")]
    (dom/setTextContent iterations-span iterations)))

(add-watch (:number-of-iterations state) :watch-iterations (fn [key atom old-val new-val] (update-iterations!)))

(defn neighbour-translations
  []
  (->> (for [i (range -1 2)
             j (range -1 2)
             :when (not= 0 i j)]
         [i j])))

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
  (let [id (-> e .-target (.getAttribute "id"))
        [_ x-str y-str] (str/split id "-")
        x (edn/read-string x-str)
        y (edn/read-string y-str)]
    (set-cell! x y (not (get-cell x y)))))

(defn random
  [{rows :rows cols :cols random-factor :random-factor}]
  (mapv (fn [_] (mapv (fn [_] (< (rand) random-factor)) (range cols))) (range rows)))

(defn inc-iterations!
  []
  (swap! (:number-of-iterations state) inc))

(defn reset-iterations!
  []
  (reset! (:number-of-iterations state) 0))

(defn start!
  []
  (let [game-settings @(:game-settings state)
        interval (:interval game-settings)
        game-id (.setInterval js/window (fn []
                                          (reset! (:board state) (next-state @(:board state) game-settings))
                                          (inc-iterations!)) interval)]
    (reset! (:game-id state) game-id)))

(defn stop!
  []
  (.clearInterval js/window @(:game-id state)))

(defn init-random!
  []
  (reset! (:board state) (random @(:game-settings state)))
  (reset-iterations!))

(defn clear!
  []
  (reset! (:board state) (board @(:game-settings state)))
  (reset-iterations!))

(defn set-interval!
  [interval]
  (swap! (:game-settings state) assoc :interval interval))

(defn set-random-factor!
  [random-factor]
  (swap! (:game-settings state) assoc :random-factor random-factor))

(init-random!)
(start!)
(render-table! @(:board state) game-settings)

(add-watch (:board state) :board-watcher (fn [key atom old-val new-val] (render-table! @(:board state) game-settings)))
(add-watch (:game-id state) :game-id-watcher (fn [key atom old-val new-val] (render-table! @(:board state) game-settings)))
(add-watch (:game-settings state) :game-settings-watcher (fn [key atom old-val new-val] (render-table! @(:board state) game-settings)))


(.addEventListener (js/document.querySelector "#game") "click" toggle-cell!)

(.addEventListener (js/document.querySelector "#start") "click" (fn [_] (start!)))
(.addEventListener (js/document.querySelector "#stop") "click" (fn [_] (stop!)))
(.addEventListener (js/document.querySelector "#random") "click" (fn [_] (init-random!)))
(.addEventListener (js/document.querySelector "#clear") "click" (fn [_] (clear!)))
(.addEventListener (js/document.querySelector "#randomFactor") "change"
                   (fn [_]
                     (set-random-factor! (-> (js/document.querySelector "#randomFactor") .-value))))

(.addEventListener (js/document.querySelector "#refreshInterval") "keyup"
                   (fn [e]
                     (set-interval! (-> e .-target .-value))
                     (stop!)
                     (start!)))