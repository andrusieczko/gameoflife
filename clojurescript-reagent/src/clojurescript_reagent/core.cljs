(ns clojurescript-reagent.core
  (:require
    [reagent.core :as r]
    [reagent.dom :as d]))

;; -------------------------
;; Model

(def game-settings
  {:rows          30
   :cols          30
   :random-factor 0.2
   :interval      300})

(defn board [{rows :rows cols :cols}]
  (mapv (fn [_] (mapv (fn [_] false) (range cols))) (range rows)))

(def state
  {:board                (r/atom (board game-settings))
   :game-settings        (r/atom game-settings)
   :game-id              (r/atom nil)
   :number-of-iterations (r/atom 0)})

;; -------------------------
;; Logic

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

;; -------------------------
;; Views

(defn toggle-cell! [x y]
  (swap! (:board state) assoc-in [x y] (not (get-in @(:board state) [x y]))))

(defn table-cell [x y cell]
  [:td {:key      (str "cell-" x y)
        :class    (if cell "alive")
        :on-click (fn [] (toggle-cell! x y))}])

(defn table-row [x row]
  [:tr {:key (str "row-" x)} (map-indexed (fn [y cell] (table-cell x y cell)) row)])

(defn table []
  [:table {:id "game"}
   [:tbody (map-indexed table-row @(:board state))]])

(defn home-page []
  [:div
   [:h2 "ClojureScript Reagent"]
   [table]
   [:input {:type "button" :value "Start" :on-click #(start!)} ]
   [:input {:type "button" :value "Stop" :on-click #(stop!)} ]
   [:input {:type "button" :value "Random" :on-click #(init-random!)} ]
   [:input {:type "button" :value "Clear" :on-click #(clear!)} ]
   [:span " | "]
   [:label {:for "refreshInterval"} ]
   [:input {:type     "text" :id "refreshInterval"
            :value    (:interval @(:game-settings state))
            :on-change (fn [e]
                         (set-interval! (-> e .-target .-value))
                         (stop!)
                         (start!))} ]
   [:span " | "]
   [:label {:for "randomFactor"} ]
   [:input {:type     "text" :id "randomFactor"
            :value    (:random-factor @(:game-settings state))
            :on-change #(set-random-factor! (-> % .-target .-value))} ]
   [:span " | "]
   [:span (str "Number of iterations: " @(:number-of-iterations state))]
   ])

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
