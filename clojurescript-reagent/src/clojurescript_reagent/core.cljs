(ns clojurescript-reagent.core
    (:require
      [reagent.core :as r]
      [reagent.dom :as d]))


(def counter (r/atom 0))

(js/setInterval (fn [] (swap! counter inc)) 1000)

;; -------------------------
;; Views

(defn counter-view []
  [:span (str @counter)])

(defn home-page []
  [:div
   [:h2 "Welcome to Reagent"]
   [counter-view]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
