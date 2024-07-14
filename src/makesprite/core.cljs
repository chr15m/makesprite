(ns makesprite.core
  (:require
    #_ [applied-science.js-interop :as j]
    #_ [promesa.core :as p]
    [reagent.core :as r]
    [reagent.dom :as rdom]
    [shadow.resource :as rc]
    [alandipert.storage-atom :refer [local-storage]]
    #_ ["openai" :as openai]))

(defn initial-state [] {})

; *** constants *** ;

(def re-html-comment (js/RegExp. "<\\!--.*?-->" "g"))

(def year (-> (js/Date.) .getFullYear))

; *** functions *** ;

(defn now [] (-> (js/Date.) .toISOString))

(defn make-id [] (str (random-uuid)))

(defn is-valid-key? [k]
  (and k
       (= (.slice k 0 3) "sk-")
       (> (aget k "length") 45)))

; *** components *** ;

(defn icon [svg attrs]
  [:inline-icon
   (merge {:dangerouslySetInnerHTML
           {:__html (.replace svg re-html-comment "")}}
          attrs)])

; *** views & event handlers ***;

(defn component:main [_state]
  [:main
   [:textarea 
    {:default-value "Hello."}]
   [:button {:on-click #(js/alert "Hello world!")
             :disabled true}
    "send"]])

(defn component:header []
  [:header
   [:nav
    [:ul.spread
     [:li
      [icon (rc/inline "tabler/filled/mushroom.svg")]
      [:strong "makesprite"]]
     [:li [icon (rc/inline "tabler/outline/menu-2.svg")]]]]])

; *** launch *** ;

(defonce state
  (local-storage
    (r/atom (initial-state))
    :makesprite-state))

(defn start {:dev/after-load true} []
  (rdom/render
    [:<>
     [component:header]
     [component:main state]]
    (js/document.getElementById "app")))

(defn init []
  (start))
