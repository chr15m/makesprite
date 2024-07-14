(ns makesprite.core
  (:require
    #_ [applied-science.js-interop :as j]
    [promesa.core :as p]
    [reagent.core :as r]
    [reagent.dom :as rdom]
    [shadow.resource :as rc]
    [alandipert.storage-atom :refer [local-storage]]
    #_ ["openai" :as openai]))

;curl https://api.openai.com/v1/images/generations \
;  -H "Content-Type: application/json" \
;  -H "Authorization: Bearer $OPENAI_API_KEY" \

; *** constants *** ;

(def api-url "https://api.openai.com")

(def re-html-comment (js/RegExp. "<\\!--.*?-->" "g"))

(def year (-> (js/Date.) .getFullYear))

(def prompt-dall-e-strict "I NEED to test how the tool works with extremely simple prompts. DO NOT add any detail, just use it AS-IS:")

(def placeholder-prompt
  "A collection of casual hand-drawn game sprites for a roguelike game, featuring a new set of fantasy characters with even simpler bodies and limbs. These characters are emphasized with a very doodly, sketchy hand-drawn style, using a muted and soft color palette. The characters are designed with extremely simplified chibi-style proportions, featuring larger heads, very minimalistic bodies, and almost stick-figure-like limbs, depicted in front-facing poses on a pure white background. The emphasis is on making the sprites whimsically minimalistic, maintaining consistency with the game's visual theme.")

(def api-req-dall-e
  {:model "dall-e-3"
   :prompt ""
   :n 1
   :size "1792x1024"})

; *** functions *** ;

; simply a history of events e.g. sends, inflights, etc.?
(defn initial-state []
  {:inflight {}
   :settings {:openai-key nil}
   :ui {:prompt placeholder-prompt}
   :log []})

(defn now [] (-> (js/Date.) .toISOString))

(defn make-id [] (str (random-uuid)))

(defn is-valid-key? [k]
  (and k
       (= (.slice k 0 3) "sk-")
       (> (aget k "length") 45)))

(defn initiate-request [state]
  (let [text (str
               prompt-dall-e-strict "\n"
               (get-in @state [:ui :prompt]))
        payload (assoc api-req-dall-e
                       :prompt text
                       :metadata {:id (make-id)
                                  :k :dall-e-request
                                  :t (now)})
        api-key (get-in @state [:settings :openai-key])]
    (swap! state
           #(-> %
                (assoc :inflight payload)
                (update-in [:log] conj payload)))
    (p/let [req (js/fetch
                  (str api-url "/v1/images/generations")
                  #js {:method "POST"
                       :headers #js {:Content-Type "application/json"
                                     :Authorization (str "Bearer " api-key)}
                       :body (-> payload
                                 (dissoc :metadata)
                                 clj->js
                                 js/JSON.stringify)})
            result (.json req)
            result (js->clj result :keywordize-keys true)
            result (assoc result
                          :metadata
                          {:id (make-id)
                           :parent (get-in payload [:metadata :id])
                           :k :dall-e-response
                           :t (now)})]
      (js/console.log result)
      (swap! state
             #(-> %
                  (dissoc :inflight)
                  (update-in [:ui] dissoc :prompt)
                  (update-in [:log] conj result))))))

; *** components *** ;

(defn icon [svg attrs]
  [:inline-icon
   (merge {:dangerouslySetInnerHTML
           {:__html (.replace svg re-html-comment "")}}
          attrs)])

; *** views & event handlers ***;

(defn component:prompt [state]
  (let [txt (get-in @state [:ui :prompt])]
     [:textarea
      {:rows 10
       :placeholder "Enter your game sprite prompt here..."
       :on-change #(swap! state
                          assoc-in [:ui :prompt]
                          (-> % .-target .-value))
       :value txt}]))

(defn component:main [state]
  (let [openai-key (get-in @state [:settings :openai-key])]
    [:main
     [:input {:placeholder "Enter OpenAI API key..."
              :value openai-key
              :read-only (seq (:inflight @state))
              :on-change #(swap! state assoc-in [:settings :openai-key]
                                 (-> % .-target .-value))
              :class (when (not (is-valid-key? openai-key)) "warning")}]
     [component:prompt state (r/atom nil)]
     [:button {:on-click #(initiate-request state)
               :disabled (or (empty? (get-in @state [:ui :prompt]))
                             (not (is-valid-key? openai-key))
                             (seq (:inflight @state)))}
      "send"]
     (when (seq (:inflight @state))
       [:p "LOADING..."])]))

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
