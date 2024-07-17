(ns makesprite.core
  (:require
    #_ [applied-science.js-interop :as j]
    [promesa.core :as p]
    [reagent.core :as r]
    [reagent.dom :as rdom]
    [shadow.resource :as rc]
    [cognitect.transit :as t]
    ["idb-keyval" :as kv]
    #_ ["openai" :as openai]))

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
   :response_format "b64_json"
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

(defn get-parent [response-id logs]
  (->> logs
       (filter #(= (:id %) response-id))
       first))

(defn convert-image-data-to-blob [result]
  (let [image-data-b64 (get-in result [:data 0 :b64_json])
        image-array (js/Uint8Array.from (js/atob image-data-b64)
                                        #(.charCodeAt % 0))]
    (-> result
        (update-in [:data 0] dissoc :b64_json)
        (assoc :blob (js/Blob. #js [image-array])))))

(defn initiate-request [state]
  (let [prompt (get-in @state [:ui :prompt])
        text (str
               prompt-dall-e-strict "\n"
               prompt)
        log-entry {:id (make-id)
                   :k :dall-e-request
                   :t (now)
                   :prompt prompt
                   :payload (assoc api-req-dall-e :prompt text)}
        api-key (get-in @state [:settings :openai-key])]
    (swap! state
           #(-> %
                (assoc :inflight log-entry)
                (update-in [:log] conj log-entry)))
    (p/let [req (js/fetch
                  (str api-url "/v1/images/generations")
                  #js {:method "POST"
                       :headers #js {:Content-Type "application/json"
                                     :Authorization (str "Bearer " api-key)}
                       :body (-> (:payload log-entry)
                                 clj->js
                                 js/JSON.stringify)})
            res (.json req)
            ; extract image out to idb-keyval
            ; res (extract-image-from-response res)
            res (js->clj res :keywordize-keys true)
            res (convert-image-data-to-blob res)
            res {:id (make-id)
                    :k :dall-e-response
                    :t (now)
                    :parent (get-in log-entry [:id])
                    :response res}]
      (js/console.log "result" res)
      (swap! state
             #(-> %
                  (dissoc :inflight)
                  (update-in [:ui] dissoc :prompt)
                  (update-in [:log] conj res))))))

(defn button-notify [el]
  (let [cl (aget el "classList")
        rmfn (fn [] (.remove cl "notify"))]
    (if (.contains cl "notify")
      (rmfn)
      (.addEventListener el "transitionend" rmfn #js {:once true}))
    ; trigger CSS reflow
    ((fn [] (aget el "offsetHeight")))
    (.add cl "notify")))

(defn copy-text [el txt]
  (let [source (js/document.createElement "textarea")]
    (aset source "value" txt)
    (.appendChild el source)
    (.focus source)
    (.select source)
    (js/document.execCommand "copy")
    (.removeChild el source)))

; *** components *** ;

(defn icon
  ([attrs svg]
   [:inline-icon
    (merge {:dangerouslySetInnerHTML
            {:__html (.replace svg re-html-comment "")}}
           attrs)])
  ([svg] (icon {} svg)))

; *** views & event handlers ***;

(defn component:prompt [state]
  (let [txt (get-in @state [:ui :prompt])]
     [:textarea
      {:rows 7
       :read-only (seq (:inflight @state))
       :placeholder "Enter your game sprite prompt here..."
       :on-change #(swap! state
                          assoc-in [:ui :prompt]
                          (-> % .-target .-value))
       :value txt}]))

(defn component:response [log state]
  (let [parent (get-parent (:parent log) (:log @state))
        blob (get-in log [:response :blob])
        url (when blob (js/URL.createObjectURL blob))]
    [:generated-image
     (if blob
       [:img {:src url}]
       [:blockquote (:prompt parent)])
     [:action-buttons
      [icon
       {:data-notification-text "Prompt copied!"
        :on-click #(let [el (-> % .-currentTarget)]
                     (copy-text el (:prompt parent))
                     (button-notify el))}
       (rc/inline "tabler/outline/copy.svg")]
      [icon
       {:on-click #(swap! state assoc-in [:ui :prompt] (:prompt parent))}
       (rc/inline "tabler/outline/refresh.svg")]]]))

(defn component:log [state]
  [:ul.log
   (for [log (reverse (:log @state))]
     [:li {:key (:id log)}
      (case (:k log)
        :dall-e-response [component:response log state]
        #_#_ :dall-e-request [:span (get-in log [:prompt])]
        nil)])])

(defn component:main [state]
  (let [openai-key (get-in @state [:settings :openai-key])]
    [:main
     [:input {:placeholder "Enter OpenAI API key..."
              :value openai-key
              :on-change #(swap! state assoc-in [:settings :openai-key]
                                 (-> % .-target .-value))
              :class (when (not (is-valid-key? openai-key)) "warning")}]
     [component:prompt state (r/atom nil)]
     (let [disabled (or (empty? (get-in @state [:ui :prompt]))
                        (not (is-valid-key? openai-key))
                        (seq (:inflight @state)))]
       [:action-buttons
        [:button {:on-click #(swap! state assoc-in [:ui :prompt] "")
                  :disabled disabled}
         "clear"]
        [:button {:on-click #(initiate-request state)
                  :disabled disabled}
         (if (seq (:inflight @state))
           "sending"
           "send")]])
     [component:log state]]))

(defn component:header []
  [:header
   [:nav
    [:ul.spread
     [:li
      [icon (rc/inline "tabler/filled/mushroom.svg")]
      [:strong "makesprite"]]
     [:li [icon (rc/inline "tabler/outline/menu-2.svg")]]]]])

; *** launch *** ;

(defonce state (r/atom nil))

(defn start {:dev/after-load true} []
  (rdom/render
    [:<>
     [component:header]
     [component:main state]]
    (js/document.getElementById "app")))

(def w
  (t/writer :json
            #_ {:handlers {js/AudioBuffer (AudioBufferHandler.)}}))

(def r
  (t/reader :json
            #_ {:handlers {"audio-buffer" deserialize-audio-buffer}}))

(defn serialize-app-state [structure]
  (t/write w structure))

(defn deserialize-app-state [transit]
  (t/read r transit))

; clear the saved state
; (kv/del "makesprite-state")

(defn init []
  (add-watch state :state-watcher
             (fn [_k _r old-state new-state]
               (when (not= old-state new-state)
                 (kv/update "makesprite-state"
                            (fn []
                              (serialize-app-state new-state))))))
  (p/let [serialized-value (kv/get "makesprite-state")
          serialized (if serialized-value
                       (deserialize-app-state serialized-value)
                       (initial-state))]
    (js/console.log "serialized state" serialized)
    (reset! state serialized)
    (start)))
