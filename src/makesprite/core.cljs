(ns makesprite.core
  (:require
    [applied-science.js-interop :as j]
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
  (p/let [image-data-b64 (get-in result [:data 0 :b64_json])
          image-array (js/Uint8Array.from (js/atob image-data-b64)
                                          #(.charCodeAt % 0))
          image-blob (js/Blob. #js [image-array])
          image-id (make-id)]
    (kv/set (str "image-" image-id) image-blob)
    (-> result
        (update-in [:data 0] dissoc :b64_json)
        (assoc :image-id image-id))))

(defn remove-error [*state]
  (update-in *state [:ui] dissoc :error-message))

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
                remove-error
                (update-in [:log] conj log-entry)))
    (p/catch
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
                    (update-in [:log] conj res))))
      (fn [err]
        (swap! state
               #(-> %
                    (dissoc :inflight)
                    (assoc-in [:ui :error-message] (.toString err))))))))

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

(defn resize-canvas-to-image! [canvas img]
  (aset canvas "width" (aget img "width"))
  (aset canvas "height" (aget img "height")))

(defn process-click
  [_state img ev]
  (js/console.log "process-click")
  (let [target (-> ev .-currentTarget)
        rect (.getBoundingClientRect target)
        x (- (aget ev "clientX") (aget rect "left"))
        y (- (aget ev "clientY") (aget rect "top"))
        canvas (js/document.querySelector "canvas#workspace")
        ctx (.getContext canvas "2d")]
    (resize-canvas-to-image! canvas img)
    (.drawImage ctx img 0 0 (aget img "width") (aget img "height"))
    (let [img-data (aget (.getImageData ctx 0 0
                                        (aget img "width")
                                        (aget img "height"))
                         "data")
          xs (js/Math.round (* (/ x (aget rect "width")) (aget img "width")))
          ys (js/Math.round (* (/ y (aget rect "height")) (aget img "height")))
          index (js/Math.round (* (+ (* ys (aget img "width")) xs) 4))
          color-clicked (map #(aget img-data (+ index %)) (range 4))
          #_#_ css-color (str "5px solid rgb("
                         (nth color-clicked 0) ","
                         (nth color-clicked 1) ","
                         (nth color-clicked 2)
                         ")")]
      (js/console.log index)
      (js/console.log xs ys)
      (js/console.log "img-data" img-data)
      (js/console.log "color-clicked" color-clicked)
      ;(js/console.log css-color)
      ; (aset ctx "fillStyle" "red")
      ; (.fillRect ctx xs ys 1 1)
      ;(aset target "style" "border" css-color)
      
      )
    (comment (js/console.log "rect" rect)
             (js/console.log "click-pos" [x y])
             (js/console.log canvas)
             (js/console.log (aget target "clientWidth")
                             (aget target "clientHeight"))
             (js/console.log (aget ev "clientX")
                             (aget ev "clientY"))
             (js/console.log (aget img "width")
                             (aget img "height"))
             (js/console.log img))))

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

(defn component:image [state image-id parent]
  (let [img-ref (r/atom nil)]
    (p/let [blob (kv/get (str "image-" image-id))
            url (when blob (js/URL.createObjectURL blob))
            img (when url (js/Image.))
            done-fn #(reset! img-ref [img url])]
      (if img
        (do
          (aset img "onload" done-fn)
          (aset img "onerror" #(reset! img-ref [nil nil]))
          (j/assoc! img :src url))
        (done-fn)))
    (fn []
      (let [[img url] @img-ref]
        (if img
          [:img {:src url
                 :on-click #(process-click state img %)}]
          [:blockquote (:prompt parent)])))))

(defn component:response [log state]
  (let [parent (get-parent (:parent log) (:log @state))
        image-id (get-in log [:response :image-id])]
    [:generated-image
     [component:image state image-id parent]
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
         [icon (rc/inline "tabler/outline/trash.svg")]
         "clear"]
        [:button {:on-click #(initiate-request state)
                  :disabled disabled}
         (if (seq (:inflight @state))
           [:<>
            [icon
             {:class "spin"}
             (rc/inline "tabler/filled/square.svg")]
            "sending"]
           [:<>
            [icon (rc/inline "tabler/outline/send.svg")]
            "send"])]])
     (when-let [msg (get-in @state [:ui :error-message])]
       [:p.error.spread
        [:span
         [icon (rc/inline "tabler/outline/alert-circle.svg")]
         msg]
        [icon {:class "right clickable"
               :on-click #(swap! state remove-error)}
         (rc/inline "tabler/outline/x.svg")]])
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
     [component:main state]
     [:canvas#workspace]]
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
    (reset! state
            (dissoc serialized :inflight))
    (start)))
