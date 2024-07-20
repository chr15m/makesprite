(ns makesprite.core
  (:require
    [applied-science.js-interop :as j]
    [promesa.core :as p]
    [reagent.core :as r]
    [reagent.dom :as rdom]
    [shadow.resource :as rc]
    [cognitect.transit :as t]
    ["idb-keyval" :as kv]
    ["q-floodfill$default" :as floodfill]
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

(defn load-image [url]
  (js/Promise.
    (fn [res err]
      (let [img (js/Image.)]
        (aset img "onload" #(res img))
        (aset img "onerror" err)
        (aset img "src" url)))))

(defn canvas-to-blob [canvas]
  (js/Promise.
    (fn [res _err]
      (.toBlob canvas res))))

(defn resize-canvas-to-image! [canvas img]
  (aset canvas "width" (aget img "width"))
  (aset canvas "height" (aget img "height")))

(defn process-b64-image-and-store! [result]
  (p/let [image-data-b64 (get-in result [:data 0 :b64_json])
          image-array (js/Uint8Array.from (js/atob image-data-b64)
                                          #(.charCodeAt % 0))
          image-blob (js/Blob. #js [image-array])
          image-id (make-id)]
    (kv/set (str "image-" image-id) image-blob)
    ; flood fill the corners and edges
    (p/let [img (load-image (js/URL.createObjectURL image-blob))
            canvas (.createElement js/document "canvas")
            ctx (.getContext canvas "2d")
            w (aget img "width")
            h (aget img "height")]
      (js/console.log "img" img)
      (resize-canvas-to-image! canvas img)
      (.drawImage ctx img 0 0 w h)
      ; flood fill at the corners and edges
      (let [ff (floodfill. (.getImageData ctx 0 0 w h))
            w (- w 2)
            h (- h 2)
            w2 (js/Math.floor (/ w 2))
            h2 (js/Math.floor (/ h 2))]
        (.fill ff "rgba(0,0,0,0)" 0 0 50)
        (doseq [[x y] [[0 0] [w 0] [w h] [0 h]
                       [0 h2] [w h2]
                       [w2 0] [w2 h]]]
          (.fill ff "rgba(0,0,0,0)" x y 50))
        (.putImageData ctx (aget ff "imageData") 0 0))
      ; save the processed version
      (js/console.log "canvas-to-blob")
      (p/let [blob (canvas-to-blob canvas)]
        (kv/set (str "image-processed-" image-id) blob)
        (js/console.log "image-processed saved")))
    (js/console.log "save result")
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
              res (process-b64-image-and-store! res)
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

; *** views & event handlers ***;

(defn component:prompt [state]
  (let [txt (get-in @state [:ui :prompt])]
     [:textarea#prompt
      {:rows 7
       :read-only (seq (:inflight @state))
       :placeholder "Enter your game sprite prompt here..."
       :on-change #(swap! state
                          assoc-in [:ui :prompt]
                          (-> % .-target .-value))
       :value txt}]))

(defn update-bounding-box [bb x y]
  (-> bb
      (update :min-x #(min % x))
      (update :min-y #(min % y))
      (update :max-x #(max % x))
      (update :max-y #(max % y))))

(defn multi-pass-flood-fill-and-extract
  [canvas start-x start-y background-color]
  (let [ctx (.getContext canvas "2d")
        width (.-width canvas)
        height (.-height canvas)
        image-data (.getImageData ctx 0 0 width height)
        data (.-data image-data)
        extracted-pixels (js/Set.)
        stack (js/Array. #js [start-x start-y])
        bounding-box (atom {:min-x width
                            :min-y height
                            :max-x 0
                            :max-y 0})]
    ; First pass: Find pixels to extract
    (while (pos? (.-length stack))
      (let [[x y] (.pop stack)
            index (* (+ (* y width) x) 4)
            key (str x "," y)]
        (when (and (>= x 0) (< x width)
                   (>= y 0) (< y height)
                   (not (.has extracted-pixels key)))
          (let [current-color [(aget data index)
                               (aget data (+ index 1))
                               (aget data (+ index 2))
                               (aget data (+ index 3))]]
            (when-not (= current-color background-color)
              (.add extracted-pixels key)
              (swap! bounding-box update-bounding-box x y)
              (.push stack
                     #js [(inc x) y] #js [(dec x) y]
                     #js [x (inc y)] #js [x (dec y)]))))))
    ; Second pass: Extract only the identified pixels
    (let [{:keys [min-x min-y max-x max-y]} @bounding-box
          extracted-w (inc (- max-x min-x))
          extracted-h (inc (- max-y min-y))
          extracted-image-data (.createImageData ctx extracted-w extracted-h)
          extracted-data (.-data extracted-image-data)]

      (doseq [y (range min-y (inc max-y))
              x (range min-x (inc max-x))
              :let [source-index (* (+ (* y width) x) 4)
                    target-index (* (+ (* (- y min-y) extracted-w)
                                       (- x min-x)) 4)
                    key (str x "," y)]]
        (if (.has extracted-pixels key)
          (do
            (aset extracted-data target-index
                  (aget data source-index))
            (aset extracted-data (+ target-index 1)
                  (aget data (+ source-index 1)))
            (aset extracted-data (+ target-index 2)
                  (aget data (+ source-index 2)))
            (aset extracted-data (+ target-index 3) 255))
          (aset extracted-data (+ target-index 3) 0)))

      extracted-image-data)))

(defn mount-canvas [canvas img]
  (when canvas
    (let [ctx (.getContext canvas "2d")
          w (aget img "width")
          h (aget img "height")]
      (resize-canvas-to-image! canvas img)
      (.drawImage ctx img 0 0 w h))))

(defn mount-canvas-sprite [canvas img-data]
  (when canvas
    (let [ctx (.getContext canvas "2d")]
      (resize-canvas-to-image! canvas img-data)
      (.putImageData ctx img-data 0 0))))

(defn canvas-click [state _image-id ev]
  (let [canvas (-> ev .-currentTarget)
        ctx (.getContext canvas "2d")
        rect (.getBoundingClientRect canvas)
        x (- (aget ev "clientX") (aget rect "left"))
        y (- (aget ev "clientY") (aget rect "top"))
        w (aget canvas "width")
        h (aget canvas "height")
        img-data (.getImageData ctx 0 0 w h)
        img-data-data (aget img-data "data")
        xs (js/Math.round (* (/ x (aget rect "width")) w))
        ys (js/Math.round (* (/ y (aget rect "height")) h))
        index (js/Math.round (* (+ (* ys w) xs) 4))
        color-clicked (map #(aget img-data-data (+ index %)) (range 4))
        #_#_ css-color (str "5px solid rgb("
                            (nth color-clicked 0) ","
                            (nth color-clicked 1) ","
                            (nth color-clicked 2)
                            ")")]
    (js/console.log "color-clicked" color-clicked)
    (when (not= color-clicked '(0 0 0 0))
      (swap! state assoc-in [:ui :sprite] :loading)
      (p/let [_ (p/delay 1)
              sprite-image-data (multi-pass-flood-fill-and-extract
                                  canvas xs ys [0 0 0 0])]
        (js/console.log "extracted" sprite-image-data)
        (swap! state assoc-in [:ui :sprite] sprite-image-data))
      (js/console.log "done extract"))
    #_ (let [ff (floodfill. img-data)]
         (.fill ff "rgba(0,0,0,0)" xs ys 50)
         (.putImageData ctx (aget ff "imageData") 0 0)
         (p/let [blob (canvas-to-blob canvas)]
           (kv/set (str "image-processed-" image-id) blob)))))

(defn component:image [state image-id parent]
  (let [img-ref (r/atom nil)]
    (p/let [blob (kv/get (str "image-processed-" image-id))
            blob (or blob (kv/get (str "image-" image-id)))
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
      (let [[img _url] @img-ref]
        (if img
          [:div
           [:canvas.chequerboard
            {:ref #(mount-canvas % img)
             :on-click #(canvas-click state image-id %)}]]
          [:blockquote (:prompt parent)])))))

(defn component:response [log state]
  (let [parent (get-parent (:parent log) (:log @state))
        image-id (get-in log [:response :image-id])]
    [:generated-image
     [component:image state image-id parent]
     [:action-buttons
      [icon
       {:data-notification-text "Prompt copied!"
        :title "Copy prompt to clipboard"
        :on-click #(let [el (-> % .-currentTarget)]
                     (copy-text el (:prompt parent))
                     (button-notify el))}
       (rc/inline "tabler/outline/copy.svg")]
      [icon
       {:title "Re-run prompt"
        :on-click (fn [_ev]
                    (swap! state assoc-in [:ui :prompt] (:prompt parent))
                    (-> (js/document.querySelector "#prompt")
                        (.scrollIntoView true)))}
       (rc/inline "tabler/outline/refresh.svg")]]]))

(defn component:log [state]
  [:ul.log
   (for [log (reverse (:log @state))]
     [:li {:key (:id log)}
      (case (:k log)
        :dall-e-response [component:response log state]
        #_#_ :dall-e-request [:span (get-in log [:prompt])]
        nil)])])

(defn component:extracted-sprite [state]
  (when-let [img-data (get-in @state [:ui :sprite])]
    (let [close-fn #(swap! state update-in [:ui] dissoc :sprite)]
      [:sprite-dialog
       {:on-click #(when (= (aget % "currentTarget")
                            (aget % "target"))
                     (close-fn))}
       [:div
        [:div.spread
         [:span]
         [:span
          [icon {:class "right clickable"
                 :on-click close-fn}
           (rc/inline "tabler/outline/x.svg")]]]
        (if (= img-data :loading)
          [:span [icon {:class "spin"}
                  (rc/inline "tabler/outline/spiral.svg")]
           "extracting"]
          [:canvas.chequerboard
           {:ref #(mount-canvas-sprite % img-data)}])]])))

(defn component:main [state]
  (let [openai-key (get-in @state [:settings :openai-key])]
    [:main
     [component:extracted-sprite state]
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
             (rc/inline "tabler/outline/spiral.svg")]
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
     [:canvas#workspace.chequerboard]]
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
                              (-> new-state
                                  (dissoc :inflight)
                                  (update-in [:ui] dissoc :sprite)
                                  serialize-app-state))))))
  (p/let [serialized-value (kv/get "makesprite-state")
          serialized (if serialized-value
                       (deserialize-app-state serialized-value)
                       (initial-state))]
    (js/console.log "serialized state" serialized)
    (reset! state serialized)
    (start)))
