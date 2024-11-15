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
    ["react-intersection-observer" :refer [InView]]
    ["jszip" :as JSZip]))

(def w (t/writer :json))
(def r (t/reader :json))

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
   :templates {} ; currently unusued / commented out
   :favourites []
   :ui {:prompt {:text placeholder-prompt :values {}}
        :click-mode :sprite
        :screen :home}
   :log []})

(defn now [] (-> (js/Date.) .toISOString))

(defn make-id [] (str (random-uuid)))

(defn is-valid-key? [k]
  (and k
       (= (.slice k 0 3) "sk-")
       (> (aget k "length") 45)))

(defn hex [typedarray]
  (let [a (js/Array.from (js/Uint8Array. typedarray))]
    (-> (.map a
              (fn [b]
                (-> b
                    (.toString 16)
                    (.padStart 2 "0"))))
        (.join ""))))

(defn sha256 [src]
  (p/let [b (-> (js/TextEncoder.) (.encode (.toString src)))
          digest (j/call-in js/crypto [:subtle :digest] "SHA-256" b)]
    (hex digest)))

(defn obj->sha256 [obj]
  (p/let [h (hash obj)]
    (sha256 h)))

(defn short-id [id]
  (let [parts (-> id str (.split "-"))]
    (str (first parts) "-" (second parts))))

(defn get-log [id logs]
  (->> logs
       (filter #(= (:id %) id))
       first))

(defn load-image [url]
  (js/Promise.
    (fn [res err]
      (let [img (js/Image.)]
        (aset img "onload" #(res img))
        (aset img "onerror" err)
        (aset img "src" url)))))

(defn canvas-to-blob [canvas & [image-type quality]]
  (js/Promise.
    (fn [res _err]
      (.toBlob canvas res image-type quality))))

(defn resize-canvas-to-image! [canvas img]
  (aset canvas "width" (aget img "width"))
  (aset canvas "height" (aget img "height")))

(defn image-to-blob
  [img]
  (p/let [canvas (.createElement js/document "canvas")
          ctx (.getContext canvas "2d")
          w (aget img "width")
          h (aget img "height")]
    (js/console.log "img" img)
    (resize-canvas-to-image! canvas img)
    (.drawImage ctx img 0 0 w h)
    (canvas-to-blob canvas)))

(defn flood-fill-image-background
  "Flood fill image blob corners and edges to remove the background."
  [url & [additional-points]]
  (p/let [img (load-image url)
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
      (doseq [[x y] [[0 0] [w 0] [w h] [0 h]
                     [0 h2] [w h2]
                     [w2 0] [w2 h]]]
        (.fill ff "rgba(0,0,0,0)" x y 50))
      (doseq [[x y] additional-points]
        (.fill ff "rgba(0,0,0,0)" x y 50))
      (.putImageData ctx (aget ff "imageData") 0 0))
    (js/console.log "canvas-to-blob")
    (canvas-to-blob canvas)))

(defn image-blob-to-jpeg-blog [image-blob]
  (p/let [img (load-image (js/URL.createObjectURL image-blob))
          canvas (.createElement js/document "canvas")
          ctx (.getContext canvas "2d")
          w (aget img "width")
          h (aget img "height")]
    (resize-canvas-to-image! canvas img)
    (.drawImage ctx img 0 0 w h)
    (canvas-to-blob canvas "image/jpeg" 0.75)))

(defn process-b64-image-and-store! [result]
  (p/let [image-data-b64 (get-in result [:data 0 :b64_json])
          image-array (js/Uint8Array.from (js/atob image-data-b64)
                                          #(.charCodeAt % 0))
          image-blob (js/Blob. #js [image-array])
          image-id (make-id)]
    (kv/set (str "image-" image-id) image-blob)
    ; flood fill the corners and edges
    (p/let [processed-image-blob (flood-fill-image-background
                                   (js/URL.createObjectURL image-blob))]
      (kv/set (str "image-processed-" image-id) processed-image-blob)
      (js/console.log "image-processed saved"))
    (-> result
        (update-in [:data 0] dissoc :b64_json)
        (assoc :image-id image-id))))

(defn remove-error [*state]
  (update-in *state [:ui] dissoc :error-message))

(def re-mustache (js/RegExp. "{{(.*?)}}" "m"))

(defn extract-variables [txt]
  (when txt
    (->> txt
         (re-seq re-mustache)
         (map second)
         set
         vec)))

(defn replace-vars [prompt-text values]
  (let [vars (extract-variables prompt-text)]
    (reduce
      (fn [txt v]
        (.replaceAll txt (str "{{" v "}}") (get values v)))
      prompt-text
      vars)))

(defn initiate-request [state]
  (let [prompt (get-in @state [:ui :prompt])
        text (str
               prompt-dall-e-strict "\n"
               (replace-vars (:text prompt) (:values prompt)))
        log-entry {:id (make-id)
                   :k :dall-e-request
                   :t (now)
                   :prompt-text text
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
        (js/console.error "API error:" err)
        (swap! state
               #(-> %
                    (dissoc :inflight)
                    (assoc-in [:ui :error-message]
                              (or
                                (j/get err :message)
                                (j/get-in err [:error :message])
                                (.toString err)))))))))

(defn notify [el]
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

(defn copy-canvas-to-clipboard! [canvas]
  (p/let [sprite-blob (canvas-to-blob canvas)
          clipboard-item (js/ClipboardItem.
                           (clj->js {"image/png" sprite-blob}))]
    (js/navigator.clipboard.write #js [clipboard-item])))

(defn download-file [blob filename]
  (let [url (js/URL.createObjectURL blob)
        link (js/document.createElement "a")]
    (aset link "href" url)
    (aset link "download" filename)
    (js/document.body.appendChild link)
    (.click link)
    (js/document.body.removeChild link)
    (js/URL.revokeObjectURL url)))

(defn download-canvas [canvas sprite-name]
  (p/let [blob (canvas-to-blob canvas)]
    (download-file blob (str sprite-name ".png"))))

(defn update-textbox-height [height element]
  (when element
    (let [el (if (aget element "style") element element)
          css-height (aget (.. el -style) "height")]
      (aset (.. el -style) "height" "auto")
      (let [sh (aget el "scrollHeight")
            expanded (+ sh 8)]
        (aset (.. el -style) "height" css-height)
        (when (not= @height expanded)
          (reset! height expanded))))))

(defn has-favourite [*state id]
  (->> (get-in *state [:favourites])
       (filterv #(= id %))
       first))

(defn del-favourite [*state id]
  (update-in *state [:favourites]
             #(filterv
               (fn [fav-id] (not= id fav-id))
               %)))

(defn add-favourite [*state id]
  (update-in *state [:favourites] conj id))

(defn show-modal [*state component]
  (assoc-in *state [:ui :modal] component))

(defn close-modal [*state]
  (update-in *state [:ui] dissoc :modal))

(defn update-log-entry [*state match-log callback]
  (update-in *state [:log]
             #(mapv
                (fn [log]
                  (if (= (:id log) (:id match-log))
                    (callback log)
                    log))
                %)))

; *** components *** ;

(defn icon
  ([attrs svg]
   [:inline-icon
    (merge {:dangerouslySetInnerHTML
            {:__html (.replace svg re-html-comment "")}}
           attrs)])
  ([svg] (icon {} svg)))

; *** views & event handlers ***;

(defn component:settings-warning [state]
  (let [openai-key (get-in @state [:settings :openai-key])]
    (when (empty? openai-key)
      [:p.error
       [icon (rc/inline "tabler/outline/alert-circle.svg")]
       "No OpenAI API key set. "
       [:a {:href "#"
            :on-click (fn [ev]
                        (.preventDefault ev)
                        (swap! state assoc-in [:ui :screen] :settings))}
        "Click here to update the settings"] "."])))

(defn set-prompt! [state prompt where & [post-render-cb]]
  (let [coords [:ui :prompt]
        coords (if where (conj coords where) coords)]
    (swap! state
           #(-> %
                (assoc-in coords prompt)
                (assoc-in [:ui :screen] :home))))
  (p/do!
    (p/delay 0)
    (update-textbox-height (r/cursor state [:ui :prompt :height])
                           (js/document.querySelector "#prompt"))
    (when post-render-cb (post-render-cb))))

(defn component:prompt [state]
  (let [height (r/cursor state [:ui :prompt :height])]
    (fn []
      (let [txt (get-in @state [:ui :prompt :text])
            variables (extract-variables txt)]
        [:<>
         [:textarea
          {:id "prompt"
           :auto-focus true
           :rows 3
           :ref #(update-textbox-height height %)
           :style {:height @height}
           :read-only (seq (:inflight @state))
           :placeholder "Enter your game sprite prompt here..."
           :on-change #(let [el (-> % .-target)]
                         (set-prompt! state
                                      (aget el "value")
                                      :text))
           :value txt}]
         (doall
           (for [v variables]
             (let [id (str "var-" v)
                   display-name (.replaceAll v "_" " ")]
               [:label {:for id
                        :key id}
                display-name
                [:input {:name id
                         :id id
                         :disabled (seq (:inflight @state))
                         :placeholder display-name
                         :value (get-in @state [:ui :prompt :values v])
                         :on-change
                         #(swap! state assoc-in [:ui :prompt :values v]
                                 (-> % .-target .-value))}]])))]))))

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

      [extracted-image-data @bounding-box])))

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

(defn canvas-click [state log image-id ev]
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
        click-mode (get-in @state [:ui :click-mode])]
    (js/console.log "color-clicked" color-clicked)
    (case click-mode
      :sprite
      (when (not= color-clicked '(0 0 0 0))
        (swap! state assoc-in [:ui :extracted-sprite] {:loading true})
        (p/let [_ (p/delay 1)
                [sprite-image-data bounding-box]
                (multi-pass-flood-fill-and-extract
                  canvas xs ys [0 0 0 0])]
          (js/console.log "extracted" sprite-image-data)
          (swap! state assoc-in [:ui :extracted-sprite]
                 {:img-data sprite-image-data
                  :image-id image-id
                  :bounding-box bounding-box})
          ; copy to clipboard
          (when (aget js/window "ClipboardItem")
            (p/let [sprite-canvas (doto (.createElement js/document "canvas")
                                    (aset "width"
                                          (aget sprite-image-data "width"))
                                    (aset "height"
                                          (aget sprite-image-data "height")))
                    _ctx (doto (.getContext sprite-canvas "2d")
                           (.putImageData sprite-image-data 0 0))]
              (copy-canvas-to-clipboard! sprite-canvas)
              (swap! state assoc-in [:ui :extracted-sprite :copied] true))))
        (js/console.log "done extract"))
      :background
      (let [ff (floodfill. img-data)]
        (.fill ff "rgba(0,0,0,0)" xs ys 50)
        (.putImageData ctx (aget ff "imageData") 0 0)
        (p/let [blob (canvas-to-blob canvas)
                t (now)]
          (kv/set (str "image-processed-" image-id) blob)
          (swap! state update-log-entry log
                 #(update-in % [:fills] conj [xs ys t]))
          (js/console.log "log state after fill"
                          (get-log (:id log) (:log @state))))))))

(defn component:image [state log image-id parent]
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
          [:canvas.chequerboard
           {:id (str "canvas-" image-id)
            :ref #(mount-canvas % img)
            :on-click #(canvas-click state log image-id %)}]
          [:div (get-in parent [:prompt :text])])))))

(defn delete-log-entry [*state log]
  (let [parent-id (:parent log)
        log-id (:id log)
        image-id (get-in log [:response :image-id])
        t (now)
        deleted {:deleted true
                 :lastModified t}]
    (kv/set (str "image-" image-id) deleted)
    (kv/set (str "image-processed-" image-id) deleted)
    (update-in *state [:log]
               #(mapv
                  (fn [log]
                    (if (or (= (:id log) parent-id)
                            (= (:id log) log-id))
                      (merge log deleted)
                      log))
                  %))))

(defn revert-to-original-image! [state log]
  (p/let [image-id (get-in log [:response :image-id])
          original-image-blob (kv/get (str "image-" image-id))
          canvas (js/document.getElementById (str "canvas-" image-id))
          ctx (.getContext canvas "2d")
          processed-image-blob (flood-fill-image-background
                                 (js/URL.createObjectURL original-image-blob))
          img (load-image (js/URL.createObjectURL processed-image-blob))]
    (.drawImage ctx img 0 0)
    (kv/set (str "image-processed-" image-id) processed-image-blob)
    (swap! state update-log-entry log #(dissoc % :fills))))

(defn component:message-modal [state msg]
  (let [done-fn #(swap! state close-modal)]
    [:div.modal
     {:on-click #(when (= (aget % "currentTarget")
                          (aget % "target"))
                   (done-fn))}
     [:div
      [:div.spread
       [:span]
       [:span
        [icon {:class "right clickable"
               :on-click #(done-fn)}
         (rc/inline "tabler/outline/x.svg")]]]
      [:p msg]
      [:div.spread
       [:span]
       [:action-buttons
        [:button
         {:title "Done"
          :on-click #(done-fn)}
         [icon (rc/inline "tabler/outline/check.svg")]
         "Ok"]]]]]))

(defn component:dialog-modal [state msg message-icon callback]
  (let [done-fn (fn [& [result]]
                  (callback result)
                  (swap! state close-modal))]
    [:div.modal
     {:on-click #(when (= (aget % "currentTarget")
                          (aget % "target"))
                   (done-fn))}
     [:div
      [:div.spread
       [:span
        [icon message-icon]]
       [:span
        [icon {:class "right clickable"
               :on-click #(done-fn)}
         (rc/inline "tabler/outline/x.svg")]]]
      [:p msg]
      [:div.spread
       [:span]
       [:action-buttons
        [:button
         {:title "Cancel"
          :on-click #(done-fn)}
         [icon (rc/inline "tabler/outline/x.svg")]
         "Cancel"]
        [:button
         {:title "Ok"
          :on-click #(done-fn true)}
         [icon (rc/inline "tabler/outline/check.svg")]
         "Ok"]]]]]))

(defn show-confirm-modal! [state message icon]
  (js/Promise. (fn [res]
                 (swap! state show-modal
                        [component:dialog-modal state message icon res]))))

(defn component:log-item [log state show?]
  (fn [] ; has to be in a function to isolate the InView observer
    (let [parent (get-log (:parent log) (:log @state))
          image-id (get-in log [:response :image-id])
          favourite (has-favourite @state (:id log))]
      [:generated-image
       [:> InView {:as "span"
                   :on-change (fn [inView _entry]
                                ;(js/console.log "inView" inView entry)
                                (when inView
                                  (reset! show? true)))}
        [:span.spread
         (let [click-mode (get-in @state [:ui :click-mode])]
           [:action-buttons
            [icon
             {:title "Extract sprite mode"
              :class (when (= click-mode :sprite) "selected")
              :on-click #(swap! state update-in [:ui] assoc :click-mode :sprite)}
             (rc/inline "tabler/outline/body-scan.svg")]
            [icon
             {:title "Erase background mode"
              :class (when (= click-mode :background) "selected")
              :on-click #(swap! state update-in [:ui] assoc
                                :click-mode :background)}
             (rc/inline "tabler/outline/eraser.svg")]])
         [:action-buttons
          (let [i (rc/inline "tabler/outline/trash.svg")]
            [icon
             {:title "Delete image"
              :on-click #(p/let [confirm (show-confirm-modal!
                                           state
                                           "Delete this image?" i)]
                           (when confirm (swap! state delete-log-entry log)))}
             i])
          (let [i (rc/inline "tabler/outline/arrow-back-up.svg")]
            [icon
             {:title "Revert image"
              :on-click #(p/let [confirm (show-confirm-modal!
                                           state
                                           "Revert to original image?" i)]
                           (when confirm (revert-to-original-image! state log)))}
             i])
          [icon
           {:title "Download image"
            :on-click #(download-canvas
                         (js/document.getElementById (str "canvas-" image-id))
                         (str "sprites-" (short-id image-id)))}
           (rc/inline "tabler/outline/download.svg")]
          [icon
           {:data-notification-text "Prompt copied!"
            :title "Copy prompt to clipboard"
            :on-click #(let [el (-> % .-currentTarget)
                             prompt (get-in parent [:prompt])
                             prompt-text (replace-vars (:text prompt)
                                                       (:values prompt))]
                         (copy-text el prompt-text)
                         (notify el))}
           (rc/inline "tabler/outline/copy.svg")]
          [icon
           {:title "Re-run prompt"
            :on-click (fn [_ev]
                        (let [prompt (:prompt parent)
                              prompt (if (= (type prompt) js/String)
                                       {:text prompt}
                                       prompt)]
                          (set-prompt! state prompt nil
                                       #(.scrollIntoView
                                          (js/document.querySelector "#prompt")
                                          true))))}
           (rc/inline "tabler/outline/refresh.svg")]
          [icon
           {:title (if favourite "Un-favourite" "Favourite")
            :on-click #(swap! state
                              (if favourite del-favourite add-favourite)
                              (:id log))}
           (if favourite
             (rc/inline "tabler/filled/heart.svg")
             (rc/inline "tabler/outline/heart.svg"))]]]
        [:div.result
         (when @show?
           [component:image state log image-id parent])]]])))

(defn component:log [state & [favourites?]]
  [:ul.log
   (doall
     (for [log (reverse (:log @state))]
       (when (and (not (:deleted log))
                  (or (not favourites?)
                      (has-favourite @state (:id log))))
         [:li {:key (:id log)}
          (case (:k log)
            :dall-e-response [component:log-item log state (r/atom false)]
            #_#_ :dall-e-request [:span (get-in log [:prompt :text])]
            nil)])))])

(defn component:extracted-sprite [state]
  (when-let [{:keys [img-data copied loading image-id bounding-box]}
             (get-in @state [:ui :extracted-sprite])]
    (let [close-fn #(swap! state update-in [:ui] dissoc :extracted-sprite)]
      [:sprite-dialog.modal
       {:on-click #(when (= (aget % "currentTarget")
                            (aget % "target"))
                     (close-fn))}
       [:div
        [:div.spread
         [:span (when copied "Sprite copied!")]
         [:span
          [icon {:class "right clickable"
                 :on-click close-fn}
           (rc/inline "tabler/outline/x.svg")]]]
        (if loading
          [:span [icon {:class "spin"}
                  (rc/inline "tabler/outline/spiral.svg")]
           "extracting"]
          [:<>
           [:canvas#extracted-sprite.chequerboard
            {:data-notification-text "Sprite copied to clipboard!"
             :ref #(mount-canvas-sprite % img-data)}]
           [:div.spread
            [:span]
            [:action-buttons
             (when (aget js/window "ClipboardItem")
               [:button
                {:data-notification-text "Sprite copied!"
                 :title "Copy sprite to clipboard"
                 :on-click #(let [el (-> % .-currentTarget)
                                  canvas (js/document.getElementById
                                           "extracted-sprite")]
                              (copy-canvas-to-clipboard! canvas)
                              (notify el))}
                [icon (rc/inline "tabler/outline/copy.svg")]
                "Copy"])
             [:button
              {:title "Download sprite"
               :on-click #(p/let [canvas (js/document.getElementById
                                           "extracted-sprite")
                                  bounding-box-hash (obj->sha256 bounding-box)]
                            (download-canvas canvas
                                             (str "sprite-"
                                                  (short-id image-id) "-"
                                                  (.substr
                                                    bounding-box-hash
                                                    0 8))))}
              [icon (rc/inline "tabler/outline/download.svg")]
              "Download"]]]])]])))

(defn component:done-button [state & [label]]
  [:button
   {:on-click #(swap! state assoc-in [:ui :screen] :home)}
   (or label "Done")])

(defn blob-to-b64 [blob]
  (js/Promise.
    (fn [res _err]
      (let [reader (js/FileReader.)]
        (set! (.-onload reader)
              (fn [e]
                (let [data-url (.-result e.target)
                      base64-str (second (.split data-url ","))]
                  (res base64-str))))
        (.readAsDataURL reader blob)))))

(defn serialize-log-image [log]
  (when-let [image-id (get-in log [:response :image-id])]
    (p/let [
            image-blob (kv/get (str "image-" image-id))
            jpeg-blob (image-blob-to-jpeg-blog image-blob)
            image-b64 (blob-to-b64 jpeg-blob)
            ;image-processed (kv/get (str "image-processed-" image-id))
            ;image-processed-b64 (blob-to-b64 image-processed)
            ]
      [image-id image-b64])))

(defn generate-export! [state]
  (swap! state assoc-in [:ui :generating-export] true)
  (p/let [favourites (set (get-in @state [:favourites]))
          log (get-in @state [:log])
          log-items (filterv #(contains? favourites (:id %)) log)
          parent-log-items (mapv #(get-log (:parent %) log) log-items)
          log-items (concat log-items parent-log-items)
          ;log-items (get-in @state [:log])
          serialized-images (p/all (map serialize-log-image log-items))
          log-items-transit (t/write w log-items)
          zip (JSZip.)]
    (.file zip "makesprite.transit.json" log-items-transit)
    (let [img-folder (.folder zip "images")]
      (doseq [[id b64] (remove nil? serialized-images)]
        (.file img-folder (str "image-" id ".jpg") b64 #js {:base64 true})))
    (p/let [content (.generateAsync zip #js {:type "blob"})]
      (download-file content "makesprite-export.zip")
      (swap! state update-in [:ui] dissoc :generating-export))))

(defn component:settings [state]
  (let [openai-key (get-in @state [:settings :openai-key])]
    [:<>
     [:h2 "Settings"]
     [:label {:for "openai-key"} "OpenAI API key:"]
     [:input#openai-key
      {:type "password"
       :placeholder "Enter OpenAI API key..."
       :name "openai-key"
       :value openai-key
       :on-change #(swap! state assoc-in [:settings :openai-key]
                          (-> % .-target .-value))
       :class (when (not (is-valid-key? openai-key)) "warning")}]
     [:p
      [:small [:a {:href "https://platform.openai.com/api-keys"
                   :target "_BLANK"}
               "Get an API key here"] "."
       " Your key stays in your browser and is not sent to any server except OpenAI."]]
     [:action-buttons
      [component:done-button state "Set key"]]
     [:h3 "About"]
     [:p "Makesprite runs 100% in the browser. It is a static client-side app with no server component.
         It is hosted on GitHub pages. Prompts are sent to the OpenAI servers and run on DALL-E.
         Any images you generate are stored in IndexedDb in your browser only.
         If you want to back up your images use the Export function below."]
     [:p "Makesprite is written in " [:a {:href "https://clojurescript.org"} "ClojureScript"] " and the"
      [:a {:href "https://github.com/chr15m/makesprite"} " source code is available on GitHub"] "."]
     [:h3 "Export"]
     [:button
      {:on-click #(generate-export! state)}
      (if (get-in @state [:ui :generating-export])
        [icon
         {:class "spin"}
         (rc/inline "tabler/outline/spiral.svg")]
        [icon (rc/inline "tabler/outline/download.svg")])
      "Export favourites"]
     [:h3 "Reset"]
     (let [i (rc/inline "tabler/outline/alert-circle.svg")]
       [:button
        {:on-click
         #(p/let [confirm (show-confirm-modal!
                            state
                            [:span.error "Reset the app and DELETE ALL DATA?"] i)]
            (when confirm
              (p/do!
                (kv/clear)
                ; (reset! state nil)
                (.reload js/location))))}
        [icon (rc/inline "tabler/outline/alert-circle.svg")]
        "Reset the app"])
     [:h3 "Legal"]
     [:p "This tool comes with " [:strong "absolutely no warranty"] "."
      " You are completely responsible for your use of this software"
      " and any content you produce with it."]
     [:p "This tool does not generate images. It uses your OpenAI"
      " keys to access the OpenAI API and images are generated by DALL-E."]
     [:p
      "Recent rulings in some jurisdictions indicate that generated"
      " content may be considered " [:strong "public domain"] "."]
     [:h3 "Build"]
     [:p "Build ID: " [:code (rc/inline "build.txt")]]]))

(defn component:action-buttons [state]
  (let [openai-key (get-in @state [:settings :openai-key])
        disabled (or (empty? (get-in @state [:ui :prompt :text]))
                     (not (is-valid-key? openai-key))
                     (seq (:inflight @state)))]
    [:div.spread
     #_ (let [prompt (get-in @state [:ui :prompt])
           txt (get-in @state [:ui :prompt :text])
           variables (extract-variables txt)]
       [:button {:on-click #(initiate-save-template! state prompt)
                 :disabled (or disabled (empty? variables))}
        [icon (rc/inline "tabler/outline/template.svg")]
        "save template"])
     [:action-buttons
      [:button {:on-click
                #(set-prompt! state {:text ""
                                     :values {}}
                              nil)
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
          "send"])]]]))

(defn component:error-message [state]
  (when-let [msg (get-in @state [:ui :error-message])]
    [:p.error.spread
     [:span
      [icon (rc/inline "tabler/outline/alert-circle.svg")]
      msg]
     [icon {:class "right clickable"
            :on-click #(swap! state remove-error)}
      (rc/inline "tabler/outline/x.svg")]]))

(defn component:discord [state]
  (let [discord-link "https://discord.gg/egyRd6wd73"]
    [:<>
     [:h2 "Discord"]
     [:p
      [:a {:href discord-link
           :target "_BLANK"}
       "Join the Discord"]
      " to share prompts and sprites."]
     [:action-buttons.spread
      [:a {:href discord-link
           :target "_BLANK"}
       [:button
        [icon (rc/inline "tabler/filled/brand-discord.svg")]
        "Join"]]
      [component:done-button state "Close"]]]))

(defn component:favourites [state]
  [:<>
   [:div
    [component:extracted-sprite state]
    [:h2 "Favourites"]
    [component:settings-warning state]]
   [:div
    (if (empty? (:favourites @state))
      [:p "You have not favourited any images yet."]
      [component:log state true])]])

(defn component:home [state]
  [:<>
   [:div
    [component:extracted-sprite state]
    [component:settings-warning state]
    [component:prompt state (r/atom nil)]
    [component:action-buttons state]
    [component:error-message state]]
   [:div
    [component:log state]]])

(defn component:main [state]
  (let [screen (get-in @state [:ui :screen])]
    [:main
     [:<>
      (let [modal (get-in @state [:ui :modal])]
        [:<> modal])]
     (case screen
       :settings [component:settings state]
       :discord [component:discord state]
       :favourites [component:favourites state]
       ;:templates [component:templates state]
       [component:home state])]))

(defn component:footer []
  [:footer
   "Copyright 2024 "
   [:a.preserve
    {:href "https://mccormick.cx"
     :target "_BLANK"}
    "Chris McCormick"
    [:img.me
     {:src (str "https://mccormick.cx/gfx/"
                "chris-mccormick-software-developer-thumb.jpg?makesprite")}]]])

(defn component:header [state]
  [:header
   [:nav
    [:ul.spread
     [:li.clickable
      {:on-click #(swap! state assoc-in [:ui :screen] :home)}
      [icon (rc/inline "logo.svg")]
      [:strong "makesprite"]]
     [:li
      [:ul
       [:li
        [:a {:href "https://github.com/chr15m/makesprite"
             :target "_BLANK"}
         [icon
          {:title "GitHub"}
          (rc/inline "tabler/outline/brand-github.svg")]]]
       [:li
        [icon
         {:title "Discord"
          :class "clickable"
          :on-click #(swap! state assoc-in [:ui :screen] :discord)}
         (rc/inline "tabler/outline/brand-discord.svg")]]
       [:li
        [icon
         {:title "Favourites"
          :class "clickable"
          :on-click #(swap! state assoc-in [:ui :screen] :favourites)}
         (if (= (get-in @state [:ui :screen]) :favourites)
           (rc/inline "tabler/filled/heart.svg")
           (rc/inline "tabler/outline/heart.svg"))]]
       #_ [:li
           [icon
            {:title "Templates"
             :class "clickable"
             :on-click #(swap! state assoc-in [:ui :screen] :templates)}
            (rc/inline "tabler/outline/template.svg")]]
       #_ [:li
           [icon
            {:title "Projects"
             :class "clickable"
             :on-click #(swap! state assoc-in [:ui :screen] :folders)}
            (rc/inline "tabler/outline/folders.svg")]]
       [:li
        [icon
         {:title "Generate"
          :class "clickable"
          :on-click #(swap! state assoc-in [:ui :screen] :home)}
         (rc/inline "tabler/outline/photo-search.svg")]]
       [:li
        [icon
         {:title "Settings"
          :class "clickable"
          :on-click #(swap! state assoc-in [:ui :screen] :settings)}
         (rc/inline "tabler/outline/settings.svg")]]]]]]])

; *** launch *** ;

(defonce state (r/atom nil))

(js/console.log "state" @state)

(defn start {:dev/after-load true} []
  (rdom/render
    [:<>
     [component:header state]
     [component:main state]
     [component:footer]]
    (js/document.getElementById "app")))

(defn serialize-app-state [structure]
  (->
    (t/write w structure)
    js/JSON.parse))

(defn deserialize-app-state [transit-object]
  (->> transit-object
       js/JSON.stringify
       (t/read r)))

; clear the saved state
; (kv/del "makesprite-state")

(defn state-watcher [_k _r old-state new-state]
  (when (not= old-state new-state)
    (kv/update "makesprite-state"
               (fn []
                 (-> new-state
                     (dissoc :inflight)
                     (update-in [:ui] dissoc
                                :extracted-sprite :modal
                                :generating-export)
                     serialize-app-state)))))

(defn loading-message [msg]
  (let [m (js/document.querySelector "#loading-message")]
    (aset m "textContent"
          (str "Setting up initial sprites\n"
               msg))))

(defn load-and-store-image [image-id fills]
  (when image-id
    (loading-message (str "Storing image:\n" image-id))
    (p/let [url (str "default-images/image-" image-id ".jpg")
            image (load-image url)
            image-blob (image-to-blob image)]
      (kv/set (str "image-" image-id) image-blob)
      ; flood fill the corners and edges
      (loading-message (str "Storing processed image:\n" image-id))
      (p/let [processed-image-blob (flood-fill-image-background
                                     url
                                     (reverse fills))]
        (kv/set (str "image-processed-" image-id) processed-image-blob)
        (js/console.log "image-processed saved")))))

(defn setup-initial-state []
  (js/console.log "setup initial state")
  (loading-message "")
  (p/let [*state (initial-state)
          _ (loading-message "Loading data")
          res (js/fetch "makesprite-defaults.transit.json")
          transit (.text res)
          log (t/read r transit)]
    (js/console.log "initial log data" log)
    (p/all (map #(load-and-store-image (get-in % [:response :image-id])
                                       (get-in % [:fills]))
                log))
    (assoc *state :log (vec log))))

(defn init []
  (add-watch state :state-watcher #'state-watcher)
  (p/let [serialized-value (kv/get "makesprite-state")
          serialized (if serialized-value
                       (deserialize-app-state serialized-value)
                       (setup-initial-state))]
    (js/console.log "serialized state" serialized)
    (reset! state serialized)
    (start)))
