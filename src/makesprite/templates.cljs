(ns makesprite.templates)

(defn templates-match? [a b]
  (let [check-keys [:text :values]]
    (= (select-keys a check-keys) (select-keys b check-keys))))

(defn get-matching-template
  "Find a template with the same text and values as the passed in prompt."
  [templates prompt]
  (->> templates
       (filter #(templates-match? % prompt))
       first))

(defn save-template [*state prompt template-name]
  (let [prompt (if (:id prompt)
                 prompt
                 (assoc prompt :id (make-id)))]
    (update-in *state [:templates]
               #(->> %
                     (remove (fn [t] (templates-match? t prompt)))
                     (concat [(assoc prompt
                                     :name template-name
                                     :lastModifed (now))])))))

(defn delete-template [*state template]
  (update-in *state [:templates]
             (fn [templates]
               (remove #(templates-match? % template) templates))))

(defn use-template [*state template]
  (-> *state
      (update-in [:ui]
                 assoc
                 :prompt (dissoc template :id :lastModified)
                 :screen :home)))

(defn component:template-name-modal [_state prompt existing-template res _err]
  (let [save-name (r/atom (:name existing-template))]
    (fn []
      [:div.modal
       {:on-click #(when (= (aget % "currentTarget")
                            (aget % "target"))
                     (res))}
       [:div
        [:div.spread
         [:span]
         [:span
          [icon {:class "right clickable"
                 :on-click #(res)}
           (rc/inline "tabler/outline/x.svg")]]]
        [:label "Template name"
         [:input {:placeholder "Template name..."
                  :auto-focus true
                  :value @save-name
                  :on-change #(reset! save-name (-> % .-target .-value))}]]
        [:div.spread
         [:span]
         [:action-buttons
          [:button
           {:title "Cancel"
            :on-click #(res)}
           [icon (rc/inline "tabler/outline/x.svg")]
           "Cancel"]
          [:button
           {:title "Save template"
            :on-click #(res {:save @save-name :prompt prompt})
            :disabled (empty? @save-name)}
           [icon (rc/inline "tabler/outline/device-floppy.svg")]
           "Save"]]]]])))

(defn initiate-save-template! [state prompt]
  (p/let [existing-template (get-matching-template (:templates @state) prompt)
          result (p/create
                   (fn [res err]
                     (swap! state show-modal
                            [component:template-name-modal
                             state prompt existing-template res err])))]
    (if (:save result)
      (swap! state
             #(-> %
                  (save-template prompt (:save result))
                  (show-modal [component:message state "Template saved!"])))
      (swap! state close-modal))))

(defn component:templates [state]
  [:<>
   [:h2 "Templates"]
   (doall
     (for [template (:templates @state)]
       ^{:key (:id template)}
       [:section.template
        [:h3 (:name template)]
        [:blockquote (:text template)]
        [:ul
         (for [k (extract-variables (:text template))]
           [:li {:key k} k " = " "\"" (get-in template [:values k]) "\""])]
        [:action-buttons
         [:button
          {:title "Delete this template"
           :on-click #(swap! state
                             (fn [*state]
                               (-> *state
                                   (show-modal
                                     [component:message state "Template deleted"])
                                   (delete-template template))))}
          [icon (rc/inline "tabler/outline/x.svg")]
          "Delete"]
         [:button
          {:title "Use this template"
           :on-click #(swap! state use-template template)}
          [icon (rc/inline "tabler/outline/check.svg")]
          "Use"]]]))])
