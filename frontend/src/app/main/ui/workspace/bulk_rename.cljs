;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui.workspace.bulk-rename
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.main.data.modal :as modal]
   [app.main.data.workspace.bulk-rename :as dwbr]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.ds.controls.checkbox :refer [checkbox*]]
   [app.main.ui.icons :as deprecated-icon]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [rumext.v2 :as mf]))

(def ^:private preview-count 6)

(mf/defc text-input*
  {::mf/private true}
  [{:keys [state* id placeholder auto-focus on-key-down]}]
  [:input {:id id
           :class (stl/css :text-input)
           :type "text"
           :auto-focus auto-focus
           :placeholder placeholder
           :default-value @state*
           :on-change #(reset! state* (dom/get-target-val %))
           :on-key-down on-key-down}])

(mf/defc bulk-rename-modal
  {::mf/register modal/components
   ::mf/register-as :bulk-rename}
  [{:keys [ids]}]
  (let [objects     (mf/deref refs/workspace-page-objects)
        names       (mapv #(dm/get-in objects [% :name]) ids)

        rename-to*  (mf/use-state "$&")
        match*      (mf/use-state "")
        start*      (mf/use-state 1)
        regex*      (mf/use-state false)
        case*       (mf/use-state false)

        opts        {:rename-to @rename-to*
                     :match @match*
                     :start @start*
                     :regex? @regex*
                     :case-sensitive? @case*}
        new-names   (dwbr/new-names names opts)
        bad-regex?  (and (seq @match*) (nil? (dwbr/match-regex @match* opts)))
        changed     (->> (map vector ids names new-names)
                         (filter (fn [[_ old new]] (not= old new))))

        on-apply
        (mf/use-fn
         (mf/deps changed)
         (fn []
           (when (seq changed)
             (st/emit! (dwbr/rename-shapes (map (fn [[id _ new]] [id new]) changed))))
           (modal/hide!)))

        on-key-down
        (mf/use-fn
         (mf/deps on-apply)
         (fn [event]
           (cond
             (and (kbd/enter? event) (not (.-isComposing ^js event)))
             (do (dom/prevent-default event) (on-apply))

             (kbd/esc? event)
             (do (dom/prevent-default event) (modal/hide!)))))]

    [:div {:class (stl/css :modal-overlay)}
     [:div {:class (stl/css :modal-container)}
      [:div {:class (stl/css :modal-header)}
       [:h2 {:class (stl/css :modal-title)} (tr "workspace.bulk-rename.title" (count ids))]
       [:button {:class (stl/css :modal-close-btn)
                 :on-click modal/hide!} deprecated-icon/close]]

      [:div {:class (stl/css :modal-content)}
       [:label {:class (stl/css :field)}
        [:span {:class (stl/css :field-label)} (tr "workspace.bulk-rename.rename-to")]
        [:> text-input* {:state* rename-to*
                         :id "bulk-rename-to"
                         :placeholder "$&"
                         :auto-focus true
                         :on-key-down on-key-down}]]

       [:label {:class (stl/css :field)}
        [:span {:class (stl/css :field-label)} (tr "workspace.bulk-rename.match")]
        [:> text-input* {:state* match*
                         :id "bulk-rename-match"
                         :placeholder (tr "workspace.bulk-rename.match-placeholder")
                         :on-key-down on-key-down}]]

       [:div {:class (stl/css :options)}
        [:> checkbox* {:id "bulk-rename-regex"
                       :label (tr "workspace.bulk-rename.regex")
                       :checked @regex*
                       :on-change #(swap! regex* not)}]
        [:> checkbox* {:id "bulk-rename-case"
                       :label (tr "workspace.bulk-rename.case-sensitive")
                       :checked @case*
                       :on-change #(swap! case* not)}]
        [:label {:class (stl/css :start)}
         (tr "workspace.bulk-rename.start")
         [:input {:type "number"
                  :class (stl/css :number-input)
                  :default-value @start*
                  :on-change #(reset! start* (or (d/parse-integer (dom/get-target-val %)) 1))
                  :on-key-down on-key-down}]]]

       [:p {:class (stl/css :hint)} (tr "workspace.bulk-rename.hint")]

       (when bad-regex?
         [:p {:class (stl/css :error)} (tr "workspace.bulk-rename.bad-regex")])

       [:ul {:class (stl/css :preview)}
        (for [[index [old new]] (d/enumerate (take preview-count (map vector names new-names)))]
          [:li {:key index :class (stl/css-case :preview-row true :unchanged (= old new))}
           [:span {:class (stl/css :old-name)} old]
           [:span {:class (stl/css :arrow)} "→"]
           [:span {:class (stl/css :new-name)} new]])
        (when (> (count names) preview-count)
          [:li {:class (stl/css :preview-more)}
           (tr "workspace.bulk-rename.more" (- (count names) preview-count))])]]

      [:div {:class (stl/css :modal-footer)}
       [:button {:class (stl/css :cancel-button) :on-click modal/hide!}
        (tr "labels.cancel")]
       [:button {:class (stl/css :accept-button)
                 :disabled (or bad-regex? (empty? changed))
                 :on-click on-apply}
        (tr "workspace.bulk-rename.apply" (count changed))]]]]))
