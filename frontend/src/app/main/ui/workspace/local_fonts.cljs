;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui.workspace.local-fonts
  "Pick fonts installed on this computer and add them to the team fonts."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.main.data.fonts :as df]
   [app.main.data.local-fonts :as dlf]
   [app.main.data.modal :as modal]
   [app.main.data.notifications :as ntf]
   [app.main.store :as st]
   [app.main.ui.ds.controls.checkbox :refer [checkbox*]]
   [app.main.ui.icons :as deprecated-icon]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [tr]]
   [beicon.v2.core :as rx]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

(def ^:private max-listed 200)

(mf/defc family-row*
  {::mf/private true}
  [{:keys [family styles checked installed on-toggle]}]
  [:li {:class (stl/css :family-row)}
   [:> checkbox* {:id (str "local-font-" family)
                  :checked checked
                  :disabled installed
                  :on-change #(on-toggle family)}]
   [:label {:for (str "local-font-" family)
            :class (stl/css :family-name)
            :style {:font-family (str "\"" family "\"")}}
    family]
   [:span {:class (stl/css :family-meta)}
    (if installed
      (tr "workspace.local-fonts.already-added")
      (tr "workspace.local-fonts.styles" (count styles)))]])

(mf/defc local-fonts-modal
  {::mf/register modal/components
   ::mf/register-as :local-fonts}
  [{:keys [team-id]}]
  (let [fonts*     (mf/use-state nil)
        error*     (mf/use-state nil)
        search*    (mf/use-state "")
        selected*  (mf/use-state #{})
        uploading* (mf/use-state false)
        fonts      @fonts*
        search     @search*
        selected   @selected*
        uploading? @uploading*

        team-families
        (mf/with-memo []
          (into #{} (map :font-family) (vals (:fonts @st/state))))

        families
        (mf/with-memo [fonts search]
          (let [term (str/lower search)]
            (->> (group-by :family fonts)
                 (filter (fn [[family _]]
                           (or (str/blank? term)
                               (str/includes? (str/lower family) term))))
                 (sort-by first))))

        on-toggle
        (mf/use-fn
         (fn [family]
           (swap! selected* #(if (contains? % family) (disj % family) (conj % family)))))

        on-add
        (mf/use-fn
         (mf/deps fonts selected team-id)
         (fn []
           (let [chosen (filterv #(contains? selected (:family %)) fonts)]
             (reset! uploading* true)
             (->> (dlf/upload chosen team-id (:fonts @st/state))
                  (rx/reduce conj [])
                  (rx/subs!
                   (fn [added]
                     (run! #(st/emit! (df/add-font %)) added)
                     (modal/hide!)
                     (st/emit! (df/fetch-fonts team-id)
                               (ntf/success (tr "workspace.local-fonts.added" (count selected)))))
                   (fn [cause]
                     (reset! uploading* false)
                     (js/console.error "Uploading local fonts" cause)
                     (st/emit! (ntf/error (tr "workspace.local-fonts.upload-error")))))))))]

    (mf/with-effect []
      (let [sub (->> (dlf/query)
                     (rx/subs! #(reset! fonts* %)
                               #(reset! error* (or (ex-message %) (str %)))))]
        #(rx/dispose! sub)))

    [:div {:class (stl/css :modal-overlay)}
     [:div {:class (stl/css :modal-container)}
      [:div {:class (stl/css :modal-header)}
       [:h2 {:class (stl/css :modal-title)} (tr "workspace.local-fonts.title")]
       [:button {:class (stl/css :modal-close-btn)
                 :on-click modal/hide!} deprecated-icon/close]]

      [:div {:class (stl/css :modal-content)}
       [:p {:class (stl/css :hint)} (tr "workspace.local-fonts.hint")]
       (cond
         (some? @error*)
         [:p {:class (stl/css :error)} (tr "workspace.local-fonts.denied")]

         (nil? fonts)
         [:p {:class (stl/css :hint)} (tr "workspace.local-fonts.loading")]

         :else
         [:*
          [:input {:class (stl/css :search-input)
                   :type "search"
                   :auto-focus true
                   :data-testid "local-fonts-search"
                   :placeholder (tr "workspace.local-fonts.search")
                   :on-change #(reset! search* (dom/get-target-val %))}]
          [:ul {:class (stl/css :family-list)}
           (for [[family styles] (take max-listed families)]
             [:> family-row* {:key family
                              :family family
                              :styles styles
                              :installed (contains? team-families family)
                              :checked (contains? selected family)
                              :on-toggle on-toggle}])]
          (when (> (count families) max-listed)
            [:p {:class (stl/css :hint)} (tr "workspace.local-fonts.more" (- (count families) max-listed))])])]

      [:div {:class (stl/css :modal-footer)}
       [:button {:class (stl/css :cancel-button) :on-click modal/hide!}
        (tr "labels.cancel")]
       [:button {:class (stl/css :accept-button)
                 :data-testid "local-fonts-add"
                 :disabled (or uploading? (empty? selected))
                 :on-click on-add}
        (if uploading?
          (tr "workspace.local-fonts.uploading")
          (tr "workspace.local-fonts.add" (count selected)))]]]]))

(defn open!
  [team-id]
  (st/emit! (modal/show :local-fonts {:team-id team-id})))

(def available? dlf/supported?)

