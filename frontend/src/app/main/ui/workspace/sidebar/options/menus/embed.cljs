;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui.workspace.sidebar.options.menus.embed
  "Playback options for a shape carrying an embed.

  The canvas only ever shows the poster, so this panel is where the
  playback the viewer will perform is configured."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.types.embed :as ctem]
   [app.main.data.workspace.embed :as dwe]
   [app.main.store :as st]
   [app.main.ui.components.title-bar :refer [title-bar*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.controls.switch :refer [switch*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.util.i18n :refer [tr]]
   [rumext.v2 :as mf]))

(def embed-attrs [:embed])

(mf/defc embed-menu*
  [{:keys [ids values]}]
  (let [embed     (get values :embed)
        kind      (get embed :kind)
        open*     (mf/use-state true)
        open?     (deref open*)

        toggle-content
        (mf/use-fn #(swap! open* not))

        on-toggle
        (mf/use-fn
         (mf/deps ids)
         (fn [attr]
           (fn [value]
             (st/emit! (dwe/update-embed ids {attr value})))))

        on-detach
        (mf/use-fn
         (mf/deps ids)
         (fn []
           (st/emit! (dwe/detach-embed ids))))

        on-open-source
        (mf/use-fn
         (mf/deps embed)
         (fn []
           (when-let [url (ctem/watch-url embed)]
             (.open js/window url "_blank" "noopener,noreferrer"))))]

    (when (ctem/playable? embed)
      [:div {:class (stl/css :element-set)}
       [:div {:class (stl/css :element-title)}
        [:> title-bar* {:collapsable true
                        :collapsed (not open?)
                        :on-collapsed toggle-content
                        :aria-expanded open?
                        :aria-controls "embed-content"
                        :title (tr "workspace.options.embed.title")
                        :class (stl/css :title-spacing-embed)}
         [:> icon-button* {:variant "ghost"
                           :aria-label (tr "workspace.options.embed.detach")
                           :on-click on-detach
                           :icon i/detach}]]]

       (when open?
         [:div {:class (stl/css :element-content) :id "embed-content"}
          [:div {:class (stl/css :embed-source)}
           [:span {:class (stl/css :embed-provider)} (ctem/provider-label embed)]
           (when (contains? ctem/iframe-kinds kind)
             [:> icon-button* {:variant "ghost"
                               :aria-label (tr "workspace.options.embed.open-source")
                               :on-click on-open-source
                               :icon i/open-link}])]

          ;; A GIF is replayed by an <img>, which exposes no playback
          ;; controls beyond looping
          (if (= :gif kind)
            [:div {:class (stl/css :embed-row)}
             [:> switch* {:label (tr "workspace.options.embed.loop")
                          :default-checked (true? (:loop embed))
                          :on-change (on-toggle :loop)}]]

            [:*
             [:div {:class (stl/css :embed-row)}
              [:> switch* {:label (tr "workspace.options.embed.autoplay")
                           :default-checked (true? (:autoplay embed))
                           :on-change (on-toggle :autoplay)}]]
             [:div {:class (stl/css :embed-row)}
              [:> switch* {:label (tr "workspace.options.embed.loop")
                           :default-checked (true? (:loop embed))
                           :on-change (on-toggle :loop)}]]
             [:div {:class (stl/css :embed-row)}
              [:> switch* {:label (tr "workspace.options.embed.muted")
                           :default-checked (or (true? (:muted embed))
                                                (true? (:autoplay embed)))
                           :disabled (true? (:autoplay embed))
                           :on-change (on-toggle :muted)}]]
             [:div {:class (stl/css :embed-row)}
              [:> switch* {:label (tr "workspace.options.embed.controls")
                           :default-checked (not (false? (:controls embed)))
                           :on-change (on-toggle :controls)}]]

             (when (true? (:autoplay embed))
               [:div {:class (stl/css :embed-hint)}
                (tr "workspace.options.embed.autoplay-hint")])])])])))
