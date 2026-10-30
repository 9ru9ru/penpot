;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui.workspace.sidebar.options.menus.dev-note
  "Handoff: a note for developers on any layer, and \"ready for dev\" on
  boards. Both show in the inspect tab and the viewer's inspect mode."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.main.data.workspace.shapes :as dwsh]
   [app.main.store :as st]
   [app.main.ui.components.title-bar :refer [title-bar*]]
   [app.main.ui.ds.controls.switch :refer [switch*]]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

(mf/defc dev-note-menu*
  [{:keys [shape]}]
  (let [id     (dm/get-prop shape :id)
        board? (= :frame (dm/get-prop shape :type))
        note   (:dev-annotation shape)
        ready? (true? (:dev-ready shape))

        on-note-blur
        (mf/use-fn
         (mf/deps id note)
         (fn [event]
           (let [value (str/trim (dom/get-target-val event))]
             (when (not= value (or note ""))
               (st/emit! (dwsh/update-shapes [id] #(if (str/blank? value)
                                                     (dissoc % :dev-annotation)
                                                     (assoc % :dev-annotation value))))))))

        on-ready-change
        (mf/use-fn
         (mf/deps id)
         (fn [value]
           (st/emit! (dwsh/update-shapes [id] #(if value
                                                 (assoc % :dev-ready true)
                                                 (dissoc % :dev-ready))))))]

    [:section {:class (stl/css :element-set)
               :aria-label (tr "workspace.options.dev-note")}
     [:div {:class (stl/css :element-title)}
      [:> title-bar* {:collapsable false
                      :title (tr "workspace.options.dev-note")}]]
     (when board?
       [:div {:class (stl/css :ready-row)}
        [:> switch* {:label (tr "workspace.options.dev-ready")
                     :default-checked ready?
                     :key (str id ready?)
                     :on-change on-ready-change}]])
     [:textarea {:class (stl/css :note-input)
                 :key (str id)
                 :data-testid "dev-note"
                 :default-value (or note "")
                 :placeholder (tr "workspace.options.dev-note-placeholder")
                 :on-blur on-note-blur}]]))
