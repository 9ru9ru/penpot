;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui.workspace.sidebar.options.menus.mask
  "How a masked group hides its content: by the mask's opacity (alpha)
  or by its brightness (luminance)."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.main.data.workspace.shapes :as dwsh]
   [app.main.store :as st]
   [app.main.ui.components.title-bar :refer [title-bar*]]
   [app.main.ui.ds.controls.select :refer [select*]]
   [app.util.i18n :refer [tr]]
   [rumext.v2 :as mf]))

(mf/defc mask-menu*
  [{:keys [ids mask-type]}]
  (let [options
        (mf/with-memo []
          [{:value "alpha" :label (tr "workspace.options.mask.alpha") :id "alpha"}
           {:value "luminance" :label (tr "workspace.options.mask.luminance") :id "luminance"}])

        on-change
        (mf/use-fn
         (mf/deps ids)
         (fn [value]
           (st/emit! (dwsh/update-shapes ids #(assoc % :mask-type (keyword value))))))]

    [:section {:class (stl/css :element-set)
               :aria-label (tr "workspace.options.mask")}
     [:div {:class (stl/css :element-title)}
      [:> title-bar* {:collapsable false
                      :title (tr "workspace.options.mask")}]]
     [:div {:class (stl/css :mask-row)}
      [:> select* {:default-selected (d/name (or mask-type :alpha))
                   :options options
                   :data-testid "mask.type"
                   :on-change on-change}]]]))
