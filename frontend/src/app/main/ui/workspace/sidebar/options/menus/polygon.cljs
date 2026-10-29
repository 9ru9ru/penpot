;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui.workspace.sidebar.options.menus.polygon
  "Point count and star ratio of a polygon or star."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.math :as mth]
   [app.common.types.polygon :as ctpo]
   [app.main.data.workspace.shapes :as dwsh]
   [app.main.store :as st]
   [app.main.ui.components.title-bar :refer [title-bar*]]
   [app.main.ui.ds.controls.numeric-input :refer [numeric-input*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.util.i18n :refer [tr]]
   [rumext.v2 :as mf]))

(mf/defc polygon-menu*
  [{:keys [ids values]}]
  (let [{:keys [kind count ratio]} values
        star? (= kind :star)

        change!
        (mf/use-fn
         (mf/deps ids)
         (fn [attrs]
           (st/emit! (dwsh/update-shapes ids
                                         (fn [shape]
                                           (if-let [params (:polygon shape)]
                                             (ctpo/update-shape shape (merge params attrs))
                                             shape))))))

        on-count-change
        (mf/use-fn
         (mf/deps change!)
         (fn [value]
           (when (number? value)
             (change! {:count value}))))

        on-ratio-change
        (mf/use-fn
         (mf/deps change!)
         (fn [value]
           (when (number? value)
             (change! {:ratio (/ value 100)}))))]

    [:section {:class (stl/css :element-set)
               :aria-label (tr (if star? "workspace.options.polygon.star" "workspace.options.polygon.polygon"))}
     [:div {:class (stl/css :element-title)}
      [:> title-bar* {:collapsable false
                      :title (tr (if star? "workspace.options.polygon.star" "workspace.options.polygon.polygon"))}]]
     [:div {:class (stl/css :polygon-row)}
      [:div {:class (stl/css :polygon-field)
             :title (tr "workspace.options.polygon.count")}
       [:> numeric-input* {:icon i/polygon
                           :min ctpo/min-count
                           :max ctpo/max-count
                           :step 1
                           :value count
                           :on-change on-count-change}]]
      (when star?
        [:div {:class (stl/css :polygon-field)
               :title (tr "workspace.options.polygon.ratio")}
         [:> numeric-input* {:icon i/star
                             :min 1
                             :max 100
                             :step 1
                             :value (mth/round (* 100 ratio))
                             :on-change on-ratio-change}]])]]))
