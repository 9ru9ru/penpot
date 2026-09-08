;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui.workspace.sidebar.options.drawing.stroke
  "Stroke the pen, path, line and arrow tools will draw with.

  Picking one of those tools used to leave the right sidebar empty,
  so there was no way to choose a width or a colour before drawing —
  only after, by selecting the finished shape."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.main.data.workspace.colors :as dc]
   [app.main.data.workspace.drawing.common :as dwdc]
   [app.main.store :as st]
   [app.main.ui.components.title-bar :refer [title-bar*]]
   [app.main.ui.workspace.sidebar.options.rows.stroke-row :refer [stroke-row*]]
   [app.util.i18n :refer [tr]]
   [cuerdas.core :as str]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def ^:private ref:draw-stroke
  (l/derived dwdc/get-draw-stroke st/state))

(defn- change-attrs
  [attrs]
  (st/emit! (dwdc/update-draw-stroke attrs)))

(defn- change-attr
  "Adapt a `stroke-row*` callback, which is shaped for a stroke at an
  index inside a shape, to the single stroke held as a preference."
  [attr]
  (fn [_index value]
    (when-not (str/empty? value)
      (change-attrs {attr value}))))

(defn- change-cap
  "Caps take nil for \"none\", so unlike the other attributes they must
  not be filtered out as empty."
  [attr]
  (fn [_index value]
    (change-attrs {attr value})))

(mf/defc options*
  [{:keys [tool]}]
  (let [stroke       (mf/deref ref:draw-stroke)
        disable-drag (mf/use-state false)

        on-focus     (mf/use-fn #(reset! disable-drag true))
        on-blur      (mf/use-fn #(reset! disable-drag false))

        on-color-change
        (mf/use-fn
         (fn [_index color]
           ;; Reuse the shape-level conversion so a library colour, a
           ;; gradient or an opacity change lands the same way here.
           (let [shape (dc/update-shape-stroke-color {:strokes [stroke]} 0 color)]
             (change-attrs (get-in shape [:strokes 0])))))

        on-color-detach
        (mf/use-fn
         (fn [_index]
           (change-attrs {:stroke-color-ref-id nil
                          :stroke-color-ref-file nil})))

        ;; There is only ever one stroke here, so "remove" means going
        ;; back to what a freshly drawn path would have had.
        on-remove
        (mf/use-fn
         (fn [_index]
           (st/emit! (dwdc/update-draw-stroke dwdc/default-draw-stroke))))

        on-cap-switch
        (mf/use-fn
         (mf/deps stroke)
         (fn [_index]
           (change-attrs {:stroke-cap-start (:stroke-cap-end stroke)
                          :stroke-cap-end (:stroke-cap-start stroke)})))]

    [:div {:class (stl/css :element-set)}
     [:div {:class (stl/css :element-title)}
      [:> title-bar* {:collapsable false
                      :title (tr "workspace.options.draw-stroke")
                      :class (stl/css :title-spacing-draw-stroke)}]]

     [:div {:class (stl/css :element-content)}
      [:> stroke-row* {:index 0
                       :ids []
                       :stroke stroke
                       :title (tr "workspace.options.stroke-color")
                       ;; caps only mean something on the open paths
                       ;; these tools produce
                       :show-caps true
                       :on-color-change on-color-change
                       :on-color-detach on-color-detach
                       :on-remove on-remove
                       :on-stroke-width-change (change-attr :stroke-width)
                       :on-stroke-dash-change (change-attr :stroke-dash)
                       :on-stroke-gap-change (change-attr :stroke-gap)
                       :on-stroke-style-change (change-attr :stroke-style)
                       :on-stroke-alignment-change (change-attr :stroke-alignment)
                       :on-stroke-cap-start-change (change-cap :stroke-cap-start)
                       :on-stroke-cap-end-change (change-cap :stroke-cap-end)
                       :on-stroke-cap-switch on-cap-switch
                       :disable-drag disable-drag
                       :on-focus on-focus
                       :on-blur on-blur}]

      [:div {:class (stl/css :draw-stroke-hint)}
       (if (= tool :arrow)
         (tr "workspace.options.draw-stroke.arrow-hint")
         (tr "workspace.options.draw-stroke.hint"))]]]))
