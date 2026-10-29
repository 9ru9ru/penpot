;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui.workspace.viewport.outline-view
  "Figma's outline mode (Ctrl+Y): the rendered canvas is hidden and every
  visible shape is drawn as a hairline, images as crossed boxes and text
  as outlined glyphs, so stacked or hidden-behind shapes show up."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.common.geom.shapes :as gsh]
   [app.common.uuid :as uuid]
   [app.main.ui.workspace.viewport.outline :refer [outline*]]
   [rumext.v2 :as mf]))

(def ^:private line-color "#4c4f55")

(mf/defc text-outline*
  {::mf/private true
   ::mf/wrap [mf/memo]}
  [{:keys [shape zoom]}]
  (let [position-data (:position-data shape)]
    (if (seq position-data)
      [:g {:transform (gsh/transform-str shape)}
       (for [[index data] (d/enumerate position-data)]
         [:text {:key index
                 :x (:x data)
                 :y (:y data)
                 :dominant-baseline "ideographic"
                 :text-length (:width data)
                 :length-adjust "spacingAndGlyphs"
                 :fill "none"
                 :stroke line-color
                 :stroke-width (/ 0.6 zoom)
                 :style {:font-family (:font-family data)
                         :font-size (:font-size data)
                         :font-weight (:font-weight data)
                         :font-style (:font-style data)
                         :white-space "pre"}}
          (:text data)])]
      [:> outline* {:shape shape :zoom zoom :color line-color}])))

(mf/defc image-outline*
  {::mf/private true
   ::mf/wrap [mf/memo]}
  [{:keys [shape zoom]}]
  (let [[p1 p2 p3 p4] (:points shape)
        width (/ 0.6 zoom)]
    [:g
     [:> outline* {:shape shape :zoom zoom :color line-color}]
     (when (and p1 p3)
       [:line {:x1 (dm/get-prop p1 :x) :y1 (dm/get-prop p1 :y)
               :x2 (dm/get-prop p3 :x) :y2 (dm/get-prop p3 :y)
               :stroke line-color :stroke-width width}])
     (when (and p2 p4)
       [:line {:x1 (dm/get-prop p2 :x) :y1 (dm/get-prop p2 :y)
               :x2 (dm/get-prop p4 :x) :y2 (dm/get-prop p4 :y)
               :stroke line-color :stroke-width width}])]))

(defn- hidden-ids
  "Hidden shapes and everything inside them."
  [objects]
  (into #{}
        (comp (filter :hidden)
              (mapcat #(cons (:id %) (cfh/get-children-ids objects (:id %)))))
        (vals objects)))

(mf/defc outline-view*
  [{:keys [objects zoom vbox background]}]
  (let [hidden (mf/with-memo [objects] (hidden-ids objects))
        shapes (mf/with-memo [objects hidden]
                 (->> (vals objects)
                      (remove #(= uuid/zero (:id %)))
                      (remove #(contains? hidden (:id %)))
                      (vec)))]
    [:g {:class (stl/css :outline-view)
         :clip-path "url(#clip-handlers)"}
     [:rect {:x (:x vbox) :y (:y vbox)
             :width (:width vbox) :height (:height vbox)
             :fill (or background "#E8E9EA")}]
     (for [shape shapes]
       (let [id (:id shape)]
         (case (:type shape)
           :text  [:> text-outline* {:key (dm/str id) :shape shape :zoom zoom}]
           :image [:> image-outline* {:key (dm/str id) :shape shape :zoom zoom}]
           [:> outline* {:key (dm/str id) :shape shape :zoom zoom :color line-color}])))]))
