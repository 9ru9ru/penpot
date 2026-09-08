;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui.workspace.viewport.embeds
  "Play badges, and in-place playback, for shapes carrying an embed.

  The player lives in a `foreignObject` inside the widget layer, so it
  inherits the viewport's own vbox transform and pans and zooms with the
  canvas without any coordinate maths of its own. That also makes it
  work identically under the SVG and the wasm renderer, since both keep
  this widget layer."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.common.geom.shapes :as gsh]
   [app.common.types.embed :as ctem]
   [app.main.data.workspace.embed :as dwe]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.viewer.embeds :refer [embed-player*]]
   [app.util.dom :as dom]
   [okulary.core :as l]
   [rumext.v2 :as mf]))

(def ^:private ref:active-embed
  (l/derived :workspace-active-embed st/state))

;; Screen-space size of the badge; divided by the zoom so it holds still
;; while the canvas scales.
(def ^:private badge-size 24)
(def ^:private badge-margin 6)

(mf/defc embed-badge*
  {::mf/private true}
  [{:keys [shape zoom]}]
  (let [size    (/ badge-size zoom)
        margin  (/ badge-margin zoom)
        selrect (get shape :selrect)
        x       (+ (:x1 selrect) margin)
        y       (+ (:y1 selrect) margin)
        ;; a play triangle inscribed in the badge, nudged right so it
        ;; reads as centred
        glyph   (/ size 3)
        gx      (+ x (/ size 2.6))
        gy      (+ y (/ size 2))

        on-click
        (mf/use-fn
         (mf/deps (:id shape))
         (fn [event]
           (dom/stop-propagation event)
           (st/emit! (dwe/toggle-embed (:id shape)))))]

    [:g {:class (stl/css :embed-badge)
         :on-click on-click
         :on-double-click dom/stop-propagation}
     [:rect {:x x
             :y y
             :width size
             :height size
             :rx (/ size 4)
             :class (stl/css :embed-badge-bg)}]
     [:polygon {:points (dm/fmt "%,% %,% %,%"
                                gx (- gy (/ glyph 2))
                                gx (+ gy (/ glyph 2))
                                (+ gx (* glyph 0.85)) gy)
                :class (stl/css :embed-badge-glyph)}]]))

(mf/defc embed-canvas-player*
  {::mf/private true}
  [{:keys [shape]}]
  (let [stop-events
        (mf/use-fn
         (fn [event]
           ;; A same-origin <video> would otherwise bubble its control
           ;; clicks up to the viewport, which reads them as an
           ;; interrupt and stops playback on the first click.
           (dom/stop-propagation event)))]

    [:foreignObject {:x (dm/get-prop shape :x)
                     :y (dm/get-prop shape :y)
                     :width (dm/get-prop shape :width)
                     :height (dm/get-prop shape :height)
                     :transform (gsh/transform-str shape)}
     [:div {:class (stl/css :embed-canvas-player)
            :on-pointer-down stop-events
            :on-mouse-down stop-events
            :on-double-click stop-events}
      [:> embed-player* {:embed (:embed shape)}]]]))

(mf/defc embed-badges*
  "Badge every visible embed of the current page, and play the active one."
  [{:keys [zoom]}]
  (let [objects (mf/deref refs/workspace-page-objects)
        active  (mf/deref ref:active-embed)

        shapes  (mf/with-memo [objects]
                  (->> (vals objects)
                       (filter (fn [shape] (ctem/playable? (:embed shape))))
                       (remove :hidden)
                       (vec)))

        playing (get objects active)]

    (when (seq shapes)
      [:g {:class (stl/css :embed-badges)}
       (for [shape shapes]
         [:> embed-badge* {:key (dm/str (:id shape))
                           :shape shape
                           :zoom zoom}])

       (when (and (some? playing)
                  (ctem/playable? (:embed playing)))
         [:> embed-canvas-player* {:shape playing}])])))
