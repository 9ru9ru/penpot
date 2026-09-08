;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui.workspace.viewport.embeds
  "Play badges for shapes carrying an embed.

  Playback happens in the viewer, so on the canvas an embed is just its
  poster. The badge is what tells the two apart."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.common.types.embed :as ctem]
   [app.main.refs :as refs]
   [rumext.v2 :as mf]))

;; Screen-space size of the badge; divided by the zoom so it stays put
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
        gy      (+ y (/ size 2))]
    [:g {:class (stl/css :embed-badge)}
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

(mf/defc embed-badges*
  "Badge every visible embed of the current page."
  [{:keys [zoom]}]
  (let [objects (mf/deref refs/workspace-page-objects)
        shapes  (mf/with-memo [objects]
                  (->> (vals objects)
                       (filter (fn [shape] (ctem/playable? (:embed shape))))
                       (remove :hidden)
                       (vec)))]
    (when (seq shapes)
      [:g {:class (stl/css :embed-badges)}
       (for [shape shapes]
         [:> embed-badge* {:key (dm/str (:id shape))
                           :shape shape
                           :zoom zoom}])])))
