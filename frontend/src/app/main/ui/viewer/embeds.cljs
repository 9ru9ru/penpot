;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui.viewer.embeds
  "Playable media layered on top of the viewer canvas.

  Neither renderer can draw a video: Skia has no video decoder and SVG
  `<image>` holds no player. So embeds are plain HTML positioned over
  the canvas, which also makes this work identically for the SVG and the
  wasm viewport."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.common.files.helpers :as cfh]
   [app.common.types.embed :as ctem]
   [app.config :as cf]
   [rumext.v2 :as mf]))

(def ^:private iframe-allow
  "autoplay; fullscreen; picture-in-picture; encrypted-media")

(defn- hidden?
  "True when the shape or any of its ancestors is hidden."
  [objects shape]
  (or (:hidden shape)
      (->> (cfh/get-parents objects (:id shape))
           (some :hidden)
           (boolean))))

(defn- collect-embeds
  "Playable, visible embeds inside `frame-id`, in render order."
  [objects frame-id]
  (when (some? frame-id)
    (->> (cfh/get-children-ids-with-self objects frame-id)
         (keep (fn [id] (get objects id)))
         (filter (fn [shape] (ctem/playable? (:embed shape))))
         (remove (fn [shape] (hidden? objects shape))))))

(defn- embed-style
  "Place the untransformed shape rect over the canvas, then reapply the
  shape transform around its centre.

  The shape matrix lives in page units and is applied around the shape
  centre, so under a uniform zoom only its translation part needs
  scaling — the linear part is unchanged by the conjugation."
  [shape origin zoom]
  (let [{:keys [x y width height transform opacity]} shape]
    (cond-> {:position "absolute"
             :left (* (- x (:x origin)) zoom)
             :top (* (- y (:y origin)) zoom)
             :width (* width zoom)
             :height (* height zoom)
             :opacity (or opacity 1)}

      (some? transform)
      (assoc :transform (dm/fmt "matrix(%, %, %, %, %, %)"
                                (:a transform) (:b transform)
                                (:c transform) (:d transform)
                                (* (:e transform) zoom)
                                (* (:f transform) zoom))
             :transform-origin "center"))))

(mf/defc embed-player*
  {::mf/private true}
  [{:keys [embed]}]
  (let [{:keys [kind media-id autoplay loop muted controls]} embed]
    (if (contains? ctem/iframe-kinds kind)
      [:iframe {:class (stl/css :embed-frame)
                :src (ctem/player-url embed)
                :title (ctem/provider-label embed)
                :allow iframe-allow
                :allowFullScreen true
                :referrerPolicy "strict-origin-when-cross-origin"
                :frameBorder "0"}]

      (let [uri (cf/resolve-file-media {:id media-id})]
        (case kind
          :video
          [:video {:class (stl/css :embed-media)
                   :src uri
                   :controls (not (false? controls))
                   :autoPlay (true? autoplay)
                   ;; browsers refuse to autoplay audible media
                   :muted (or (true? muted) (true? autoplay))
                   :loop (true? loop)
                   :playsInline true
                   :preload "metadata"}]

          :gif
          [:img {:class (stl/css :embed-media)
                 :src uri
                 :alt ""
                 :draggable false}]

          nil)))))

(mf/defc embeds-overlay*
  "Render every playable embed of `frame` on top of the viewer canvas.

  `origin` is the bounds origin the canvas was laid out from — the
  `size` map the viewer already computes — so the two stay aligned even
  when shadows or blurs pad the frame."
  [{:keys [objects frame origin zoom]}]
  (let [embeds (mf/with-memo [objects frame]
                 (collect-embeds objects (:id frame)))]
    (when (seq embeds)
      [:div {:class (stl/css :embeds-overlay)}
       (for [shape embeds]
         [:div {:key (dm/str (:id shape))
                :class (stl/css-case :embed-item true
                                     ;; a gif has nothing to click, so it
                                     ;; must not swallow prototype hotspots
                                     :embed-passthrough (= :gif (:kind (:embed shape))))
                :style (embed-style shape origin zoom)}
          [:> embed-player* {:embed (:embed shape)}]])])))
