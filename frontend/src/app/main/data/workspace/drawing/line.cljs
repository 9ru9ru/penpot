;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.data.workspace.drawing.line
  "Straight line and arrow drawing.

  Both tools produce a two-point `:path`; the arrow differs only in the
  end cap, which the renderers already know how to draw."
  (:require
   [app.common.geom.point :as gpt]
   [app.common.geom.rect :as grc]
   [app.common.geom.shapes :as gsh]
   [app.common.geom.shapes.flex-layout :as gslf]
   [app.common.geom.shapes.grid-layout :as gslg]
   [app.common.math :as mth]
   [app.common.types.container :as ctn]
   [app.common.types.modifiers :as ctm]
   [app.common.types.path :as path]
   [app.common.types.shape :as cts]
   [app.common.types.shape-tree :as ctst]
   [app.common.types.shape.layout :as ctl]
   [app.common.uuid :as uuid]
   [app.main.data.helpers :as dsh]
   [app.main.data.workspace.drawing.common :as common]
   [app.main.streams :as ms]
   [app.util.mouse :as mse]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

;; Below this a drag reads as a stray click rather than a line, and the
;; shape is discarded instead of being created as a dot.
(def ^:private min-length 2)

(def ^:private snap-step (/ mth/PI 4))

(defn- snap-to-45
  "Project `point` onto the nearest 45° ray from `origin`, the way Figma
  constrains a line while shift is held."
  [origin point]
  (let [v (gpt/to-vec origin point)
        l (gpt/length v)]
    (if (< l min-length)
      point
      (let [angle (mth/round (mth/atan2 (:y v) (:x v)) snap-step)]
        (gpt/add origin (gpt/point (* l (mth/cos angle))
                                   (* l (mth/sin angle))))))))

(defn- update-line
  [state origin point shift?]
  (update-in state [:workspace-drawing :object]
             (fn [object]
               (let [point   (cond-> point shift? (->> (snap-to-45 origin)))
                     content (path/points->content [origin point])
                     selrect (path/calc-selrect content)]
                 (-> object
                     (assoc ::end point)
                     (assoc :content content)
                     (assoc :selrect selrect)
                     (assoc :points (grc/rect->points selrect)))))))

(defn- setup-frame
  "Attach the drawing shape to the board under its start point."
  [origin]
  (ptk/reify ::setup-frame
    ptk/UpdateEvent
    (update [_ state]
      (let [objects   (dsh/lookup-page-objects state)
            frame-id  (->> (ctst/top-nested-frame objects origin)
                           ;; We don't want to change the structure of
                           ;; component copies
                           (ctn/get-first-valid-parent objects)
                           :id)
            flex?     (ctl/flex-layout? objects frame-id)
            grid?     (ctl/grid-layout? objects frame-id)
            drop-index (when flex? (gslf/get-drop-index frame-id objects origin))
            drop-cell  (when grid? (gslg/get-drop-cell frame-id objects origin))]

        (update-in state [:workspace-drawing :object]
                   (fn [object]
                     (-> object
                         (assoc :frame-id frame-id)
                         (assoc :parent-id frame-id)
                         (cond-> (some? drop-index)
                           (with-meta {:index drop-index}))
                         (cond-> (some? drop-cell)
                           (with-meta {:cell drop-cell})))))))))

(defn- normalize-angle
  "Rebuild the segment as a horizontal one carrying the drag angle as the
  shape rotation.

  Stored as a plain diagonal, a line has an axis-aligned bounding box and
  no rotation of its own, so the sidebar reports 0 degrees for a visibly
  slanted line and offers no field to straighten it with. Keeping the
  angle in `:rotation` — the way a line behaves in Figma — makes that
  control read and drive the real angle."
  [shape origin end]
  (let [v      (gpt/to-vec origin end)
        len    (gpt/length v)
        center (gpt/add origin (gpt/scale v 0.5))
        angle  (mod (mth/degrees (mth/atan2 (:y v) (:x v))) 360)
        half   (/ len 2)
        shape  (-> shape
                   (assoc :content (path/points->content
                                    [(gpt/point (- (:x center) half) (:y center))
                                     (gpt/point (+ (:x center) half) (:y center))]))
                   ;; `setup-path` keeps whatever selrect it is handed, so
                   ;; the stale one has to go for it to be recomputed
                   (dissoc :selrect :points)
                   (cts/setup-shape))]
    (gsh/transform-shape shape (ctm/rotation-modifiers shape center angle))))

(defn- finish-drawing
  [origin]
  (ptk/reify ::finish-drawing
    ptk/UpdateEvent
    (update [_ state]
      (update-in state [:workspace-drawing :object]
                 (fn [{:keys [::end] :as shape}]
                   (let [shape (dissoc shape ::end)]
                     (if (or (nil? end)
                             (< (gpt/distance origin end) min-length))
                       (assoc shape :initialized? false)
                       (normalize-angle shape origin end))))))))

(defn handle-drawing
  "Draw a line, or an arrow when `tool` is `:arrow`, by dragging."
  [tool stroke]
  (ptk/reify ::handle-drawing
    ptk/WatchEvent
    (watch [_ _ stream]
      (let [stopper (mse/drag-stopper stream)
            origin  @ms/mouse-position
            stroke  (cond-> stroke
                      (= tool :arrow)
                      (assoc :stroke-cap-end :triangle-arrow))

            shape   (cts/setup-shape
                     {:type :path
                      :initialized? true
                      :frame-id uuid/zero
                      :parent-id uuid/zero
                      :strokes [stroke]
                      :content (path/points->content [origin origin])})]

        (rx/concat
         (rx/of #(update % :workspace-drawing assoc :object shape))
         (->> ms/mouse-position
              (rx/with-latest-from ms/mouse-position-shift)
              (rx/map (fn [[point shift?]]
                        #(update-line % origin point shift?)))
              (rx/take-until stopper))
         (rx/of
          (setup-frame origin)
          (finish-drawing origin)
          (common/handle-finish-drawing)))))))
