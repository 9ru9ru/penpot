;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.data.workspace.smart-selection
  "Figma's smart selection. When the selected layers are already tidy
  (rows and columns with one spacing), dragging a gap changes the
  spacing of all of them at once, and dragging the dot in the middle of
  a layer moves it to another slot."
  (:require
   [app.common.geom.point :as gpt]
   [app.common.math :as mth]
   [app.common.types.modifiers :as ctm]
   [app.common.types.shape.layout :as ctl]
   [app.main.constants :as mconst]
   [app.main.data.helpers :as dsh]
   [app.main.data.workspace.modifiers :as dwm]
   [app.main.data.workspace.tidy :as tidy]
   [app.main.features :as features]
   [app.main.streams :as ms]
   [app.util.mouse :as mse]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

(defn- shape-rect
  [shape]
  (assoc (tidy/bounds shape) :id (:id shape)))

(defn arrangement
  "The arrangement of `shapes` when smart selection applies to them:
  two or more layers, none of them placed by a flex or grid layout, all
  already where tidy up would put them. Otherwise nil."
  [objects shapes]
  (when (and (> (count shapes) 1)
             (not-any? #(ctl/any-layout-immediate-child? objects %) shapes))
    (let [rects (mapv shape-rect shapes)]
      (when (tidy/tidy? rects)
        (tidy/arrange rects)))))

(defn- move-tree
  "A modifiers tree that moves each rect to its new top-left corner."
  [objects rects positions]
  (let [deltas (into {}
                     (map (fn [{:keys [id x y]}]
                            (let [p (get positions id)]
                              [id (gpt/point (- (:x p) x) (- (:y p) y))])))
                     rects)]
    (dwm/build-modif-tree (keys deltas) objects
                          #(ctm/move-modifiers (get deltas (:id %))))))

(defn- set-drag
  [drag]
  (ptk/reify ::set-drag
    ptk/UpdateEvent
    (update [_ state]
      (if drag
        (-> state
            (assoc-in [:workspace-local :smart-selection] drag)
            (assoc-in [:workspace-local :transform] :move))
        (update state :workspace-local dissoc :smart-selection :transform)))))

(defn- drag
  "For each mouse point `step` gives {:preview positions :commit
  positions :info drag-info}. Previews them until the mouse is released,
  then commits the last :commit positions."
  [state stream step]
  (let [objects (dsh/lookup-page-objects state)
        rects   (->> (dsh/lookup-selected state)
                     (keep #(get objects %))
                     (mapv shape-rect))
        stopper (mse/drag-stopper stream)
        steps   (->> ms/mouse-position
                     (rx/filter some?)
                     (rx/map step)
                     (rx/take-until stopper)
                     (rx/share))
        preview (->> steps
                     (rx/sample mconst/move-sample-time)
                     (rx/mapcat (fn [{:keys [preview info]}]
                                  (let [tree (move-tree objects rects preview)]
                                    (rx/of (set-drag info)
                                           (if (features/active-feature? state "render-wasm/v1")
                                             (dwm/set-wasm-modifiers tree {:ignore-snap-pixel true})
                                             (dwm/set-modifiers tree false true)))))))
        commit  (->> steps
                     (rx/last)
                     (rx/mapcat (fn [{:keys [commit]}]
                                  (let [tree (move-tree objects rects commit)]
                                    (if (features/active-feature? state "render-wasm/v1")
                                      (rx/of (dwm/apply-wasm-modifiers tree {:ignore-snap-pixel true}))
                                      (rx/of (dwm/set-modifiers tree false true)
                                             (dwm/apply-modifiers)))))))]
    (rx/concat
     (rx/merge preview commit)
     (rx/of (set-drag nil)))))

(defn start-gap-drag
  "Drag a gap of the selection: `axis` :x changes the space between
  columns, :y the space between rows. Whole pixels, never negative."
  [axis]
  (ptk/reify ::start-gap-drag
    ptk/WatchEvent
    (watch [_ state stream]
      (let [objects (dsh/lookup-page-objects state)
            shapes  (->> (dsh/lookup-selected state) (keep #(get objects %)))
            arr     (arrangement objects shapes)
            start   @ms/mouse-position
            key     (if (= axis :x) :hgap :vgap)]
        (when (and arr start)
          (drag state stream
                (fn [pt]
                  (let [delta (if (= axis :x) (- (:x pt) (:x start)) (- (:y pt) (:y start)))
                        gap   (max 0 (mth/round (+ (get arr key) delta)))
                        pos   (tidy/positions (assoc arr key gap))]
                    {:preview pos :commit pos :info {:axis axis :gap gap :point pt}}))))))))

(defn- slots
  "Center of every slot of the arrangement, in reading order."
  [arr]
  (let [{:keys [col-x col-w row-y row-h]} (tidy/grid arr)]
    (vec (for [[ri row] (map-indexed vector (:rows arr))
               ci       (range (count row))]
           (gpt/point (+ (nth col-x ci) (/ (nth col-w ci) 2))
                      (+ (nth row-y ri) (/ (nth row-h ri) 2)))))))

(defn- rechunk
  "Puts the rects back into rows of the same sizes as before."
  [rects sizes]
  (loop [rects rects sizes sizes rows []]
    (if (empty? sizes)
      rows
      (recur (drop (first sizes) rects) (rest sizes) (conj rows (vec (take (first sizes) rects)))))))

(defn start-reorder-drag
  "Drag one layer of the selection to another slot: the others close
  up around it, and it lands in the slot nearest the mouse."
  [id]
  (ptk/reify ::start-reorder-drag
    ptk/WatchEvent
    (watch [_ state stream]
      (let [objects (dsh/lookup-page-objects state)
            shapes  (->> (dsh/lookup-selected state) (keep #(get objects %)))
            arr     (arrangement objects shapes)
            start   @ms/mouse-position]
        (when (and arr start)
          (let [order  (vec (apply concat (:rows arr)))
                sizes  (mapv count (:rows arr))
                moved  (some #(when (= id (:id %)) %) order)
                others (filterv #(not= id (:id %)) order)
                centers (slots arr)]
            (when moved
              (drag state stream
                    (fn [pt]
                      (let [slot      (apply min-key #(gpt/distance pt (nth centers %)) (range (count centers)))
                            order     (into (conj (subvec others 0 slot) moved) (subvec others slot))
                            positions (tidy/positions (assoc arr :rows (rechunk order sizes)))]
                        {:preview (assoc positions id {:x (+ (:x moved) (- (:x pt) (:x start)))
                                                       :y (+ (:y moved) (- (:y pt) (:y start)))})
                         :commit  positions
                         :info    {:reorder id :slot slot}}))))))))))
