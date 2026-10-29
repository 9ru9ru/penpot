;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui.workspace.viewport.smart-selection
  "Handles of the smart selection: a bar in each gap to drag the spacing,
  and a dot in the middle of each layer to drag it to another slot."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.main.data.workspace.smart-selection :as dwss]
   [app.main.data.workspace.tidy :as tidy]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.util.dom :as dom]
   [app.util.object :as obj]
   [rumext.v2 :as mf]))

(def ^:private color "var(--color-accent-quaternary)")

(defn- gap-handles
  "One handle per gap: {:axis :x|:y :x :y :width :height} in canvas
  coordinates, the area of the gap between two neighbours."
  [{:keys [rows hgap vgap] :as arr}]
  (let [{:keys [col-x col-w row-y row-h]} (tidy/grid arr)]
    (concat
     (when (pos? (count (filter #(> (count %) 1) rows)))
       (for [[ri row] (map-indexed vector rows)
             ci       (range (dec (count row)))]
         {:axis :x
          :x (+ (nth col-x ci) (nth col-w ci))
          :y (nth row-y ri)
          :width hgap
          :height (nth row-h ri)}))
     (for [ri (range (dec (count rows)))
           ci (range (min (count (nth rows ri)) (count (nth rows (inc ri)))))]
       {:axis :y
        :x (nth col-x ci)
        :y (+ (nth row-y ri) (nth row-h ri))
        :width (nth col-w ci)
        :height vgap}))))

(mf/defc gap-handle*
  {::mf/private true}
  [{:keys [handle zoom]}]
  (let [{:keys [axis x y width height]} handle
        hover* (mf/use-state false)
        hover? (deref hover*)
        horiz? (= axis :x)
        ;; Leave room to grab a gap that is (nearly) closed.
        min-hit (/ 8 zoom)
        hit-w  (if horiz? (max width min-hit) width)
        hit-h  (if horiz? height (max height min-hit))
        hit-x  (- (+ x (/ width 2)) (/ hit-w 2))
        hit-y  (- (+ y (/ height 2)) (/ hit-h 2))
        cx     (+ x (/ width 2))
        cy     (+ y (/ height 2))
        len    (/ 12 zoom)

        on-pointer-down
        (mf/use-fn
         (mf/deps axis)
         (fn [event]
           (when (dom/left-mouse? event)
             (dom/stop-propagation event)
             (st/emit! (dwss/start-gap-drag axis)))))]

    [:g {:on-pointer-enter #(reset! hover* true)
         :on-pointer-leave #(reset! hover* false)}
     [:rect {:x hit-x :y hit-y :width hit-w :height hit-h
             :class (stl/css-case :gap-area true :gap-x horiz? :gap-y (not horiz?))
             :fill color
             :fill-opacity (if hover? 0.2 0)
             :on-pointer-down on-pointer-down}]
     [:line {:x1 (if horiz? cx (- cx (/ len 2)))
             :y1 (if horiz? (- cy (/ len 2)) cy)
             :x2 (if horiz? cx (+ cx (/ len 2)))
             :y2 (if horiz? (+ cy (/ len 2)) cy)
             :class (stl/css :gap-bar)
             :stroke color
             :stroke-width (/ 2 zoom)
             :stroke-linecap "round"}]]))

(mf/defc reorder-dot*
  {::mf/private true}
  [{:keys [rect zoom]}]
  (let [id (:id rect)
        on-pointer-down
        (mf/use-fn
         (mf/deps id)
         (fn [event]
           (when (dom/left-mouse? event)
             (dom/stop-propagation event)
             (st/emit! (dwss/start-reorder-drag id)))))]
    [:circle {:cx (+ (:x rect) (/ (:width rect) 2))
              :cy (+ (:y rect) (/ (:height rect) 2))
              :r (/ 4 zoom)
              :class (stl/css :reorder-dot)
              :fill "var(--app-white)"
              :stroke color
              :stroke-width (/ 1.5 zoom)
              :on-pointer-down on-pointer-down}]))

(mf/defc gap-label*
  {::mf/private true}
  [{:keys [drag zoom]}]
  (let [{:keys [gap point]} drag
        text   (dm/str gap)
        width  (/ (+ 12 (* 7 (count text))) zoom)
        height (/ 20 zoom)
        x      (+ (:x point) (/ 12 zoom))
        y      (+ (:y point) (/ 12 zoom))]
    [:g {:class (stl/css :gap-label)}
     [:rect {:x x :y y :width width :height height :rx (/ 4 zoom) :fill color}]
     [:text {:x (+ x (/ width 2))
             :y (+ y (/ 14 zoom))
             :font-size (/ 12 zoom)
             :fill "var(--app-white)"
             :text-anchor "middle"}
      text]]))

(mf/defc smart-selection*
  [{:keys [shapes zoom]}]
  (let [;; Not derefed: the shapes already change on every edit.
        objects   (deref refs/workspace-page-objects)
        drag      (mf/deref refs/smart-selection)
        transform (mf/deref refs/current-transform)
        arr       (mf/with-memo [shapes]
                    (dwss/arrangement objects shapes))]
    (cond
      (and drag (:gap drag))
      [:> gap-label* {:drag drag :zoom zoom}]

      (and arr (nil? transform))
      [:g {:class (stl/css :smart-selection)}
       (for [[i handle] (map-indexed vector (gap-handles arr))]
         [:> gap-handle* {:key (dm/str "gap-" i) :handle handle :zoom zoom}])
       (for [rect (apply concat (:rows arr))]
         [:> reorder-dot* {:key (dm/str (:id rect)) :rect rect :zoom zoom}])])))

(obj/set! smart-selection* "displayName" "smart-selection*")
