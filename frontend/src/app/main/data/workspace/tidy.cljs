;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.data.workspace.tidy
  "Figma's tidy up: the selection is read as rows and columns from where
  the layers are now, then laid out on a grid with even spacing."
  (:require
   [app.common.data :as d]
   [app.common.geom.rect :as grc]
   [app.common.math :as mth]
   [app.main.data.helpers :as dsh]
   [app.main.data.workspace.transforms :as dwt]
   [app.main.data.workspace.undo :as dwu]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

(defn- bounds
  [shape]
  (-> shape :points grc/points->rect))

(defn- group-rows
  "Groups rects into rows: a rect joins the current row while its vertical
  center falls inside the row's band."
  [rects]
  (->> (sort-by :y rects)
       (reduce (fn [rows r]
                 (let [row (peek rows)
                       top (when row (apply min (map :y row)))
                       bot (when row (apply max (map #(+ (:y %) (:height %)) row)))
                       cy  (+ (:y r) (/ (:height r) 2))]
                   (if (and row (<= top cy bot))
                     (conj (pop rows) (conj row r))
                     (conj rows [r]))))
               [])
       (mapv #(vec (sort-by :x %)))))

(defn- median
  [xs]
  (when (seq xs)
    (let [xs (vec (sort xs))]
      (nth xs (quot (count xs) 2)))))

(defn- spacing
  "The typical gap between neighbours, never negative."
  [gaps]
  (max 0 (mth/round (or (median gaps) 0))))

(defn layout
  "New top-left corner of each rect ({:id :x :y :width :height}),
  keeping the rows and columns they are in now."
  [rects]
  (let [rows     (group-rows rects)
        ncols    (apply max (map count rows))
        col-w    (mapv (fn [c] (apply max 0 (keep #(some-> (get % c) :width) rows))) (range ncols))
        row-h    (mapv (fn [row] (apply max (map :height row))) rows)
        hgap     (spacing (for [row rows [a b] (partition 2 1 row)]
                            (- (:x b) (+ (:x a) (:width a)))))
        vgap     (spacing (for [[ra rb] (partition 2 1 rows)]
                            (- (apply min (map :y rb))
                               (apply max (map #(+ (:y %) (:height %)) ra)))))
        x0       (apply min (map :x rects))
        y0       (apply min (map :y rects))
        col-x    (reductions + x0 (map #(+ % hgap) col-w))
        row-y    (reductions + y0 (map #(+ % vgap) row-h))]
    (into {}
          (for [[ri row] (d/enumerate rows)
                [ci r]   (d/enumerate row)]
            [(:id r) {:x (nth col-x ci) :y (nth row-y ri)}]))))

(defn tidy-up-selected
  []
  (ptk/reify ::tidy-up-selected
    ptk/WatchEvent
    (watch [_ state _]
      (let [objects  (dsh/lookup-page-objects state)
            selected (dsh/lookup-selected state)
            rects    (->> selected
                          (keep #(get objects %))
                          (map #(assoc (bounds %) :id (:id %))))
            undo-id  (js/Symbol)]
        (when (> (count rects) 1)
          (let [positions (layout rects)]
            (rx/concat
             (rx/of (dwu/start-undo-transaction undo-id))
             (->> (rx/from (vec positions))
                  (rx/map (fn [[id pos]] (dwt/update-position id pos {:absolute? true}))))
             (rx/of (dwu/commit-undo-transaction undo-id)))))))))
