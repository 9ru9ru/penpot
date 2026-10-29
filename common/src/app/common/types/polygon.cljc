;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.common.types.polygon
  "Regular polygons and stars, as Figma draws them. They are `:path`
  shapes whose `:polygon` attribute ({:kind :polygon|:star :count n
  :ratio r}) says how to build the path again when the count or the star
  ratio changes. The shape fills its box: a triangle's base sits on the
  bottom edge and its tip touches the top."
  (:require
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.geom.rect :as grc]
   [app.common.geom.shapes.common :as gco]
   [app.common.math :as mth]
   [app.common.types.path :as path]))

(def min-count 3)
(def max-count 60)

(def defaults
  {:polygon {:kind :polygon :count 3 :ratio 1}
   :star    {:kind :star :count 5 :ratio 0.382}})

(defn- unit-points
  "Corners on the unit circle, starting at the top and going clockwise;
  every other one pulled in to `ratio` for a star."
  [{:keys [kind count ratio]}]
  (let [n (if (= kind :star) (* 2 count) count)]
    (for [i (range n)]
      (let [a (- (* i (/ (* 2 mth/PI) n)) (/ mth/PI 2))
            r (if (and (= kind :star) (odd? i)) ratio 1)]
        (gpt/point (* r (mth/cos a)) (* r (mth/sin a)))))))

(defn- fit
  "Scales and moves `points` so their bounding box is `rect`."
  [points {:keys [x y width height]}]
  (let [xs (map :x points)
        ys (map :y points)
        x0 (apply min xs)
        y0 (apply min ys)
        sx (/ width (max 0.0001 (- (apply max xs) x0)))
        sy (/ height (max 0.0001 (- (apply max ys) y0)))]
    (mapv #(gpt/point (+ x (* sx (- (:x %) x0)))
                      (+ y (* sy (- (:y %) y0))))
          points)))

(defn normalize
  [params]
  (-> params
      (update :count #(-> (or % min-count) (mth/round) (mth/clamp min-count max-count)))
      (update :ratio #(-> (or % 1) (mth/clamp 0.01 1)))))

(defn content
  "The path of a polygon or star filling `rect`."
  [params rect]
  (-> (unit-points (normalize params))
      (fit rect)
      (path/points->content :close true)))

(defn update-shape
  "Rebuilds the path of a polygon shape from new `params`, keeping its
  box, rotation and flips."
  [shape params]
  (let [params    (normalize params)
        selrect   (:selrect shape)
        content   (content params selrect)
        transform (cond-> (:transform shape (gmt/matrix))
                    (:flip-x shape) (gmt/scale (gpt/point -1 1))
                    (:flip-y shape) (gmt/scale (gpt/point 1 -1)))
        content   (path/transform-content content (gmt/transform-in (gco/shape->center shape) transform))]
    (-> shape
        (assoc :polygon params)
        (path/update-geometry content))))

(defn shape-in-rect
  "Attributes for a new, unrotated polygon shape filling `rect`."
  [params rect]
  (let [content (content params rect)
        selrect (grc/make-rect rect)]
    {:polygon (normalize params)
     :content content
     :selrect selrect
     :points  (grc/rect->points selrect)}))
