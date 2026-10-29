;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui.workspace.sidebar.options.menus.easing-editor
  "A cubic-bezier editor: drag the two handles, or type the four numbers."
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.math :as mth]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

;; The box shows x in [0, 1] and y in [-0.5, 1.5].
(def ^:private size 120)
(def ^:private pad 8)
(def ^:private y-min -0.5)
(def ^:private y-max 1.5)

(defn- ->px
  [x y]
  [(+ pad (* x (- size (* 2 pad))))
   (+ pad (* (/ (- y-max y) (- y-max y-min)) (- size (* 2 pad))))])

(defn- ->value
  [px py]
  [(mth/clamp (/ (- px pad) (- size (* 2 pad))) 0 1)
   (mth/clamp (- y-max (* (/ (- py pad) (- size (* 2 pad))) (- y-max y-min))) y-min y-max)])

(defn- round2
  [n]
  (/ (mth/round (* n 100)) 100))

(defn- parse-bezier
  [text]
  (let [nums (->> (re-seq #"-?\d*\.?\d+" text)
                  (map d/parse-double)
                  (remove nil?))]
    (when (= 4 (count nums))
      (let [[x1 y1 x2 y2] nums]
        [(mth/clamp x1 0 1) y1 (mth/clamp x2 0 1) y2]))))

(mf/defc bezier-editor*
  [{:keys [value on-change]}]
  (let [[x1 y1 x2 y2] value
        drag*     (mf/use-ref nil)
        svg-ref   (mf/use-ref nil)
        [p0x p0y] (->px 0 0)
        [p1x p1y] (->px x1 y1)
        [p2x p2y] (->px x2 y2)
        [p3x p3y] (->px 1 1)

        on-pointer-down
        (mf/use-fn
         (fn [event]
           (let [handle (-> (dom/get-current-target event) (dom/get-data "handle"))]
             (mf/set-ref-val! drag* handle)
             (.setPointerCapture (dom/get-current-target event) (.-pointerId ^js event)))))

        on-pointer-move
        (mf/use-fn
         (mf/deps value on-change)
         (fn [event]
           (when-let [handle (mf/ref-val drag*)]
             (let [rect   (.getBoundingClientRect (mf/ref-val svg-ref))
                   [x y]  (->value (- (.-clientX ^js event) (.-left rect))
                                   (- (.-clientY ^js event) (.-top rect)))
                   [a b c d] value]
               (on-change (mapv round2 (if (= handle "1") [x y c d] [a b x y])))))))

        on-pointer-up
        (mf/use-fn #(mf/set-ref-val! drag* nil))

        on-text-key-down
        (mf/use-fn
         (mf/deps on-change)
         (fn [event]
           (when (kbd/enter? event)
             (when-let [parsed (parse-bezier (dom/get-target-val event))]
               (on-change (mapv round2 parsed)))
             (dom/blur! (dom/get-target event)))))

        on-text-blur
        (mf/use-fn
         (mf/deps on-change)
         (fn [event]
           (when-let [parsed (parse-bezier (dom/get-target-val event))]
             (on-change (mapv round2 parsed)))))]

    [:div {:class (stl/css :bezier-editor)}
     [:svg {:ref svg-ref
            :class (stl/css :bezier-box)
            :width size
            :height size
            :view-box (dm/str "0 0 " size " " size)}
      [:rect {:class (stl/css :bezier-area)
              :x pad :y (second (->px 0 1))
              :width (- size (* 2 pad))
              :height (- (second (->px 0 0)) (second (->px 0 1)))}]
      [:line {:class (stl/css :bezier-arm) :x1 p0x :y1 p0y :x2 p1x :y2 p1y}]
      [:line {:class (stl/css :bezier-arm) :x1 p3x :y1 p3y :x2 p2x :y2 p2y}]
      [:path {:class (stl/css :bezier-curve)
              :d (dm/str "M" p0x "," p0y " C" p1x "," p1y " " p2x "," p2y " " p3x "," p3y)}]
      (for [[handle hx hy] [["1" p1x p1y] ["2" p2x p2y]]]
        [:circle {:key handle
                  :class (stl/css :bezier-handle)
                  :data-handle handle
                  :cx hx :cy hy :r 5
                  :on-pointer-down on-pointer-down
                  :on-pointer-move on-pointer-move
                  :on-pointer-up on-pointer-up}])]

     [:input {:class (stl/css :bezier-text)
              :key (str/join "," value)
              :default-value (str/join ", " value)
              :title (tr "workspace.options.interaction-easing-custom-hint")
              :on-key-down on-text-key-down
              :on-blur on-text-blur}]]))
