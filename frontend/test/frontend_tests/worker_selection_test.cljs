;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns frontend-tests.worker-selection-test
  (:require
   [app.common.files.builder :as fb]
   [app.common.geom.point :as gpt]
   [app.common.geom.rect :as grc]
   [app.worker.selection :as selection]
   [cljs.test :as t :include-macros true]))

(def ^:private stroke
  {:stroke-style :solid
   :stroke-alignment :center
   :stroke-width 2
   :stroke-color "#000000"
   :stroke-opacity 1})

(def ^:private transparent-gradient
  {:type :linear
   :start-x 0.5
   :start-y 0
   :end-x 0.5
   :end-y 1
   :width 1
   :stops [{:color "#000000" :opacity 0 :offset 0}
           {:color "#000000" :opacity 0 :offset 1}]})

(defn- add-shape
  "Adds a 100x100 shape at `x` and returns [state id]."
  [state type x fills]
  (let [state (fb/add-shape state {:type type
                                   :x x
                                   :y 0
                                   :width 100
                                   :height 100
                                   :fills fills
                                   :strokes [stroke]})]
    [state (::fb/last-id state)]))

(defn- setup
  []
  (let [state (-> (fb/create-state)
                  (fb/add-file {:name "Test"})
                  (fb/add-page {:name "Page 1"}))

        [state no-fill]
        (add-shape state :rect 0 [])

        [state transparent]
        (add-shape state :rect 200 [{:fill-color "#B1B2B5" :fill-opacity 0}])

        [state visible]
        (add-shape state :rect 400 [{:fill-color "#B1B2B5" :fill-opacity 1}])

        [state gradient]
        (add-shape state :rect 600 [{:fill-color-gradient transparent-gradient :fill-opacity 1}])

        [state mixed]
        (add-shape state :rect 800 [{:fill-color "#B1B2B5" :fill-opacity 0}
                                    {:fill-color "#B1B2B5" :fill-opacity 0.5}])

        [state circle]
        (add-shape state :circle 1000 [{:fill-color "#B1B2B5" :fill-opacity 0}])

        page  (fb/get-current-page state)
        index (selection/add-page {} page)]

    {:index index
     :page-id (:id page)
     :ids {:no-fill no-fill
           :transparent transparent
           :visible visible
           :gradient gradient
           :mixed mixed
           :circle circle}}))

(defn- hovered
  "Ids under the point, queried the way the viewport hover does it."
  [{:keys [index page-id]} x y]
  (set (selection/query index {:page-id page-id
                               :rect (grc/center->rect (gpt/point x y) 5)
                               :include-frames? true
                               :clip-children? true
                               :using-selrect? false})))

(t/deftest invisible-fills-are-hit-only-on-the-stroke
  (let [{:keys [ids] :as data} (setup)
        hit? (fn [key x y] (contains? (hovered data x y) (get ids key)))]

    (t/testing "a shape without fills"
      (t/is (not (hit? :no-fill 50 50)))
      (t/is (not (hit? :no-fill 8 50)))
      (t/is (hit? :no-fill 0 50))
      (t/is (hit? :no-fill 2 50))
      (t/is (hit? :no-fill -2 50)))

    (t/testing "a shape whose only fill is fully transparent"
      (t/is (not (hit? :transparent 250 50)))
      (t/is (hit? :transparent 200 50)))

    (t/testing "a gradient whose stops are all transparent"
      (t/is (not (hit? :gradient 650 50)))
      (t/is (hit? :gradient 600 50)))

    (t/testing "a circle whose fill is fully transparent"
      (t/is (not (hit? :circle 1050 50)))
      (t/is (hit? :circle 1000 50)))

    (t/testing "a visible fill keeps the whole area clickable"
      (t/is (hit? :visible 450 50)))

    (t/testing "one visible fill among transparent ones is enough"
      (t/is (hit? :mixed 850 50)))))
