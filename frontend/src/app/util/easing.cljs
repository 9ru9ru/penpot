;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.util.easing
  "CSS easing and duration of a prototype animation. Springs become a
  CSS `linear()` curve sampled from the simulation, and take the time
  the spring needs to settle instead of the stored duration."
  (:require
   [app.common.types.shape.interactions :as ctsi]
   [cuerdas.core :as str]))

(def ^:private max-points 80)

(defn- num->str
  [n]
  (str (/ (js/Math.round (* n 10000)) 10000)))

(defn- linear-easing
  [samples]
  (let [step   (max 1 (js/Math.ceil (/ (count samples) max-points)))
        points (conj (vec (take-nth step samples)) (peek samples))]
    (str "linear(" (str/join ", " (map num->str points)) ")")))

(def ^:private spring-easing
  (memoize (fn [easing]
             (linear-easing (first (ctsi/spring-samples (get ctsi/springs easing)))))))

(defn bezier->css
  [[x1 y1 x2 y2]]
  (str "cubic-bezier(" (str/join ", " (map num->str [x1 y1 x2 y2])) ")"))

(defn css-easing
  [{:keys [easing easing-bezier]}]
  (cond
    (ctsi/spring? easing)
    (spring-easing easing)

    (= :custom-bezier easing)
    (bezier->css (or easing-bezier ctsi/default-custom-bezier))

    (contains? ctsi/easing-beziers easing)
    (bezier->css (get ctsi/easing-beziers easing))

    :else
    "ease"))

(defn duration
  [{:keys [easing duration]}]
  (or (ctsi/spring-duration easing) duration))
