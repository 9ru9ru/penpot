;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.data.workspace.bulk-rename
  "Renaming several layers at once, as Figma's rename dialog does:
  `$&` is the current name (or the matched text), `$n`/`$nn`/`$nnn` count
  up from the start number and `$N`/`$NN`/`$NNN` count down to it."
  (:require
   [app.common.types.shape-tree :as ctst]
   [app.main.data.helpers :as dsh]
   [app.main.data.workspace.undo :as dwu]
   [beicon.v2.core :as rx]
   [cuerdas.core :as str]
   [potok.v2.core :as ptk]))

(defn- pad
  [n width]
  (str/pad (str n) {:length width :padding "0" :type :left}))

(defn- expand-numbers
  "Replaces the counter tokens; longest first so `$nn` is not read as `$n`."
  [template asc desc]
  (-> template
      (str/replace "$nnn" (pad asc 3))
      (str/replace "$NNN" (pad desc 3))
      (str/replace "$nn" (pad asc 2))
      (str/replace "$NN" (pad desc 2))
      (str/replace "$n" (str asc))
      (str/replace "$N" (str desc))))

(defn- escape-regex
  [s]
  (.replace s (js/RegExp. "[.*+?^${}()|[\\]\\\\]" "g") "\\$&"))

(defn match-regex
  "The RegExp for a match string, or nil when it is empty or invalid."
  [match {:keys [regex? case-sensitive?]}]
  (when (seq match)
    (try
      (js/RegExp. (if regex? match (escape-regex match))
                  (if case-sensitive? "g" "gi"))
      (catch :default _ nil))))

(defn new-names
  "The new name of every layer in `names` (in order). Without `match` the
  whole name becomes `rename-to`; with it only the matched parts change,
  and `rename-to` may use `$&` and, as a regular expression, `$1`..."
  [names {:keys [rename-to match start] :or {start 1} :as opts}]
  (let [total (count names)
        regex (match-regex match opts)]
    (vec
     (map-indexed
      (fn [index name]
        (let [asc  (+ start index)
              desc (+ start (- total index 1))
              to   (expand-numbers (or rename-to "") asc desc)]
          (cond
            (some? regex)
            (.replace name regex to)

            (seq match)
            name

            (str/blank? to)
            name

            :else
            (str/replace to "$&" name))))
      names))))

(defn ordered-ids
  "The ids in the order of the layers panel, top first."
  [state ids]
  (let [objects (dsh/lookup-page-objects state)]
    (reverse (ctst/sort-z-index objects (vec ids)))))

(defn rename-shapes
  "Applies `names-by-id` in a single undo step."
  [names-by-id]
  (ptk/reify ::rename-shapes
    ptk/WatchEvent
    (watch [_ _ _]
      (let [undo-id (js/Symbol)]
        (rx/concat
         (rx/of (dwu/start-undo-transaction undo-id))
         (->> (rx/from (vec names-by-id))
              (rx/map (fn [[id name]]
                        (ptk/event :app.main.data.workspace/end-rename-shape-by-id
                                   {:id id :name name}))))
         (rx/of (dwu/commit-undo-transaction undo-id)))))))
