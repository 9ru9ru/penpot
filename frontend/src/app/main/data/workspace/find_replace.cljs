;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.data.workspace.find-replace
  "Matching for the layers panel find & replace, with the usual options:
  match case, whole word and regular expression."
  (:require
   [app.common.types.text :as txt]
   [cuerdas.core :as str]))

(defn- escape-regex
  [s]
  (.replace s (js/RegExp. "[.*+?^${}()|[\\]\\\\]" "g") "\\$&"))

(defn matcher
  "A global RegExp for `search`, or nil when the search is empty or an
  invalid regular expression. Whole words are bounded by any letter or
  digit, so it also works for Hangul."
  [search {:keys [match-case? whole-word? regex?]}]
  (when (seq search)
    (try
      (let [source (if regex? search (escape-regex search))
            source (if whole-word?
                     (str "(?<![\\p{L}\\p{N}_])(?:" source ")(?![\\p{L}\\p{N}_])")
                     source)]
        (js/RegExp. source (str "gu" (when-not match-case? "i"))))
      (catch :default _ nil))))

(defn matches?
  [re text]
  (and (some? re)
       (string? text)
       (do (set! (.-lastIndex re) 0)
           (.test re text))))

(defn replace-all
  "Replaces every match. Outside regular-expression mode `$` in the
  replacement is literal."
  [re text replacement {:keys [regex?]}]
  (if (and (some? re) (string? text))
    (.replace text re (if regex? replacement (str/replace replacement "$" "$$")))
    text))

(defn content-matches?
  [re content]
  (->> (txt/node-seq txt/is-text-node? content)
       (some #(matches? re (:text %)))
       (boolean)))

(defn replace-in-content
  [re content replacement opts]
  (txt/transform-nodes
   txt/is-text-node?
   (fn [node] (update node :text #(replace-all re % replacement opts)))
   content))
