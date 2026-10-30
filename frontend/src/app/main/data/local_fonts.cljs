;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.data.local-fonts
  "Fonts installed on this computer (the browser's Local Font Access
  API, Chromium only), uploaded to the team fonts so everyone in the
  team gets them, like Figma's local fonts."
  (:require
   ["opentype.js" :as ot]
   [app.common.data :as d]
   [app.main.data.fonts :as df]
   [beicon.v2.core :as rx]))

(defn supported?
  []
  (fn? (.-queryLocalFonts js/window)))

(defn query
  "Installed fonts as [{:family :style :full-name :postscript-name :data}],
  where :data is the browser's FontData. Asks for permission the first
  time."
  []
  (->> (rx/from (.queryLocalFonts js/window))
       (rx/map (fn [fonts]
                 (->> (array-seq fonts)
                      (mapv (fn [^js f]
                              {:family (.-family f)
                               :style (.-style f)
                               :full-name (.-fullName f)
                               :postscript-name (.-postscriptName f)
                               :data f})))))))

(defn- tag
  [^js ab]
  (let [dv (js/DataView. ab)]
    (.getUint32 dv 0)))

(def ^:private tag-ttcf 0x74746366)
(def ^:private tag-true 0x74727565)

(defn- pad4
  [n]
  (* 4 (js/Math.ceil (/ n 4))))

(defn- collection-fonts
  "Splits a TrueType collection (.ttc) into standalone font files."
  [^js ab]
  (let [dv  (js/DataView. ab)
        src (js/Uint8Array. ab)
        n   (.getUint32 dv 8)]
    (vec
     (for [i (range n)]
       (let [off         (.getUint32 dv (+ 12 (* 4 i)))
             num-tables  (.getUint16 dv (+ off 4))
             header-size (+ 12 (* 16 num-tables))
             tables      (vec (for [t (range num-tables)]
                                (let [rec (+ off 12 (* 16 t))]
                                  {:offset (.getUint32 dv (+ rec 8))
                                   :length (.getUint32 dv (+ rec 12))})))
             total       (reduce + header-size (map (comp pad4 :length) tables))
             out         (js/Uint8Array. total)
             odv         (js/DataView. (.-buffer out))]
         ;; Offset table and table records, then each table with its
         ;; record pointing at its new place.
         (.set out (.subarray src off (+ off header-size)) 0)
         (loop [tables tables pos header-size idx 0]
           (when-let [{:keys [offset length]} (first tables)]
             (.set out (.subarray src offset (+ offset length)) pos)
             (.setUint32 odv (+ 12 (* 16 idx) 8) pos)
             (recur (rest tables) (+ pos (pad4 length)) (inc idx))))
         (.-buffer out))))))

(defn- postscript-name
  [^js ab]
  (try
    (.getEnglishName ^js (ot/parse ab) "postScriptName")
    (catch :default _ nil)))

(defn- standalone
  "The font file for one installed font. A collection is split and the
  font with the same PostScript name is picked; Apple's old 'true'
  header is changed to the standard one so the upload accepts it."
  [ab postscript]
  (let [t (tag ab)]
    (cond
      (= t tag-ttcf)
      (let [fonts (collection-fonts ab)]
        (or (d/seek #(= postscript (postscript-name %)) fonts)
            (first fonts)))

      (= t tag-true)
      (let [copy (.slice ab 0)]
        (.setUint32 (js/DataView. copy) 0 0x00010000)
        copy)

      :else ab)))

(defn- font-file
  [{:keys [^js data postscript-name]}]
  (->> (rx/from (.blob data))
       (rx/mapcat #(rx/from (.arrayBuffer ^js %)))
       (rx/map (fn [ab]
                 (js/File. #js [(standalone ab postscript-name)]
                           (str postscript-name ".ttf"))))))

(defn upload
  "Uploads the given installed fonts to the team fonts. Returns a stream
  that emits each created font variant."
  [fonts team-id installed]
  (->> (rx/from fonts)
       (rx/mapcat font-file)
       (rx/reduce conj [])
       (rx/mapcat #(df/process-upload % team-id))
       (rx/map #(df/merge-and-group-fonts {} installed %))
       (rx/mapcat #(rx/from (vals %)))
       (rx/mapcat df/upload-font-variant)))
