;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.data.workspace.embed
  "Creation and editing of playable embeds.

  An embed is an ordinary shape whose fill is a poster frame plus an
  `:embed` attribute. Nothing about the canvas changes: the workspace
  keeps drawing the poster, and only the viewer overlay reads the
  attribute and plays the media on top of it."
  (:require
   [app.common.data :as d]
   [app.common.math :as mth]
   [app.common.media :as cm]
   [app.common.types.embed :as ctem]
   [app.common.types.fills :as types.fills]
   [app.main.data.media :as dmm]
   [app.main.data.notifications :as ntf]
   [app.main.data.workspace.media :as dwm]
   [app.main.data.workspace.shapes :as dwsh]
   [app.main.store :as st]
   [app.util.i18n :refer [tr]]
   [app.util.video :as video]
   [beicon.v2.core :as rx]
   [cuerdas.core :as str]
   [potok.v2.core :as ptk]))

(def accept-video-types
  (str/join "," cm/video-types))

;; Used when no poster could be obtained; 16:9 is what both supported
;; providers serve by default.
(def ^:private default-embed-width 640)
(def ^:private default-embed-height 360)
(def ^:private placeholder-color "#1A1A1A")

(defn- strip-extension
  [filename]
  (str/replace filename #"\.[A-Za-z0-9]{2,5}$" ""))

(defn- placeholder-fills
  []
  (types.fills/create {:fill-color placeholder-color :fill-opacity 1}))

(defn- poster-fills
  [{:keys [width height mtype id]}]
  (types.fills/create
   {:fill-opacity 1
    :fill-image {:width width
                 :height height
                 :mtype mtype
                 :id id
                 :keep-aspect-ratio true}}))

(defn- add-embed-shape
  "Place a shape of `width`x`height` centred on `position`, carrying
  `fills` and the `embed` attribute."
  [{:keys [name width height fills embed position]}]
  (let [{:keys [x y]} position]
    (dwsh/create-and-add-shape
     :rect x y
     {:name name
      :width width
      :height height
      :x (mth/round (- x (/ width 2)))
      :y (mth/round (- y (/ height 2)))
      :fills fills
      :embed embed})))

;; --- PROVIDER LINKS

(defn- fetch-poster
  "Ask the backend to download the first reachable poster candidate.

  The download happens server side because provider CDNs answer with no
  CORS headers. Emits nil when no candidate is available or all fail, so
  callers fall back to a plain fill."
  [file-id name embed]
  (let [candidates (ctem/poster-urls embed)]
    (if (empty? candidates)
      (rx/of nil)
      (->> (rx/from candidates)
           (rx/mapcat (fn [url]
                        (->> (dwm/upload-media-url name file-id url)
                             (rx/catch (constantly (rx/empty))))))
           (rx/take 1)
           (rx/reduce (fn [_ media] media) nil)))))

(defn create-embed-from-url
  "Create an embed shape for a supported provider `url`.

  Emits nothing when the URL belongs to no supported provider, which
  lets callers chain this before their own paste handling."
  [url position]
  (ptk/reify ::create-embed-from-url
    ptk/WatchEvent
    (watch [_ state _]
      (when-let [embed (ctem/parse-url url)]
        (let [file-id (:current-file-id state)
              label   (ctem/provider-label embed)
              name    (str label " " (:provider-id embed))]
          (rx/concat
           (rx/of (ntf/show {:content (tr "media.loading")
                             :type :toast
                             :level :info
                             :timeout nil
                             :tag :media-loading}))
           (->> (fetch-poster file-id name embed)
                (rx/map (fn [poster]
                          (if (some? poster)
                            (add-embed-shape
                             {:name name
                              :width (:width poster)
                              :height (:height poster)
                              :fills (poster-fills poster)
                              :embed embed
                              :position position})
                            (add-embed-shape
                             {:name name
                              :width default-embed-width
                              :height default-embed-height
                              :fills (placeholder-fills)
                              :embed embed
                              :position position}))))
                (rx/catch dwm/handle-media-error)
                (rx/finalize #(st/emit! (ntf/hide :tag :media-loading))))))))))

;; --- UPLOADED VIDEO

(defn- validate-video-blob
  [blob]
  (when-not (contains? cm/video-types (.-type blob))
    (throw (ex-info "unsupported video type"
                    {:type :validation :code :media-type-not-allowed})))
  blob)

(defn upload-video
  "Upload a video blob and place it as an embed.

  Two media objects are created: the poster PNG the browser rendered,
  which becomes the shape fill, and the video itself, which only the
  viewer overlay reads."
  [{:keys [blob position]}]
  (ptk/reify ::upload-video
    ptk/WatchEvent
    (watch [_ state _]
      (let [file-id (:current-file-id state)
            name    (if (dmm/file? blob)
                      (strip-extension (.-name blob))
                      (tr "media.video"))]
        (rx/concat
         (rx/of (ntf/show {:content (tr "media.loading")
                           :type :toast
                           :level :info
                           :timeout nil
                           :tag :media-loading}))
         (->> (rx/of blob)
              (rx/map validate-video-blob)
              (rx/mapcat video/probe)
              (rx/mapcat
               (fn [{:keys [width height poster]}]
                 (rx/zip
                  (dwm/upload-blob {:file-id file-id
                                    :name (str name " poster")
                                    :is-local true
                                    :blob poster})
                  (dwm/upload-blob {:file-id file-id
                                    :name name
                                    :is-local true
                                    :blob blob
                                    :width width
                                    :height height}))))
              (rx/map
               (fn [[poster video]]
                 (add-embed-shape
                  {:name name
                   :width (:width poster)
                   :height (:height poster)
                   :fills (poster-fills poster)
                   :embed (d/without-nils
                           {:kind :video
                            :media-id (:id video)
                            :mtype (:mtype video)})
                   :position position})))
              (rx/catch dwm/handle-media-error)
              (rx/finalize #(st/emit! (ntf/hide :tag :media-loading)))))))))

;; --- EDITING

(defn update-embed
  "Merge `attrs` into the `:embed` map of every shape in `ids`.

  Shapes without an embed are left untouched, so this is safe to call
  over a mixed selection."
  [ids attrs]
  (ptk/reify ::update-embed
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (dwsh/update-shapes
              ids
              (fn [shape]
                (if (some? (:embed shape))
                  (update shape :embed merge attrs)
                  shape)))))))

(defn detach-embed
  "Drop the embed attribute, keeping the poster as a plain image."
  [ids]
  (ptk/reify ::detach-embed
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (dwsh/update-shapes ids #(dissoc % :embed))))))
