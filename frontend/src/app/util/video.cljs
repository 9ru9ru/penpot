;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.util.video
  "Client side video probing.

  The backend ships no ffmpeg, so the browser — which has to decode the
  file anyway to show it — is what reports the intrinsic size and
  renders the poster frame that every renderer ends up drawing."
  (:require
   [app.util.webapi :as wapi]
   [beicon.v2.core :as rx]))

(def ^:private poster-mtype "image/png")

;; A frame at t=0 is very often a black or blank leader, so we sample a
;; little way in, capped so a long video does not seek far into the file.
(def ^:private poster-seek-ratio 0.1)
(def ^:private poster-seek-max-seconds 2)

(defn- invalid-video
  [hint]
  (ex-info hint {:type :validation :code :invalid-video}))

(defn- poster-timestamp
  [duration]
  (if (and (number? duration)
           (js/isFinite duration)
           (pos? duration))
    (min (* duration poster-seek-ratio) poster-seek-max-seconds)
    0))

(defn- capture-poster!
  "Draw the currently displayed frame of `video` into a PNG blob.

  Calls `resolve` with the blob or `reject` with an ex-info."
  [^js video resolve reject]
  (let [width  (.-videoWidth video)
        height (.-videoHeight video)
        canvas (js/document.createElement "canvas")]
    (set! (.-width canvas) width)
    (set! (.-height canvas) height)
    (try
      (.drawImage (.getContext canvas "2d") video 0 0 width height)
      (.toBlob canvas
               (fn [blob]
                 (if (some? blob)
                   (resolve blob)
                   (reject (invalid-video "unable to render video poster"))))
               poster-mtype)
      (catch :default cause
        (reject cause)))))

(defn probe
  "Read intrinsic size, duration and a poster frame out of a video blob.

  Emits a single `{:width :height :duration :poster}` map, where
  `:poster` is a PNG blob. Errors with an `:invalid-video` ex-info when
  the browser cannot decode the file."
  [blob]
  (rx/create
   (fn [subscriber]
     (let [uri       (wapi/create-uri blob)
           video     (js/document.createElement "video")
           disposed? (volatile! false)

           dispose!
           (fn []
             (when-not @disposed?
               (vreset! disposed? true)
               (set! (.-src video) "")
               (.load video)
               (wapi/revoke-uri uri)))

           reject
           (fn [cause]
             (when-not @disposed?
               (dispose!)
               (rx/error! subscriber cause)))

           resolve
           (fn [poster]
             (when-not @disposed?
               (let [result {:width    (.-videoWidth video)
                             :height   (.-videoHeight video)
                             :duration (.-duration video)
                             :poster   poster}]
                 (dispose!)
                 (rx/push! subscriber result)
                 (rx/end! subscriber))))

           on-error
           (fn [_] (reject (invalid-video "unable to decode video")))

           on-seeked
           (fn [_] (capture-poster! video resolve reject))

           on-loaded
           (fn [_]
             (if (or (zero? (.-videoWidth video))
                     (zero? (.-videoHeight video)))
               (on-error nil)
               ;; seeking to the same timestamp never fires `seeked`, so
               ;; grab the loaded frame directly when there is no offset
               (let [ts (poster-timestamp (.-duration video))]
                 (if (zero? ts)
                   (on-seeked nil)
                   (set! (.-currentTime video) ts)))))]

       ;; muted + playsinline keeps mobile browsers from refusing to
       ;; decode a video that no user gesture started
       (set! (.-muted video) true)
       (set! (.-playsInline video) true)
       (set! (.-preload video) "auto")
       (.addEventListener video "loadeddata" on-loaded)
       (.addEventListener video "seeked" on-seeked #js {:once true})
       (.addEventListener video "error" on-error)
       (set! (.-src video) uri)

       (fn []
         (.removeEventListener video "loadeddata" on-loaded)
         (.removeEventListener video "error" on-error)
         (dispose!))))))
