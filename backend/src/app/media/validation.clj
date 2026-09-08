;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.media.validation
  "Schemas and validation functions for media uploads.
   Leaf namespace — depends on app.common.* and app.config only."
  (:require
   [app.common.exceptions :as ex]
   [app.common.media :as cm]
   [app.common.schema :as sm]
   [app.config :as cf]
   [buddy.core.bytes :as bb]
   [cuerdas.core :as str]
   [datoteka.fs :as fs]
   [datoteka.io :as io])
  (:import
   java.io.InputStream))

(def schema:upload
  [:map {:title "Upload"}
   [:filename :string]
   [:size ::sm/int]
   [:path ::fs/path]
   [:mtype {:optional true} :string]
   [:headers {:optional true}
    [:map-of :string :string]]])

(def schema:input
  [:map {:title "Input"}
   [:path ::fs/path]
   [:mtype {:optional true} ::sm/text]])

(def check-input
  (sm/check-fn schema:input))

(defn validate-media-type!
  ([upload] (validate-media-type! upload cm/image-types))
  ([upload allowed]
   (when-not (contains? allowed (:mtype upload))
     (ex/raise :type :validation
               :code :media-type-not-allowed
               :hint "Seems like you are uploading an invalid media object"))

   upload))

(def ^:private webm-magic
  "EBML header shared by webm and matroska."
  (byte-array [(unchecked-byte 0x1A) (unchecked-byte 0x45)
               (unchecked-byte 0xDF) (unchecked-byte 0xA3)]))

(defn- read-head
  "Read the first `n` bytes of `path`, or fewer when the file is shorter."
  [path n]
  (with-open [^InputStream input (io/input-stream path)]
    (let [buffer (byte-array n)
          read   (.readNBytes input buffer 0 n)]
      (if (= read n)
        buffer
        (java.util.Arrays/copyOf buffer read)))))

(defn validate-video-content!
  "Check that `upload` really holds the video container it claims.

  The backend cannot transcode video, so the bytes are stored and served
  back verbatim; refusing a mislabelled container here keeps us from
  handing out arbitrary content under a `video/*` content type."
  [{:keys [path mtype] :as upload}]
  (let [head (read-head path 12)
        ok?  (case mtype
               ;; ISO-BMFF: a box length, then the `ftyp` box type
               "video/mp4"  (and (>= (alength head) 12)
                                 (= "ftyp" (String. head 4 4 "ISO-8859-1")))
               "video/webm" (and (>= (alength head) 4)
                                 (bb/equals? webm-magic (java.util.Arrays/copyOf head 4)))
               false)]
    (when-not ok?
      (ex/raise :type :validation
                :code :media-type-mismatch
                :hint (str/ffmt "uploaded content does not look like a % file" mtype)))
    upload))

(defn validate-media-size!
  [upload]
  (let [max-size (cf/get :media-max-file-size)]
    (when (> (:size upload) max-size)
      (ex/raise :type :restriction
                :code :media-max-file-size-reached
                :hint (str/ffmt "the uploaded file size % is greater than the maximum %"
                                (:size upload)
                                max-size)))
    upload))

(defn validate-font-size!
  "Validates that the font file `upload` does not exceed the configured
  `:font-max-file-size` limit.  Accepts the same map shape as
  `validate-media-size!` — requires a `:size` key in bytes."
  [upload]
  (let [max-size (cf/get :font-max-file-size)]
    (when (> (:size upload) max-size)
      (ex/raise :type :restriction
                :code :font-max-file-size-reached
                :hint (str/ffmt "the uploaded font size % is greater than the maximum %"
                                (:size upload)
                                max-size)))
    upload))
