;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.common.types.embed
  "Playable media attached to a shape.

  A shape keeps its regular fills (a poster frame, normally) so every
  renderer, the exporter and the thumbnail pipeline keep working
  untouched. The `:embed` attribute only tells the viewer overlay what
  to play on top of that poster."
  (:require
   [app.common.data :as d]
   [app.common.schema :as sm]
   [cuerdas.core :as str]))

(def kinds
  "Provider kinds an embed can hold.

  `:youtube` and `:vimeo` are external players rendered in an iframe;
  `:video` is a media object uploaded to this file and played in a
  `<video>` element; `:gif` is an animated bitmap replayed in an `<img>`
  because Skia only decodes its first frame."
  #{:youtube :vimeo :video :gif})

(def iframe-kinds #{:youtube :vimeo})

(def schema:embed
  [:multi {:title "Embed"
           :dispatch :kind
           :decode/json #(update % :kind keyword)}
   [:youtube
    [:map {:title "YoutubeEmbed"}
     [:kind [:= :youtube]]
     [:provider-id [:string {:min 1 :max 64}]]
     [:start {:optional true} ::sm/int]
     [:autoplay {:optional true} :boolean]
     [:loop {:optional true} :boolean]
     [:muted {:optional true} :boolean]
     [:controls {:optional true} :boolean]]]
   [:vimeo
    [:map {:title "VimeoEmbed"}
     [:kind [:= :vimeo]]
     [:provider-id [:string {:min 1 :max 64}]]
     ;; Unlisted vimeo videos are only playable with this hash
     [:provider-hash {:optional true} [:string {:max 64}]]
     [:start {:optional true} ::sm/int]
     [:autoplay {:optional true} :boolean]
     [:loop {:optional true} :boolean]
     [:muted {:optional true} :boolean]
     [:controls {:optional true} :boolean]]]
   [:video
    [:map {:title "VideoEmbed"}
     [:kind [:= :video]]
     [:media-id ::sm/uuid]
     [:mtype {:optional true} [:maybe :string]]
     [:autoplay {:optional true} :boolean]
     [:loop {:optional true} :boolean]
     [:muted {:optional true} :boolean]
     [:controls {:optional true} :boolean]]]
   [:gif
    [:map {:title "GifEmbed"}
     [:kind [:= :gif]]
     [:media-id ::sm/uuid]
     [:loop {:optional true} :boolean]]]])

(def check-embed
  (sm/check-fn schema:embed))

(def valid-embed?
  (sm/validator schema:embed))

;; --- URL PARSING

(def ^:private re:youtube-id
  #"[A-Za-z0-9_-]{11}")

(def ^:private re:youtube-host
  #"(?i)^(?:https?://)?(?:www\.|m\.|music\.)?(?:youtube(?:-nocookie)?\.com|youtu\.be)(/.*)?$")

(def ^:private re:youtube-path
  #"(?i)/(?:watch|embed|shorts|live|v)/([A-Za-z0-9_-]{11})")

(def ^:private re:youtube-bare-path
  #"^/([A-Za-z0-9_-]{11})$")

(def ^:private re:vimeo-host
  #"(?i)^(?:https?://)?(?:www\.|player\.)?vimeo\.com(/.*)?$")

(def ^:private re:vimeo-id
  #"\d{6,}")

(def ^:private re:vimeo-hash
  #"[A-Za-z0-9]{6,}")

(defn- parse-query
  "Split the query string of `url` into a plain map of lowercased keys."
  [url]
  (let [query (second (str/split url #"\?" 2))]
    (if (str/blank? query)
      {}
      (reduce (fn [result pair]
                (let [[k v] (str/split pair #"=" 2)]
                  (if (str/blank? k)
                    result
                    (assoc result (str/lower k) (or v "")))))
              {}
              (-> query
                  (str/split #"#" 2)
                  (first)
                  (str/split #"&"))))))

(defn parse-start
  "Read a youtube/vimeo style timestamp (`90`, `90s`, `1m30s`, `1h2m3s`)
  into seconds. Returns nil when there is nothing usable."
  [value]
  (when-not (str/blank? value)
    (let [value (str/lower (str/trim value))]
      (if-let [plain (d/parse-integer value)]
        (when (pos? plain) plain)
        (let [h     (some-> (re-find #"(\d+)h" value) second d/parse-integer)
              m     (some-> (re-find #"(\d+)m" value) second d/parse-integer)
              s     (some-> (re-find #"(\d+)s" value) second d/parse-integer)
              total (+ (* 3600 (or h 0)) (* 60 (or m 0)) (or s 0))]
          (when (pos? total) total))))))

(defn- parse-youtube
  [url]
  (let [path  (or (second (re-find re:youtube-host url)) "")
        path  (first (str/split path #"\?" 2))
        query (parse-query url)
        ;; the classic watch URL carries the id in `v`; youtu.be/<id> and
        ;; the /embed /shorts /live /v forms carry it in the path
        id    (or (get query "v")
                  (some-> (re-find re:youtube-path path) second)
                  (some-> (re-find re:youtube-bare-path path) second))]
    (when (and (string? id) (re-matches re:youtube-id id))
      (d/without-nils
       {:kind :youtube
        :provider-id id
        :start (parse-start (or (get query "t") (get query "start")))}))))

(defn- parse-vimeo
  [url]
  (let [path  (or (second (re-find re:vimeo-host url)) "")
        path  (first (str/split path #"\?" 2))
        query (parse-query url)
        segs  (vec (remove str/blank? (str/split path #"/")))
        ;; the id is the last all-numeric segment; an unlisted video puts
        ;; its hash in the segment right after it (vimeo.com/<id>/<hash>)
        idx   (->> (map-indexed vector segs)
                   (filter (fn [[_ s]] (some? (re-matches re:vimeo-id s))))
                   (last)
                   (first))
        id    (when (some? idx) (nth segs idx))
        next* (when (some? idx) (get segs (inc idx)))
        hash* (or (get query "h")
                  (when (and (string? next*) (re-matches re:vimeo-hash next*))
                    next*))]
    (when (some? id)
      (d/without-nils
       {:kind :vimeo
        :provider-id id
        :provider-hash hash*
        :start (parse-start (get query "t"))}))))

(defn parse-url
  "Recognise `url` as an embeddable provider link.

  Returns the embed map (without playback options) or nil when the URL
  belongs to no supported provider."
  [url]
  (when (string? url)
    (let [url (str/trim url)]
      (when-not (str/blank? url)
        (cond
          (some? (re-find re:youtube-host url)) (parse-youtube url)
          (some? (re-find re:vimeo-host url))   (parse-vimeo url)
          :else nil)))))

(defn embeddable-url?
  [url]
  (some? (parse-url url)))

;; --- URL BUILDING

(defn- query-string
  [params]
  (->> params
       (remove (fn [[_ v]] (nil? v)))
       (map (fn [[k v]] (str (name k) "=" v)))
       (str/join "&")))

(defn player-url
  "Build the iframe `src` for an external provider embed.

  Defaults are deliberately non-intrusive: no autoplay, controls shown,
  sound on. Autoplay forces mute because no browser starts an audible
  video without a user gesture."
  [{:keys [kind provider-id provider-hash start autoplay loop muted controls]}]
  (let [autoplay? (true? autoplay)
        loop?     (true? loop)
        muted?    (or (true? muted) autoplay?)
        controls? (not (false? controls))]
    (case kind
      :youtube
      (str "https://www.youtube-nocookie.com/embed/" provider-id
           "?" (query-string
                {:autoplay       (if autoplay? 1 0)
                 :mute           (if muted? 1 0)
                 :loop           (if loop? 1 0)
                 ;; the youtube player only loops a single video when it
                 ;; is given an explicit one-entry playlist
                 :playlist       (when loop? provider-id)
                 :controls       (if controls? 1 0)
                 :start          start
                 :rel            0
                 :modestbranding 1
                 :playsinline    1}))

      :vimeo
      (str "https://player.vimeo.com/video/" provider-id
           "?" (query-string
                {:h           provider-hash
                 :autoplay    (if autoplay? 1 0)
                 :muted       (if muted? 1 0)
                 :loop        (if loop? 1 0)
                 :controls    (if controls? 1 0)
                 :dnt         1
                 :playsinline 1})
           (when start (str "#t=" start "s")))

      nil)))

(defn poster-urls
  "Candidate poster images for an external provider embed, best first.

  Vimeo posters need an oEmbed round trip, so only youtube is covered
  here; callers fall back to a plain fill when this is empty."
  [{:keys [kind provider-id]}]
  (case kind
    :youtube [(str "https://i.ytimg.com/vi/" provider-id "/maxresdefault.jpg")
              (str "https://i.ytimg.com/vi/" provider-id "/hqdefault.jpg")]
    []))

(defn provider-label
  [{:keys [kind]}]
  (case kind
    :youtube "YouTube"
    :vimeo   "Vimeo"
    :video   "Video"
    :gif     "GIF"
    nil))

(defn watch-url
  "Canonical page for an external embed, for the `open` action."
  [{:keys [kind provider-id]}]
  (case kind
    :youtube (str "https://www.youtube.com/watch?v=" provider-id)
    :vimeo   (str "https://vimeo.com/" provider-id)
    nil))

(defn playable?
  "True when the embed carries enough data to be rendered by the viewer."
  [embed]
  (and (map? embed)
       (contains? kinds (:kind embed))
       (if (contains? iframe-kinds (:kind embed))
         (not (str/blank? (:provider-id embed)))
         (some? (:media-id embed)))))
