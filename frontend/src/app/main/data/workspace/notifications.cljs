;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.data.workspace.notifications
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.changes :as cpc]
   [app.common.schema :as sm]
   [app.common.time :as ct]
   [app.common.uuid :as uuid]
   [app.main.data.changes :as dch]
   [app.main.data.comments :as dcm]
   [app.main.data.common :as dc]
   [app.main.data.helpers :as dsh]
   [app.main.data.modal :as modal]
   [app.main.data.plugins :as dpl]
   [app.main.data.websocket :as dws]
   [app.main.data.workspace :as-alias dw]
   [app.main.data.workspace.common :as dwc]
   [app.main.data.workspace.edition :as dwe]
   [app.main.data.workspace.layout :as dwly]
   [app.main.data.workspace.libraries :as dwl]
   [app.main.data.workspace.multiplayer :as dwm]
   [app.main.data.workspace.texts :as dwt]
   [app.main.router :as rt]
   [app.util.globals :refer [global]]
   [app.util.mouse :as mse]
   [app.util.object :as obj]
   [app.util.rxops :as rxs]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

;; FIXME: this ns should be renamed to something different

(declare process-message)
(declare handle-presence)
(declare handle-pointer-update)
(declare handle-file-change)
(declare handle-file-deleted)
(declare handle-file-restored)
(declare handle-library-change)
(declare handle-pointer-send)
(declare handle-export-update)

(defn initialize
  [team-id file-id]
  (ptk/reify ::initialize
    ptk/UpdateEvent
    (update [_ state]
      (dissoc state
              :workspace-follow
              :workspace-spotlight
              :workspace-spotlight-dismissed
              :workspace-cursor-chat))

    ptk/WatchEvent
    (watch [_ state stream]
      (let [stopper     (rx/filter (ptk/type? ::finalize) stream)
            profile-id (:profile-id state)
            sent*      (atom nil)

            initmsg    [{:type :subscribe-file
                         :file-id file-id
                         :version (obj/get global "penpotVersion")}
                        {:type :subscribe-team
                         :team-id team-id}]

            endmsg     {:type :unsubscribe-file
                        :file-id file-id}

            stream     (->> (rx/merge
                             ;; Send the subscription message
                             (->> (rx/from initmsg)
                                  (rx/map dws/send))

                             ;; Subscribe to notifications of the subscription
                             (->> stream
                                  (rx/filter (ptk/type? ::dws/message))
                                  (rx/map deref)
                                  (rx/filter (fn [{:keys [topic] :as msg}]
                                               (or (= topic uuid/zero)
                                                   (= topic profile-id)
                                                   (= topic team-id)
                                                   (= topic file-id))))
                                  (rx/map process-message))

                             ;; On reconnect, send again the subscription messages
                             (->> stream
                                  (rx/filter (ptk/type? ::dws/opened))
                                  (rx/mapcat #(->> (rx/from initmsg)
                                                   (rx/map dws/send))))

                             ;; Emit presence event for current user;
                             ;; this is because websocket server don't
                             ;; emits this for the same user.
                             (rx/of (handle-presence {:type :connect
                                                      :session-id (:session-id state)
                                                      :profile-id (:profile-id state)}))

                             ;; Emit to all other connected users the current pointer
                             ;; position changes.
                             (->> stream
                                  (rx/filter mse/pointer-event?)
                                  (rx/filter #(= :viewport (mse/get-pointer-source %)))
                                  (rx/pipe (rxs/throttle 50))
                                  (rx/map #(handle-pointer-send file-id sent* {:point (:pt %)})))

                             ;; Clicks on the canvas, so the others can see where they land.
                             (->> stream
                                  (rx/filter mse/mouse-event?)
                                  (rx/filter mse/mouse-down-event?)
                                  (rx/map #(handle-pointer-send file-id sent* {:click-id (uuid/next)})))

                             ;; The view, selection, chat... also change without the
                             ;; pointer moving; send them when they do, and now and then
                             ;; anyway so that newcomers and followers catch up.
                             (->> (rx/interval 100)
                                  (rx/map #(handle-pointer-send file-id sent* {:only-if-changed? true})))

                             ;; After a comment change (the request has usually
                             ;; finished by then) tell the others to refresh.
                             (->> stream
                                  (rx/filter #(contains? dwm/comment-change-events (ptk/type %)))
                                  (rx/debounce 800)
                                  (rx/map (fn [_] (dwm/bump-comments-rev)))))

                            (rx/take-until stopper))]

        (rx/concat stream (rx/of (dws/send endmsg)))))))

(defn- handle-change-team-role
  [{:keys [role] :as msg}]
  (ptk/reify ::handle-change-team-role
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/concat
       (rx/of :interrupt
              (dwe/clear-edition-mode)
              (dwc/set-workspace-read-only false))
       (->> (rx/of (dc/change-team-role msg)
                   ::dwt/update-editor-state)
            ;; Delay so anything that launched :interrupt can finish
            (rx/delay 100))
       (if (= :viewer role)
         (rx/of (modal/hide)
                (dwly/set-options-mode :inspect)
                (dpl/close-current-plugin {:close-only-edition-plugins? true}))
         (rx/of (dwly/set-options-mode :design)))))))

(defn- process-message
  [{:keys [type] :as msg}]
  (case type
    :join-file               (handle-presence msg)
    :leave-file              (handle-presence msg)
    :presence                (handle-presence msg)
    :disconnect              (handle-presence msg)
    :pointer-update          (handle-pointer-update msg)
    :file-change             (handle-file-change msg)
    :file-deleted            (handle-file-deleted msg)
    :file-restored           (handle-file-restored msg)
    :library-change          (handle-library-change msg)
    :notification            (dc/handle-notification msg)
    :team-role-change        (handle-change-team-role msg)
    :team-membership-change  (dc/team-membership-change msg)
    :team-organization-change         (dc/handle-change-team-organization msg)
    :organization-change-sso (dc/handle-organization-change-sso msg)
    nil))

(def ^:private heartbeat-ms 1500)

(defn- handle-pointer-send
  "Sends this session's pointer and shared state to the others.
  `sent*` holds the last message sent, so that `:only-if-changed?`
  ticks stay quiet unless something changed or a heartbeat is due."
  [file-id sent* {:keys [point click-id only-if-changed?]}]
  (ptk/reify ::handle-pointer-send
    ptk/WatchEvent
    (watch [_ state _]
      (let [prev    @sent*
            shared  (dwm/local-state state)
            point   (or point (:position prev))
            now     (inst-ms (ct/now))
            due?    (or (not only-if-changed?)
                        (not= shared (:shared prev))
                        (> (- now (:at prev 0)) heartbeat-ms))]
        (when due?
          (reset! sent* {:shared shared :position point :at now})
          (rx/of (dws/send (cond-> (assoc shared
                                          :type :pointer-update
                                          :file-id file-id
                                          :position point)
                             (some? click-id)
                             (assoc :click-id click-id)))))))))

;; --- Finalize Websocket

(defn finalize
  [_]
  (ptk/reify ::finalize))

;; --- Handle: Presence

(def ^:private presence-palette
  ["#f49ef7" ; pink
   "#75cafc" ; blue
   "#fdcf79" ; gold
   "#a9bdfa" ; indigo
   "#faa6b7" ; red
   "#cbaaff" ; purple
   "#f9b489" ; orange
   "#dee563" ; yellow
   "#b1e96f" ; lemon
   ])

(defn- assign-colors
  "Gives every session a color that is the same on every client: sessions
  are visited in id order, each one takes the palette slot its profile
  hashes to, or the next free one."
  [presence]
  (let [n (count presence-palette)]
    (loop [ids (sort-by str (keys presence)) used #{} presence presence]
      (if-let [sid (first ids)]
        (let [start (mod (hash (str (dm/get-in presence [sid :profile-id]))) n)
              slot  (or (->> (range n)
                             (map #(mod (+ start %) n))
                             (remove used)
                             (first))
                        start)]
          (recur (rest ids)
                 (conj used slot)
                 (assoc-in presence [sid :color] (nth presence-palette slot))))
        presence))))

(defn handle-presence
  [{:keys [type session-id profile-id version] :as message}]
  (letfn [(update-session [session]
            (-> session
                (assoc :id session-id)
                (assoc :profile-id profile-id)
                (assoc :updated-at (ct/now))
                (assoc :version version)
                (assoc :text-color "#000000")))

          (update-presence [presence]
            (-> presence
                (update session-id update-session)
                (d/without-nils)))]

    (ptk/reify ::handle-presence
      ptk/UpdateEvent
      (update [_ state]
        (if (or (= :disconnect type) (= :leave-file type))
          (update state :workspace-presence (comp assign-colors #(dissoc % session-id)))
          (update state :workspace-presence (comp assign-colors update-presence)))))))


(defn- update-remote-session
  [session {:keys [session-id profile-id position zoom zoom-inverse vbox vport page-id
                   selected spotlight following chat click-id comments-rev]}]
  (let [now (ct/now)]
    (cond-> (assoc session
                   :id session-id
                   :profile-id (or (:profile-id session) profile-id)
                   :zoom zoom
                   :zoom-inverse zoom-inverse
                   :vbox vbox
                   :vport vport
                   :point position
                   :selected selected
                   :spotlight spotlight
                   :following following
                   :chat chat
                   :comments-rev comments-rev
                   :updated-at now
                   :page-id page-id)

      (not= position (:point session))
      (assoc :point-at now)

      (and (some? click-id) (not= click-id (:click-id session)))
      (assoc :click-id click-id
             :click-point position
             :click-at now))))

(defn handle-pointer-update
  [{:keys [session-id comments-rev] :as msg}]
  (let [comments-changed* (volatile! false)]
    (ptk/reify ::handle-pointer-update
      IDeref
      (-deref [_] msg)

      ptk/UpdateEvent
      (update [_ state]
        (let [seen (dm/get-in state [:workspace-presence session-id :comments-rev])]
          (vreset! comments-changed* (and (some? seen) (some? comments-rev) (not= seen comments-rev))))
        (update state :workspace-presence
                (fn [presence]
                  (cond-> (update presence session-id update-remote-session msg)
                    ;; First news of this session: it needs a color.
                    (not (contains? presence session-id))
                    (assign-colors)))))

      ptk/WatchEvent
      (watch [_ state _]
        (rx/concat
         (rx/of (dwm/check-spotlight session-id))
         (when @comments-changed*
           (let [file-id   (:current-file-id state)
                 thread-id (dm/get-in state [:comments-local :open])]
             (rx/concat
              (rx/of (dcm/refresh-comment-threads file-id))
              (when thread-id
                (rx/of (dcm/retrieve-comments thread-id)))))))))))

(def ^:private
  schema:handle-file-change
  [:map {:title "handle-file-change"}
   [:type :keyword]
   [:profile-id ::sm/uuid]
   [:file-id ::sm/uuid]
   [:session-id ::sm/uuid]
   [:revn :int]
   [:vern :int]
   [:changes cpc/schema:changes]])

(def ^:private check-file-change-params!
  (sm/check-fn schema:handle-file-change))

(defn handle-file-change
  [{:keys [file-id changes revn vern] :as msg}]

  (dm/assert!
   "expected valid parameters"
   (check-file-change-params! msg))

  (ptk/reify ::handle-file-change
    IDeref
    (-deref [_] {:changes changes})

    ptk/WatchEvent
    (watch [_ _ _]
      ;; The commit event is responsible to apply the data localy
      ;; and update the persistence internal state with the updated
      ;; file-revn

      (rx/of (dch/commit {:file-id file-id
                          :file-revn revn
                          :file-vern vern
                          :save-undo? false
                          :source :remote
                          :redo-changes (vec changes)
                          :undo-changes []})))))

(defn handle-file-deleted
  [{:keys [file-id] :as msg}]
  (ptk/reify ::handle-file-deleted
    ptk/WatchEvent
    (watch [_ state _]
      (let [curr-file-id (:current-file-id state)
            team-id      (:current-team-id state)]
        ;; If the deleted file is the currently open one
        (when (= file-id curr-file-id)
          (rx/of
           (rt/nav :dashboard-recent {:team-id team-id})))))))

(def ^:private
  schema:handle-file-restored
  [:map {:title "handle-file-restored"}
   [:type :keyword]
   [:file-id ::sm/uuid]
   [:vern :int]])

(def ^:private check-file-restored-params
  (sm/check-fn schema:handle-file-restored))

(defn handle-file-restored
  [{:keys [file-id vern] :as msg}]

  (assert (check-file-restored-params msg)
          "expected valid parameters")

  (ptk/reify ::handle-file-restored
    ptk/WatchEvent
    (watch [_ state _]
      (let [curr-file-id    (:current-file-id state)
            file            (dsh/lookup-file state curr-file-id)
            curr-vern       (:vern file)]

        (when (and (= file-id curr-file-id)
                   (not= vern curr-vern))
          (rx/of (ptk/event ::dw/reload-current-file)))))))

(def ^:private schema:handle-library-change
  [:map {:title "handle-library-change"}
   [:type :keyword]
   [:profile-id ::sm/uuid]
   [:file-id ::sm/uuid]
   [:session-id ::sm/uuid]
   [:revn :int]
   [:modified-at ::ct/inst]
   [:changes cpc/schema:changes]])

(def ^:private check-library-change-params
  (sm/check-fn schema:handle-library-change))

(defn handle-library-change
  [{:keys [file-id modified-at changes revn] :as msg}]
  (assert (check-library-change-params msg)
          "expected valid arguments")

  (ptk/reify ::handle-library-change
    ptk/WatchEvent
    (watch [_ state _]
      (when (contains? (:files state) file-id)
        (rx/of (dwl/ext-library-changed file-id modified-at revn changes)
               (dwl/notify-sync-file))))))
