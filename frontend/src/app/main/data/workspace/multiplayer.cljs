;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.data.workspace.multiplayer
  "Figma-like multiplayer: following another session's view, spotlight,
  cursor chat, and the state every session shares about itself (page,
  view box, selection, clicks).

  All of it travels inside `:pointer-update` websocket messages. The
  backend relays those to the other subscribers of the file without
  looking at their keys, so none of this needs backend support."
  (:require
   [app.common.data.macros :as dm]
   [app.common.geom.align :as gal]
   [app.common.time :as ct]
   [app.common.uuid :as uuid]
   [app.main.data.common :as dcm]
   [app.main.data.helpers :as dsh]
   [app.main.data.workspace.viewport-wasm :as dwvw]
   [app.util.mouse :as mse]
   [beicon.v2.core :as rx]
   [potok.v2.core :as ptk]))

;; --- Local state shared with the other sessions

(def ^:private max-shared-selection 200)

(def chat-linger-ms
  "How long a sent cursor-chat message stays visible to the others."
  5000)

(defn- chat-payload
  [{:keys [open? text last last-at]}]
  (or (when open? (not-empty text))
      (when (and (some? last)
                 (> chat-linger-ms (- (inst-ms (ct/now)) (inst-ms last-at))))
        last)))

(defn local-state
  "What this session tells the others about itself, besides the pointer."
  [state]
  (let [local (:workspace-local state)]
    {:page-id   (:current-page-id state)
     :zoom      (:zoom local)
     :zoom-inverse (:zoom-inverse local)
     :vbox      (:vbox local)
     :vport     (:vport local)
     :selected  (into [] (take max-shared-selection) (:selected local))
     :spotlight (true? (:workspace-spotlight state))
     :following (dm/get-in state [:workspace-follow :session-id])
     :chat      (chat-payload (:workspace-cursor-chat state))}))

;; --- Following

(def ^:private user-view-events
  "Events through which the local user moves the view by themselves.
  Any of them ends following."
  #{:app.main.data.workspace.viewport/update-viewport-position
    :app.main.data.workspace.viewport/start-panning
    :app.main.data.workspace.zoom/increase-zoom
    :app.main.data.workspace.zoom/decrease-zoom
    :app.main.data.workspace.zoom/set-zoom
    :app.main.data.workspace.zoom/reset-zoom
    :app.main.data.workspace.zoom/zoom-to-fit-all
    :app.main.data.workspace.zoom/fit-to-shapes
    :app.main.data.workspace.zoom/center-on-shape
    :app.main.data.workspace.zoom/start-zooming})

(defn- user-moved-view?
  [event]
  (or (contains? user-view-events (ptk/type event))
      ;; A pointer down on the canvas (select, draw, drag...).
      (and (mse/mouse-event? event)
           (mse/mouse-down-event? event))))

(declare stop-following)

(defn- follow-view
  "Shows the whole of the leader's visible area in the local viewport,
  centered, on the leader's page."
  [session-id]
  (ptk/reify ::follow-view
    ptk/UpdateEvent
    (update [_ state]
      (let [{:keys [vbox page-id]} (dm/get-in state [:workspace-presence session-id])
            vport (dm/get-in state [:workspace-local :vport])]
        (if (and (some? vbox)
                 (some? vport)
                 (pos? (:width vbox))
                 (pos? (:height vbox))
                 (= page-id (:current-page-id state))
                 (not (dwvw/render-context-lost? state)))
          (let [srect (gal/adjust-to-viewport vport vbox {:padding 0 :min-zoom 0.01})
                zoom  (/ (:width vport) (:width srect))]
            (update state :workspace-local
                    (fn [local]
                      (-> local
                          (assoc :zoom zoom)
                          (assoc :zoom-inverse (/ 1 zoom))
                          (update :vbox merge (select-keys srect [:x :y :width :height]))))))
          state)))

    ptk/WatchEvent
    (watch [_ state _]
      (let [session (dm/get-in state [:workspace-presence session-id])
            page-id (:page-id session)
            follow  (:workspace-follow state)]
        (cond
          (nil? session)
          (rx/of (stop-following {:dismiss? false}))

          (and (some? page-id)
               (not= page-id (:current-page-id state))
               (not= page-id (:navigating-to follow))
               (contains? (:pages-index (dsh/lookup-file-data state)) page-id))
          (rx/of (ptk/event ::navigating page-id)
                 (dcm/go-to-workspace :page-id page-id))

          :else
          (rx/empty))))

    ptk/EffectEvent
    (effect [_ state _]
      (dwvw/maybe-sync-workspace-local-viewport! state))))

(defn- remote-update-from?
  [session-id event]
  (and (= :app.main.data.workspace.notifications/handle-pointer-update (ptk/type event))
       (= session-id (:session-id (deref event)))))

(defn start-following
  "Makes the local viewport track `session-id` until the user moves the
  view, clicks the canvas, presses Escape or follows someone else.
  `reason` is :manual or :spotlight."
  ([session-id]
   (start-following session-id :manual))
  ([session-id reason]
   (let [follow-id (uuid/next)]
     (ptk/reify ::start-following
       IDeref
       (-deref [_] {:follow-id follow-id})

       ptk/UpdateEvent
       (update [_ state]
         (if (= session-id (:session-id state))
           state
           (assoc state :workspace-follow {:id follow-id
                                           :session-id session-id
                                           :reason reason})))

       ptk/WatchEvent
       (watch [_ state stream]
         (when (not= session-id (:session-id state))
           (let [stopper (->> stream
                              (rx/filter (fn [event]
                                           (or (= ::stop-following (ptk/type event))
                                               (and (= ::start-following (ptk/type event))
                                                    (not= follow-id (:follow-id (deref event)))))))
                              (rx/take 1))]
             (->> (rx/merge
                   (rx/of (follow-view session-id))

                   (->> stream
                        (rx/filter (partial remote-update-from? session-id))
                        (rx/map #(follow-view session-id)))

                   ;; Re-apply once a page change we asked for has landed.
                   (->> stream
                        (rx/filter (ptk/type? :app.main.data.workspace.pages/initialize-page))
                        (rx/delay 50)
                        (rx/map #(follow-view session-id)))

                   ;; The leader left without a last message.
                   (->> (rx/interval 1000)
                        (rx/map #(follow-view session-id)))

                   (->> stream
                        (rx/filter user-moved-view?)
                        (rx/take 1)
                        (rx/map #(stop-following))))
                  (rx/take-until stopper)))))))))

(defn stop-following
  "Stops following. Leaving a spotlight by hand keeps it from pulling the
  user back in until it ends (`:dismiss? true`, the default)."
  ([] (stop-following nil))
  ([{:keys [dismiss?] :or {dismiss? true}}]
   (ptk/reify ::stop-following
     ptk/UpdateEvent
     (update [_ state]
       (let [{:keys [session-id reason]} (:workspace-follow state)]
         (cond-> (dissoc state :workspace-follow)
           (and dismiss? (= :spotlight reason))
           (update :workspace-spotlight-dismissed (fnil conj #{}) session-id)))))))

(defn toggle-following
  [session-id]
  (ptk/reify ::toggle-following
    ptk/WatchEvent
    (watch [_ state _]
      (if (= session-id (dm/get-in state [:workspace-follow :session-id]))
        (rx/of (stop-following))
        (rx/of (start-following session-id))))))

(defmethod ptk/resolve ::navigating
  [_ page-id]
  (ptk/reify ::navigating
    ptk/UpdateEvent
    (update [_ state]
      (if (contains? state :workspace-follow)
        (assoc-in state [:workspace-follow :navigating-to] page-id)
        state))))

;; --- Spotlight

(defn toggle-spotlight
  []
  (ptk/reify ::toggle-spotlight
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-spotlight not))

    ptk/WatchEvent
    (watch [_ state _]
      (when (and (:workspace-spotlight state)
                 (contains? state :workspace-follow))
        (rx/of (stop-following {:dismiss? false}))))))

(defn stop-spotlight
  []
  (ptk/reify ::stop-spotlight
    ptk/UpdateEvent
    (update [_ state]
      (dissoc state :workspace-spotlight))))

(defn check-spotlight
  "Joins or leaves a remote session's spotlight after one of its updates."
  [session-id]
  (ptk/reify ::check-spotlight
    ptk/UpdateEvent
    (update [_ state]
      (let [session (dm/get-in state [:workspace-presence session-id])]
        (if (and (not (:spotlight session))
                 (contains? (:workspace-spotlight-dismissed state) session-id))
          (update state :workspace-spotlight-dismissed disj session-id)
          state)))

    ptk/WatchEvent
    (watch [_ state _]
      (let [session (dm/get-in state [:workspace-presence session-id])
            follow  (:workspace-follow state)]
        (cond
          (and (:spotlight session)
               (not (:workspace-spotlight state))
               (not= session-id (:session-id state))
               (not= session-id (:session-id follow))
               (not (contains? (:workspace-spotlight-dismissed state) session-id)))
          (rx/of (start-following session-id :spotlight))

          (and (not (:spotlight session))
               (= session-id (:session-id follow))
               (= :spotlight (:reason follow)))
          (rx/of (stop-following {:dismiss? false}))

          :else
          (rx/empty))))))

;; --- Cursor chat

(defn open-cursor-chat
  []
  (ptk/reify ::open-cursor-chat
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-cursor-chat assoc :open? true :text ""))))

(defn update-cursor-chat
  [text]
  (ptk/reify ::update-cursor-chat
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-cursor-chat :text] text))))

(defn send-cursor-chat
  "Keeps the typed message on the others' screens for a few seconds and
  clears the input for the next one."
  []
  (ptk/reify ::send-cursor-chat
    ptk/UpdateEvent
    (update [_ state]
      (let [text (dm/get-in state [:workspace-cursor-chat :text])]
        (if (empty? text)
          state
          (update state :workspace-cursor-chat assoc
                  :text ""
                  :last text
                  :last-at (ct/now)))))))

(defn close-cursor-chat
  []
  (ptk/reify ::close-cursor-chat
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-cursor-chat assoc :open? false :text ""))))
