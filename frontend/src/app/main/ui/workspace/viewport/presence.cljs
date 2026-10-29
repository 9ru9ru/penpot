;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui.workspace.viewport.presence
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data.macros :as dm]
   [app.common.time :as ct]
   [app.main.data.workspace.multiplayer :as dwm]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.streams :as ms]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [tr]]
   [app.util.keyboard :as kbd]
   [app.util.timers :as ts]
   [beicon.v2.core :as rx]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

(def pointer-path
  (dm/str "M11.58,-0.47L11.47,-0.35L0.34,10.77L0.30,10.96L-0.46,"
          "15.52L4.29,14.72L15.53,3.47L11.58,-0.47ZL11.58,"
          "-0.47ZL11.58,-0.47ZM11.58,1.3C12.31,2.05,13.02,"
          "2.742,13.76,3.47L4.0053,13.23C3.27,12.50,2.55,"
          "11.78,1.82,11.05L11.58,1.30ZL11.58,1.30ZM1.37,12.15L2.90,"
          "13.68L1.67,13.89L1.165,13.39L1.37,12.15ZL1.37,12.15Z"))

;; A cursor stays on screen this long after it last moved.
(def ^:private cursor-idle-ms 30000)

;; Chat bubbles disappear if their sender stopped sending for this long.
(def ^:private chat-stale-ms 8000)

(def ^:private click-ripple-ms 700)

(defn- age-ms
  [inst]
  (if (some? inst)
    (- (inst-ms (ct/now)) (inst-ms inst))
    js/Infinity))

(defn- short-name
  [fullname]
  (if (> (count fullname) 16)
    (dm/str (str/slice fullname 0 12) "...")
    fullname))

(mf/defc session-cursor*
  {::mf/wrap [mf/memo]}
  [{:keys [session profile zoom]}]
  (let [point     (:point session)
        bg-color  (:color session)
        fg-color  "var(--app-white)"
        transform (str/ffmt "translate(%, %) scale(%)"
                            (dm/get-prop point :x)
                            (dm/get-prop point :y)
                            (/ 1 zoom))
        chat      (when (> chat-stale-ms (age-ms (:updated-at session)))
                    (:chat session))
        fullname  (short-name (:fullname profile))]

    [:g {:class (stl/css :multiuser-cursor) :transform transform}
     [:path {:fill bg-color :d pointer-path}]
     [:g {:transform "translate(17 -10)"}
      [:foreignObject {:x -0.3
                       :y -12.5
                       :width 300
                       :height 160}
       (if (some? chat)
         [:div {:class (stl/css :chat-bubble)
                :style {:background-color bg-color :color fg-color}}
          [:span {:class (stl/css :chat-name)} fullname]
          [:span {:class (stl/css :chat-text)} chat]]
         [:div {:class (stl/css :profile-name)
                :style {:background-color bg-color :color fg-color}}
          fullname])]]]))

(mf/defc click-ripple*
  {::mf/wrap [mf/memo]}
  [{:keys [point color zoom]}]
  [:circle {:class (stl/css :click-ripple)
            :cx (dm/get-prop point :x)
            :cy (dm/get-prop point :y)
            :r (/ 18 zoom)
            :fill "none"
            :stroke color
            :stroke-width (/ 2 zoom)
            :style {:transform-origin (dm/str (dm/get-prop point :x) "px "
                                              (dm/get-prop point :y) "px")}}])

(mf/defc session-selection*
  {::mf/wrap [mf/memo]}
  [{:keys [selected objects color zoom]}]
  [:g {:class (stl/css :multiuser-selection)
       :clip-path "url(#clip-handlers)"}
   (for [id selected]
     (when-let [points (:points (get objects id))]
       [:polygon {:key (dm/str id)
                  :points (->> points
                               (map #(dm/str (dm/get-prop % :x) "," (dm/get-prop % :y)))
                               (str/join " "))
                  :fill "none"
                  :stroke color
                  :stroke-width (/ 1.5 zoom)}]))])

(mf/defc active-cursors
  {::mf/props :obj}
  [{:keys [page-id]}]
  (let [counter  (mf/use-state 0)
        profiles (mf/deref refs/profiles)
        sessions (mf/deref refs/workspace-presence)
        own-id   (mf/deref refs/session-id)
        objects  (mf/deref refs/workspace-page-objects)
        zoom     (mf/deref refs/selected-zoom)

        sessions (->> (vals sessions)
                      (remove #(= own-id (:id %)))
                      (filter #(= page-id (:page-id %))))

        cursors  (->> sessions
                      (filter :point)
                      (filter #(> cursor-idle-ms (age-ms (:point-at %)))))

        ripples  (->> sessions
                      (filter :click-point)
                      (filter #(> click-ripple-ms (age-ms (:click-at %)))))]

    (mf/with-effect nil
      (let [sem (ts/interval 500 #(swap! counter inc))]
        (fn [] (rx/dispose! sem))))

    [:g {:class (stl/css :multiuser-layer)}
     (for [session sessions]
       (when (seq (:selected session))
         [:> session-selection*
          {:key (dm/str "sel-" (:id session))
           :selected (:selected session)
           :objects objects
           :color (:color session)
           :zoom zoom}]))

     (for [session ripples]
       [:> click-ripple*
        {:key (dm/str "click-" (:click-id session))
         :point (:click-point session)
         :color (:color session)
         :zoom zoom}])

     (for [session cursors]
       [:> session-cursor*
        {:session session
         :zoom zoom
         :profile (get profiles (:profile-id session))
         :key (dm/str (:id session))}])]))

;; --- Screen-space overlays (HTML, over the viewport)

(mf/defc follow-frame*
  "Colored frame and banner while following someone or spotlighting."
  {::mf/props :obj}
  []
  (let [follow     (mf/deref refs/workspace-follow)
        spotlight? (mf/deref refs/workspace-spotlight?)
        presence   (mf/deref refs/workspace-presence)
        profiles   (mf/deref refs/profiles)
        own-id     (mf/deref refs/session-id)

        leader     (some->> (:session-id follow) (get presence))
        own        (get presence own-id)
        watchers   (when spotlight?
                     (->> (vals presence)
                          (filter #(= own-id (:following %)))
                          (count)))

        on-stop-follow (mf/use-fn #(st/emit! (dwm/stop-following)))
        on-stop-spot   (mf/use-fn #(st/emit! (dwm/stop-spotlight)))]

    (cond
      (some? leader)
      (let [color (:color leader)
            name  (:fullname (get profiles (:profile-id leader)))]
        [:div {:class (stl/css :follow-frame) :style {"--session-color" color}}
         [:div {:class (stl/css :follow-banner)}
          [:span (if (= :spotlight (:reason follow))
                   (tr "workspace.presence.banner-spotlight" name)
                   (tr "workspace.presence.banner-following" name))]
          [:button {:class (stl/css :follow-stop) :on-click on-stop-follow}
           (tr "workspace.presence.banner-stop")]]])

      spotlight?
      [:div {:class (stl/css :follow-frame) :style {"--session-color" (:color own)}}
       [:div {:class (stl/css :follow-banner)}
        [:span (tr "workspace.presence.banner-spotlighting" (or watchers 0))]
        [:button {:class (stl/css :follow-stop) :on-click on-stop-spot}
         (tr "workspace.presence.banner-stop-spotlight")]]]

      :else
      nil)))

(mf/defc cursor-chat-input*
  "The local cursor chat: an input that rides along with the pointer."
  {::mf/props :obj}
  [{:keys [vbox zoom]}]
  (let [chat     (mf/deref refs/workspace-cursor-chat)
        own-id   (mf/deref refs/session-id)
        presence (mf/deref refs/workspace-presence)
        color    (:color (get presence own-id))
        open?    (:open? chat)
        point*   (mf/use-state nil)
        point    (deref point*)
        input-ref (mf/use-ref nil)

        on-change
        (mf/use-fn
         (fn [event]
           (st/emit! (dwm/update-cursor-chat (dom/get-target-val event)))))

        on-key-down
        (mf/use-fn
         (fn [event]
           (dom/stop-propagation event)
           (cond
             ;; Enter that confirms an IME composition is not a send.
             (and (kbd/enter? event) (not (.-isComposing ^js event)))
             (do (dom/prevent-default event)
                 (st/emit! (dwm/send-cursor-chat))
                 (some-> (mf/ref-val input-ref) (dom/set-value! "")))

             (kbd/esc? event)
             (do (dom/prevent-default event)
                 (st/emit! (dwm/close-cursor-chat))))))

        on-blur
        (mf/use-fn
         (fn []
           (st/emit! (dwm/send-cursor-chat)
                     (dwm/close-cursor-chat))))]

    (mf/with-effect [open?]
      (when open?
        (reset! point* @ms/mouse-position)
        (let [sub (rx/subscribe ms/mouse-position #(reset! point* %))]
          #(rx/dispose! sub))))

    (when (and open? (some? point) (some? vbox))
      (let [x (* (- (dm/get-prop point :x) (:x vbox)) zoom)
            y (* (- (dm/get-prop point :y) (:y vbox)) zoom)]
        [:div {:class (stl/css :chat-input-wrapper)
               :style {:left (dm/str (+ x 18) "px")
                       :top (dm/str (+ y 14) "px")
                       "--session-color" (or color "var(--color-accent-primary)")}}
         ;; Uncontrolled: feeding the value back while a Hangul syllable
         ;; is being composed breaks the composition.
         [:input {:class (stl/css :chat-input)
                  :ref input-ref
                  :auto-focus true
                  :max-length 120
                  :placeholder (tr "workspace.presence.chat-placeholder")
                  :on-change on-change
                  :on-key-down on-key-down
                  :on-blur on-blur}]]))))
