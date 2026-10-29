;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui.workspace.presence
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.config :as cfg]
   [app.main.data.workspace.multiplayer :as dwm]
   [app.main.refs :as refs]
   [app.main.store :as st]
   [app.main.ui.components.dropdown :refer [dropdown]]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [tr]]
   [rumext.v2 :as mf]))

(defn- session-title
  [{:keys [own? followed? full-name page-name spotlight?]}]
  (let [who (if (some? page-name)
              (tr "workspace.presence.on-page" full-name page-name)
              full-name)]
    (cond
      own?       (if spotlight?
                   (tr "workspace.presence.own-spotlighting" who)
                   (tr "workspace.presence.own" who))
      followed?  (tr "workspace.presence.following" who)
      :else      (tr "workspace.presence.follow" who))))

(mf/defc session-widget*
  {::mf/props :obj
   ::mf/memo true}
  [{:keys [session profile index own? followed? spotlight? page-name on-select]}]
  (let [color     (:color session)
        profile   (assoc profile :color color)
        full-name (:fullname profile)
        title     (session-title {:own? own?
                                  :followed? followed?
                                  :spotlight? spotlight?
                                  :full-name full-name
                                  :page-name page-name})
        on-click
        (mf/use-fn
         (mf/deps session own? on-select)
         (fn [event]
           (dom/stop-propagation event)
           (on-select session own?)))]

    [:li {:class (stl/css-case :session-icon true
                               :followed followed?
                               :spotlight (and own? spotlight?))
          :style {:z-index (dm/str (+ 2 (* -1 index)))
                  :background-color color
                  "--session-color" color}}
     [:button {:class (stl/css :session-button)
               :title title
               :aria-label title
               :on-click on-click}
      [:img {:alt full-name
             :style {:background-color color}
             :src (cfg/resolve-profile-photo-url profile)}]]]))

(mf/defc session-row*
  {::mf/props :obj}
  [{:keys [session profile own? followed? page-name on-select]}]
  (let [color     (:color session)
        full-name (:fullname profile)
        on-click
        (mf/use-fn
         (mf/deps session own? on-select)
         (fn [event]
           (dom/stop-propagation event)
           (on-select session own?)))]
    [:li {:class (stl/css-case :session-row true :followed followed?)
          :style {"--session-color" color}}
     [:button {:class (stl/css :session-row-button) :on-click on-click}
      [:img {:alt full-name
             :class (stl/css :session-row-avatar)
             :style {:background-color color}
             :src (cfg/resolve-profile-photo-url (assoc profile :color color))}]
      [:span {:class (stl/css :session-row-name)}
       (if own? (tr "workspace.presence.you" full-name) full-name)]
      (when (some? page-name)
        [:span {:class (stl/css :session-row-page)} page-name])]]))

(mf/defc active-sessions*
  {::mf/memo true}
  []
  (let [profiles     (mf/deref refs/profiles)
        presence     (mf/deref refs/workspace-presence)
        own-id       (mf/deref refs/session-id)
        follow       (mf/deref refs/workspace-follow)
        spotlight?   (mf/deref refs/workspace-spotlight?)
        file-data    (mf/deref refs/workspace-data)
        pages        (:pages-index file-data)

        ;; Others first, then this session.
        sessions     (->> (vals presence)
                          (sort-by #(= own-id (:id %))))
        num-sessions (count sessions)
        max-avatar-count 3
        avatar-count (if (= num-sessions max-avatar-count) max-avatar-count (- max-avatar-count 1))

        open*        (mf/use-state false)
        open?        (deref open*)
        own-menu*    (mf/use-state false)
        own-menu?    (deref own-menu*)

        page-name    (fn [session]
                       (some->> (:page-id session) (get pages) :name))

        on-select
        (mf/use-fn
         (fn [session own?]
           (reset! open* false)
           (if own?
             (swap! own-menu* not)
             (st/emit! (dwm/toggle-following (:id session))))))

        on-open      (mf/use-fn #(swap! open* not))
        on-close     (mf/use-fn #(reset! open* false))
        on-close-own (mf/use-fn #(reset! own-menu* false))

        on-spotlight
        (mf/use-fn
         (fn []
           (reset! own-menu* false)
           (st/emit! (dwm/toggle-spotlight))))]

    [:div {:class (stl/css :active-users-wrapper)}
     [:ul {:class (stl/css :active-users-list) :data-testid "active-users-list"}
      (when (> num-sessions max-avatar-count)
        [:li {:class (stl/css :users-num)}
         [:button {:class (stl/css :users-num-button)
                   :on-click on-open
                   :title (tr "workspace.presence.everyone")}
          (dm/str "+" (+ 1 (- num-sessions max-avatar-count)))]])

      (for [[index session] (d/enumerate (take avatar-count sessions))]
        (let [own? (= own-id (:id session))]
          [:> session-widget*
           {:session session
            :index index
            :own? own?
            :followed? (= (:id session) (:session-id follow))
            :spotlight? spotlight?
            :page-name (page-name session)
            :on-select on-select
            :profile (get profiles (:profile-id session))
            :key (dm/str (:id session))}]))]

     [:& dropdown {:show open? :on-close on-close}
      [:ul {:class (stl/css :sessions-menu)}
       (for [session sessions]
         (let [own? (= own-id (:id session))]
           [:> session-row*
            {:session session
             :own? own?
             :followed? (= (:id session) (:session-id follow))
             :page-name (page-name session)
             :on-select on-select
             :profile (get profiles (:profile-id session))
             :key (dm/str (:id session))}]))]]

     [:& dropdown {:show own-menu? :on-close on-close-own}
      [:ul {:class (stl/css :sessions-menu)}
       [:li {:class (stl/css :own-menu-item)}
        [:button {:class (stl/css :own-menu-button) :on-click on-spotlight}
         (if spotlight?
           (tr "workspace.presence.stop-spotlight")
           (tr "workspace.presence.start-spotlight"))]]
       [:li {:class (stl/css :own-menu-hint)}
        (tr "workspace.presence.spotlight-hint")]]]]))
