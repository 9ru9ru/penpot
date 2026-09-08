
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui.workspace.sidebar.options.drawing
  (:require
   [app.main.data.workspace.drawing.common :as dwdc]
   [app.main.ui.workspace.sidebar.options.drawing.frame :as frame]
   [app.main.ui.workspace.sidebar.options.drawing.stroke :as stroke]
   [rumext.v2 :as mf]))

(mf/defc drawing-options*
  {::mf/wrap [#(mf/throttle % 60)]
   ::mf/private true}
  [{:keys [drawing-state] :as props}]
  (let [tool (:tool drawing-state)]
    (cond
      (= tool :frame)
      [:> frame/options* {:drawing-state drawing-state}]

      ;; The pen, path, line and arrow tools all draw a stroked path,
      ;; so they share the stroke the next shape will be created with
      (contains? dwdc/stroke-tools tool)
      [:> stroke/options* {:tool tool}]

      :else nil)))


