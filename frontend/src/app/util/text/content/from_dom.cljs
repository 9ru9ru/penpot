;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.util.text.content.from-dom
  (:require
   [app.common.data :as d]
   [app.common.types.text :as txt]
   [app.util.dom :as dom]
   [app.util.text.content.styles :as styles]))

(defn is-text-node
  [node]
  (and (some? node)
       (= (.-nodeType node) js/Node.TEXT_NODE)))

(defn is-element
  [node tag]
  (and (some? node)
       (= (.-nodeType node) js/Node.ELEMENT_NODE)
       (= (.-nodeName node) (.toUpperCase tag))))

(defn is-line-break
  [node]
  (is-element node "br"))

(defn is-text-span-child
  [node]
  (or (is-line-break node)
      (is-text-node node)))

(defn get-text-span-text
  [element]
  (let [first-child (.-firstChild element)]
    (cond
      ;; A span the browser has emptied. Chrome does this while an input method
      ;; is composing -- it drops the text node and refills it on the next
      ;; keystroke -- so serializing mid-composition used to hit
      ;;   TypeError: Cannot read properties of null (reading 'nodeType')
      ;; from the nil firstChild, and the exception escaped to the error
      ;; boundary and took the whole text editor down. An empty span is empty
      ;; text, which is exactly what a <br>-only span already reports.
      (nil? first-child)
      ""

      (is-line-break first-child)
      ""

      (is-text-node first-child)
      (.-textContent element)

      :else
      (throw (js/TypeError. "Invalid text span child")))))

(defn get-attrs-from-styles
  [element attrs defaults]
  (let [attrs (or attrs [])
        value-empty? (fn [v]
                       (or (nil? v)
                           (and (string? v) (empty? v))))]
    (reduce (fn [acc key]
              (let [style (.-style element)
                    value (if (contains? styles/mapping key)
                            (let [style-name (styles/get-style-name-as-css-variable key)
                                  [_ style-decode] (get styles/mapping key)]
                              (style-decode (.getPropertyValue style style-name)))
                            (let [style-name (styles/get-style-name key)]
                              (styles/normalize-attr-value key (.getPropertyValue style style-name))))
                    default (get defaults key)
                    final-value (if (value-empty? value) default value)]
                ;; Omit attrs with no CSS value when the default is nil (e.g.
                ;; typography-ref-id). Avoids polluting round-tripped content.
                (if (and (value-empty? value) (nil? default))
                  acc
                  (assoc acc key final-value))))
            {} attrs)))

(defn get-text-span-styles
  [element]
  (get-attrs-from-styles element txt/text-span-attrs (txt/get-default-text-attrs)))

(defn get-paragraph-styles
  [element]
  (let [styles (get-attrs-from-styles element
                                      (d/concat-set txt/paragraph-attrs txt/text-node-attrs)
                                      (d/merge txt/default-paragraph-attrs txt/default-text-attrs))
        ;; Recover real font-size from data attribute, which to_dom/get-paragraph-styles may have
        ;; changed to "0" ("0" trick to avoid it interfering with height calculation in the browser).
        saved-font-size (dom/get-data element "saved-font-size")
        saved-font-size (when (and (string? saved-font-size) (not (empty? saved-font-size)))
                          saved-font-size)]
    (cond-> styles
      (some? saved-font-size)
      (assoc :font-size saved-font-size))))

(defn get-root-styles
  [element]
  (get-attrs-from-styles element txt/root-attrs txt/default-root-attrs))

(defn create-text-span
  [element]
  (let [text (get-text-span-text element)]
    (d/merge {:text text
              :key (.-id element)}
             (get-text-span-styles element))))

(defn create-paragraph
  [element]
  (let [children (mapv create-text-span (.-children element))

        ;; A paragraph must carry at least one inline node: the content schema
        ;; declares [:vector {:min 1} ...] for it, so an empty vector makes the
        ;; backend reject the entire update-file request with
        ;; :data-validation / "invalid shape found <id>".
        ;;
        ;; `.-children` yields ELEMENT children only, and an input method
        ;; leaves the syllable it is still composing as a bare text node
        ;; directly under the paragraph -- it is wrapped in an inline span only
        ;; once the syllable is committed. Typing Korean, Japanese or Chinese
        ;; therefore produced :children [] on the keystrokes in between, and
        ;; every save attempted mid-composition failed, so nothing written
        ;; during composition was ever persisted.
        ;;
        ;; Fall back to a single inline node holding the text the paragraph
        ;; currently has, styled from the paragraph element itself.
        children (if (seq children)
                   children
                   [(d/merge {:text (or (.-textContent element) "")}
                             (get-text-span-styles element))])]
    (d/merge {:type "paragraph"
              :key (.-id element)
              :children children}
             (get-paragraph-styles element))))

(defn create-root
  [element]
  (let [root-styles (get-root-styles element)
        paragraphs  (mapv create-paragraph (.-children element))

        ;; Same guard as create-paragraph, one level up. A paragraph-set is
        ;; also declared [:vector {:min 1} ...], and while an input method is
        ;; composing the editor root can transiently hold no element children
        ;; at all -- the text being composed sits directly under it as a text
        ;; node. That produced :children [] here and the backend rejected the
        ;; update with :data-validation / "invalid shape found".
        ;;
        ;; root -> paragraph-set -> paragraph -> inline nodes is the whole
        ;; nesting, and a root always gets exactly one paragraph-set, so this
        ;; and create-paragraph cover every level that can come out empty.
        paragraphs  (if (seq paragraphs)
                      paragraphs
                      [(d/merge {:type "paragraph"
                                 :children [(d/merge {:text (or (.-textContent element) "")}
                                                     (get-text-span-styles element))]}
                                (get-paragraph-styles element))])]
    (d/merge {:type "root"
              :key (.-id element)
              :children [{:type "paragraph-set"
                          :children paragraphs}]}
             root-styles)))
