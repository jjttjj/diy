;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns diy.core)

;;from hoplon
(defmacro elem
  "Create an anonymous custom element."
  [parse-rgs bind & body]
  `(fn [& args#] (let [~bind (parse-args args#)] ~@body)))

(defmacro defelem
  "Defines an element function.
  An element function creates a DOM Element (parent) given two arguments:
    * `attrs` - a number of key-value pairs for attributes and their values
    * `kids` - a sequence of DOM Elements to be appended/used inside
  The returned DOM Element is itself a function which can accept more
  attributes and child elements."
  [name & forms]
  (let [[_ name [_ & [[bind & body]]]] (macroexpand-1 `(defn ~name ~@forms))]
    `(def ~name (elem parse-args ~bind ~@body))))


(def default-elem-names
  ["a" "abbr" "address" "area" "article" "aside" "audio" "b" "base" "bdi" "bdo"
  "blockquote" "body" "br" "button" "canvas" "caption" "cite" "code" "col"
  "colgroup" "data" "datalist" "dd" "del" "details" "dfn" "dialog" "div" "dl"
  "dt" "em" "embed" "fieldset" "figure" "footer" "form" "h1" "h2" "h3" "h4" "h5"
  "h6" "head" "header" "hgroup" "hr" "html" "i" "iframe" "img" "input" "ins"
  "kbd" "keygen" "label" "legend" "li" "link" "main" "map" "mark" "menu"
  "menuitem" "meta" "meter" "nav" "noscript" "object" "ol" "optgroup" "option"
  "output" "p" "param" "pre" "progress" "q" "rb" "rp" "rt" "rtc" "ruby" "s"
  "samp" "script" "section" "select" "small" "source" "span" "strong" "style"
  "sub" "summary" "sup" "table" "tbody" "td" "template" "textarea" "tfoot" "th"
  "thead" "time" "title" "tr" "track" "u" "ul" "var" "video" "wbr"])


;;todo: maybe take as an arg sym->str?
(defmacro def-elem-ctors [elem-fn
                          & {:keys [tag-names exclude rename]
                             :or   {exclude [] rename {}}}]
  `(do
     ~@(for [el-str# (remove (set (map name exclude)) (or tag-names
                                                          default-elem-names))]
         (let [sym# (symbol (get rename el-str# el-str#))]
           `(def ~sym# (~elem-fn ~el-str#))))))
