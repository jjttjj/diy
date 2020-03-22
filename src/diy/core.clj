;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns diy.core)

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

(def default-elem-syms
  [a abbr address area article aside audio b base bdi bdo blockquote body br
   button canvas caption cite code col colgroup data datalist dd del details dfn
   dialog div dl dt em embed fieldset figure footer form h1 h2 h3 h4 h5 h6 head
   header hgroup hr html i iframe img input ins kbd keygen label legend li link
   main map mark menu menuitem meta meter nav noscript object ol optgroup option
   output p param pre progress q rb rp rt rtc ruby s samp script section select
   small source span strong style sub summary sup table tbody td template
   textarea tfoot th thead time title tr track u ul var video wbr])


(defmacro def-elem-ctors [elem-fn
                          & {:keys [tag-syms exclude rename]
                             :or   {exclude [] rename {}}}]
  (vec
    (for [sym# (remove (set exclude) (or tag-syms default-elem-syms))]
      `(def ~(get rename sym# sym#) (~elem-fn ~(name sym#))))))

;;note the invoke-fn in `extend-invoke` NEEDs a `this` first arg while the
;;function passed to `specify-invoke` isn't passed `this`.
(defmacro extend-invoke [type invoke-fn]
  `(extend-type ~type
     cljs.core/IFn
     (cljs.core/-invoke
       ([this#]
        (~invoke-fn this#))
       ([this# a#]
        (~invoke-fn this# a#))
       ([this# a# b#]
        (~invoke-fn this# a# b#))
       ([this# a# b# c#]
        (~invoke-fn this# a# b# c#))
       ([this# a# b# c# d#]
        (~invoke-fn this# a# b# c# d#))
       ([this# a# b# c# d# e#]
        (~invoke-fn this# a# b# c# d# e#))
       ([this# a# b# c# d# e# f#]
        (~invoke-fn this# a# b# c# d# e# f#))
       ([this# a# b# c# d# e# f# g#]
        (~invoke-fn this# a# b# c# d# e# f# g#))
       ([this# a# b# c# d# e# f# g# h#]
        (~invoke-fn this# a# b# c# d# e# f# g# h#))
       ([this# a# b# c# d# e# f# g# h# i#]
        (~invoke-fn this# a# b# c# d# e# f# g# h# i#))
       ([this# a# b# c# d# e# f# g# h# i# j#]
        (~invoke-fn this# a# b# c# d# e# f# g# h# i# j#))
       ([this# a# b# c# d# e# f# g# h# i# j# k#]
        (~invoke-fn this# a# b# c# d# e# f# g# h# i# j# k#))
       ([this# a# b# c# d# e# f# g# h# i# j# k# l#]
        (~invoke-fn this# a# b# c# d# e# f# g# h# i# j# k# l#))
       ([this# a# b# c# d# e# f# g# h# i# j# k# l# m#]
        (~invoke-fn this# a# b# c# d# e# f# g# h# i# j# k# l# m#))
       ([this# a# b# c# d# e# f# g# h# i# j# k# l# m# n#]
        (~invoke-fn this# a# b# c# d# e# f# g# h# i# j# k# l# m# n#))
       ([this# a# b# c# d# e# f# g# h# i# j# k# l# m# n# o#]
        (~invoke-fn this# a# b# c# d# e# f# g# h# i# j# k# l# m# n# o#))
       ([this# a# b# c# d# e# f# g# h# i# j# k# l# m# n# o# p#]
        (~invoke-fn this# a# b# c# d# e# f# g# h# i# j# k# l# m# n# o# p#))
       ([this# a# b# c# d# e# f# g# h# i# j# k# l# m# n# o# p# q#]
        (~invoke-fn this# a# b# c# d# e# f# g# h# i# j# k# l# m# n# o# p# q#))
       ([this# a# b# c# d# e# f# g# h# i# j# k# l# m# n# o# p# q# r#]
        (~invoke-fn this# a# b# c# d# e# f# g# h# i# j# k# l# m# n# o# p# q# r#))
       ([this# a# b# c# d# e# f# g# h# i# j# k# l# m# n# o# p# q# r# s#]
        (~invoke-fn this# a# b# c# d# e# f# g# h# i# j# k# l# m# n# o# p# q# r# s#))
       ([this# a# b# c# d# e# f# g# h# i# j# k# l# m# n# o# p# q# r# s# t#]
        (~invoke-fn this# a# b# c# d# e# f# g# h# i# j# k# l# m# n# o# p# q# r# s# t#))
       ([this# a# b# c# d# e# f# g# h# i# j# k# l# m# n# o# p# q# r# s# t# rest#]
        (~invoke-fn this# a# b# c# d# e# f# g# h# i# j# k# l# m# n# o# p# q# r# s# t# rest#)))))
