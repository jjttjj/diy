;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns diy.core
  (:require [goog.dom :as gdom]
            [goog.dom.classlist :as class]
            [goog.array :as garr]
            [goog.object :as gobj]
            [goog.style :as gsty]
            [clojure.string :as str])
  (:require-macros [diy.core]))

;;should props actually be handled as a sequence of pairs?
;;so that multiple of the same can be handled arbitraily
;;or that ordering can be used?
(defn parse-args
  [prop? args]
  (loop [props        {}
         kids         []
         [arg & args] args]
    (if-not (or arg args)
      [props kids]
      (cond
        (map? arg)        (recur (reduce-kv assoc props arg) kids args)
        (prop? arg)       (recur (assoc props arg (first args)) kids (rest args))
        (sequential? arg) (recur props kids (concat arg args))
        :else             (recur props (conj kids arg) args)))))

(defn specify-invoke [x invoke-fn]
  (specify! x
    IFn
    (-invoke
      ([this]
       (invoke-fn))
      ([this a]
       (invoke-fn a))
      ([this a b]
       (invoke-fn a b))
      ([this a b c]
       (invoke-fn a b c))
      ([this a b c d]
       (invoke-fn a b c d))
      ([this a b c d e]
       (invoke-fn a b c d e))
      ([this a b c d e f]
       (invoke-fn a b c d e f))
      ([this a b c d e f g]
       (invoke-fn a b c d e f g))
      ([this a b c d e f g h]
       (invoke-fn a b c d e f g h))
      ([this a b c d e f g h i]
       (invoke-fn a b c d e f g h i))
      ([this a b c d e f g h i j]
       (invoke-fn a b c d e f g h i j))
      ([this a b c d e f g h i j k]
       (invoke-fn a b c d e f g h i j k))
      ([this a b c d e f g h i j k l]
       (invoke-fn a b c d e f g h i j k l))
      ([this a b c d e f g h i j k l m]
       (invoke-fn a b c d e f g h i j k l m))
      ([this a b c d e f g h i j k l m n]
       (invoke-fn a b c d e f g h i j k l m n))
      ([this a b c d e f g h i j k l m n o]
       (invoke-fn a b c d e f g h i j k l m n o))
      ([this a b c d e f g h i j k l m n o p]
       (invoke-fn a b c d e f g h i j k l m n o p))
      ([this a b c d e f g h i j k l m n o p q]
       (invoke-fn a b c d e f g h i j k l m n o p q))
      ([this a b c d e f g h i j k l m n o p q r]
       (invoke-fn a b c d e f g h i j k l m n o p q r))
      ([this a b c d e f g h i j k l m n o p q r s]
       (invoke-fn a b c d e f g h i j k l m n o p q r s))
      ([this a b c d e f g h i j k l m n o p q r s t]
       (invoke-fn a b c d e f g h i j k l m n o p q r s t))
      ([this a b c d e f g h i j k l m n o p q r s t rest]
       (invoke-fn a b c d e f g h i j k l m n o p q r s t rest)))))

(defn invoker [this handle-props handle-kids & [{:keys [prop?]
                                                 :or   {prop? keyword?}
                                                 :as   opt}]]
  (fn [& args]
    (let [[props kids] (parse-args prop? args)]
      (handle-props this props)
      (handle-kids this kids))))

(defn specify-invoker [this handle-props handle-kids & [{:keys [prop?]
                                                         :or   {prop? keyword?}
                                                         :as   opt}]]
  (specify-invoke this
    (invoker this handle-props handle-kids opt)))
