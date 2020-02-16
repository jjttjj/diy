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
  (:require-macros [diy.core :refer [def-elem-ctors defelem elem]]))

(defn vflatten
  "Takes a sequential collection and returns a flattened vector of any nested
  sequential collections."
  ([x] (vflatten [] x))
  ([acc x] (if (sequential? x) (reduce vflatten acc x) (conj acc x))))

(defprotocol IProp)

(defn prop? [x]
  (satisfies? IProp x))

;;should props actually be handled as pairs/map entries?
;;so that multipl of the same can be handled arbitraily?
(defn parse-args
  "Parses a sequence of element arguments and returns a vector of
  properties, children and mutations"
  [args]
  (loop [props        (transient {})
         kids         (transient [])
         [arg & args] args]
    (if-not (or arg args)
      [(persistent! props) (persistent! kids)]
      (cond
        (map? arg)    (recur (reduce-kv assoc! props arg) kids args)
        (prop? arg)   (recur (assoc! props arg (first args)) kids (rest args))
        (seq? arg)    (recur props (reduce conj! kids (vflatten arg)) args)
        (vector? arg) (recur props (reduce conj! kids (vflatten arg)) args)
        :else         (recur props (conj! kids arg) args)))))

;;todo: should we expose the type to extend? ;;and also the createtag?
(defn mkinvoke! [handle-props handle-kids]
  (let [invoke!
        (fn [el & args]
          (let [[props kids] (parse-args args)]
            (handle-props el props)
            (handle-kids el kids)
            el))]
    (extend-type js/Element
      IFn
      (-invoke
        ([this]
         (invoke! this))
        ([this a]
         (invoke! this a))
        ([this a b]
         (invoke! this a b))
        ([this a b c]
         (invoke! this a b c))
        ([this a b c d]
         (invoke! this a b c d))
        ([this a b c d e]
         (invoke! this a b c d e))
        ([this a b c d e f]
         (invoke! this a b c d e f))
        ([this a b c d e f g]
         (invoke! this a b c d e f g))
        ([this a b c d e f g h]
         (invoke! this a b c d e f g h))
        ([this a b c d e f g h i]
         (invoke! this a b c d e f g h i))
        ([this a b c d e f g h i j]
         (invoke! this a b c d e f g h i j))
        ([this a b c d e f g h i j k]
         (invoke! this a b c d e f g h i j k))
        ([this a b c d e f g h i j k l]
         (invoke! this a b c d e f g h i j k l))
        ([this a b c d e f g h i j k l m]
         (invoke! this a b c d e f g h i j k l m))
        ([this a b c d e f g h i j k l m n]
         (invoke! this a b c d e f g h i j k l m n))
        ([this a b c d e f g h i j k l m n o]
         (invoke! this a b c d e f g h i j k l m n o))
        ([this a b c d e f g h i j k l m n o p]
         (invoke! this a b c d e f g h i j k l m n o p))
        ([this a b c d e f g h i j k l m n o p q]
         (invoke! this a b c d e f g h i j k l m n o p q))
        ([this a b c d e f g h i j k l m n o p q r]
         (invoke! this a b c d e f g h i j k l m n o p q r))
        ([this a b c d e f g h i j k l m n o p q r s]
         (invoke! this a b c d e f g h i j k l m n o p q r s))
        ([this a b c d e f g h i j k l m n o p q r s t]
         (invoke! this a b c d e f g h i j k l m n o p q r s t))
        ([this a b c d e f g h i j k l m n o p q r s t rest]
         (invoke! this a b c d e f g h i j k l m n o p q r s t rest))))))

(defn elem-fn
  "Returns a function which creates a dom node of type tagname."
  [tag-name]
  (fn [& args]
    (apply (gdom/createElement tag-name) args)))


