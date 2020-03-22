;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns diy.hoplon
  (:require [diy.core :as d]
            [goog.object :as obj]))

(defn cell? 
  "Returns c if c is a Cell, nil otherwise."
  [c]
  ;;(when (= (type c) javelin.core.Cell) c)
  
  (satisfies? cljs.core/IWatchable c))

(defprotocol IHoplonNode
  (-node [this]))

(defn node? [this]
  (satisfies? IHoplonNode this))

(defn- ->node [x]
  (if (node? x) (-node x) x))

(def $text #(.createTextNode js/document %))

(extend-protocol IHoplonNode
  string
  (-node [this]
    ($text this))
  number
  (-node [this]
    ($text (str this))))

(defn child-vec
  [this]
  (let [x (.-childNodes this)]
    (areduce x i ret [] (conj ret (.item x i)))))

(defn- vflatten
  "Takes a sequential collection and returns a flattened vector of any nested
  sequential collections."
  ([x] (persistent! (vflatten (transient []) x)))
  ([acc x] (if (sequential? x) (reduce vflatten acc x) (conj! acc x))))

(defn- remove-nil [nodes]
  (reduce #(if %2 (conj %1 %2) %1) [] nodes))

(defn- compact-kids
  "Flattens nested sequencences of elements, removing nil values."
  [kids]
  (->>
   (vflatten kids)
   (remove-nil)
   (mapv ->node)))


(defn- set-dom-children!
  "Sets a DOM element's children to the sequence of children given."
  [elem new-kids]
  (let [new-kids (compact-kids new-kids)
        new?     (set new-kids)]
    (loop [[new-kid & nks]              new-kids
           [old-kid & oks :as old-kids] (child-vec elem)]
      (when (or new-kid old-kid)
        
        (cond
          (= new-kid old-kid) (recur nks oks)
          (not old-kid)       (do (.appendChild elem new-kid)
                                  (recur nks oks))
          (not new-kid)       (do (when-not (new? old-kid) (.removeChild elem old-kid))
                                  (recur nks oks))
          :else               (do (.insertBefore elem new-kid old-kid)
                                  (recur nks old-kids)))))))

(defn do-watch
  "Adds f as a watcher to ref and evaluates (f init @ref) once. The watcher
  f is a function of two arguments: the previous and next values. If init is
  not provided the default (nil) will be used."
  ([ref f]
   (do-watch ref nil f))
  ([ref init f]
   (let [k (gensym)]
     (f init @ref)
     (add-watch ref k (fn [_ _ old new] (f old new)))
     k)))

(defn set-attributes! [elem kvs]
  (let [e elem]
    (doseq [[k v] kvs :let [k (name k)]]
      (if-not v
        (.removeAttribute e k)
        (.setAttribute e k (if (true? v) k v))))))

(defn set-styles! [this kvs]
  (let [e this]
    (doseq [[k v] kvs]
      (obj/set (.. e -style) (name k) (str v)))))

(defn hoplon-kids [elem]
  (if-let [hl-kids  (.-hoplonKids elem)]
    hl-kids
    (let [kids (atom (child-vec elem))]
      (set! (.-hoplonKids elem) kids)
      (do-watch kids #(set-dom-children! elem %2))
      kids)))

(defn append-child! [elem child]
  (let [kids (hoplon-kids elem)
        i    (count @kids)]
    (if (cell? child)
      (do-watch child #(do
                         (swap! kids assoc i %2)))
      (swap! kids assoc i child))
    child))

(defn remove-child! [elem child]
  (let [kids         (hoplon-kids elem)
        before-count (count @kids)]
    (if (cell? child)
      (swap! kids #(vec (remove (partial = @child) %)))
      (swap! kids #(vec (remove (partial = child) %))))
    (when-not (= (count @kids) (dec before-count))
      (throw (js/Error. "Attempted to remove a node that is not a child of parent.")))
    child))

(defn replace-child! [elem new existing]
  (swap! (hoplon-kids elem) #(mapv (fn [el] (if (= el existing) new el)) %))
  existing)

(defn insert-before! [elem new existing]
  (cond
    (not existing)      (swap! (hoplon-kids elem) conj new)
    (not= new existing) (swap! (hoplon-kids elem)
                               #(vec (mapcat (fn [el] (if (= el existing) [new el] [el])) %))))
  new)


(defmulti do!
  (fn [elem key val]
    (if-let [n (namespace key)] (keyword n "*") key)) :default ::default)

(defmethod do! ::default
  [elem key val]
  (do! elem :attr {key val}))

(defmethod do! :css/*
  [elem key val]
  (set-styles! elem {key val}))

(defmethod do! :html/*
  [elem key val]
  (set-attributes! elem {key val}))

(defmethod do! :svg/*
  [elem key val]
  (set-attributes! elem {key val}))

(defmethod do! :attr
  [elem _ kvs]
  (set-attributes! elem kvs))

(defmethod do! :css
  [elem _ kvs]
  (set-styles! elem kvs))

(defn when-dom
  "Executes a function once an element has been attached to the DOM."
  [this f]
  (if-not (instance? js/Element this)
    (js/setTimeout f 0)
    (if-let [v (obj/get this "_hoplonWhenDom")]
      (.push v f)
      (do (obj/set this "_hoplonWhenDom" (array f))
          (js/setTimeout
           (fn doit []
             (if-not (.contains (.-documentElement js/document) this)
               (js/setTimeout doit  20)
               (do (doseq [f (obj/get this "_hoplonWhenDom")] (f))
                   (obj/set this "_hoplonWhenDom" nil))))
           0)))))

(defmulti on!
  (fn [elem key val]
    (if-let [n (namespace key)] (keyword n "*") key)) :default ::default)

(defmethod on! ::default
  [elem event callback]
  (when-dom elem #(.addEventListener elem (name event) callback)))

(defmethod on! :html/*
  [elem event callback]
  (when-dom elem #(.addEventListener elem (name event) callback)))

(defn handle-kids [el kids]
  (doseq [x (vflatten kids)]
    (when-let [x (->node x)]
      (append-child! el x)))
  el)

(defn handle-props [el props]
  (doseq [[k v] props]
    (cond (cell? v) (do-watch v #(do! el k %2))
          (fn? v)   (on! el k v)
          :else     (do! el k v))))


(defn init! []
  (d/def-elem-ctors (fn [el-str]
                      (fn [& args]
                        ((d/invoker (.createElement js/document el-str)
                           handle-props
                           handle-kids)
                         args)
                        #_((d/specify-invoker (.createElement js/document el-str)
                             handle-props
                             handle-kids)
                           args)))
    :rename {meta html-meta
             time html-time
             map  html-map}
    :exclude [html head body])

  (d/extend-invoke js/Element
    (fn [this & args]
      ((d/invoker this handle-props handle-kids)
       args))))

(init!)

