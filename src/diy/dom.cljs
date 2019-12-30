(ns diy.dom
  (:require [goog.dom :as gdom]
            [goog.dom.classlist :as class]
            [goog.array :as garr]
            [goog.object :as gobj]
            [goog.style :as gsty]
            [clojure.string :as str])
  (:require-macros [diy.dom :refer [def-elem-ctors defelem elem]]))

(defn $ [selector]
  (js/document.querySelector selector))

(defn append [node & children]
  (doto node (gdom/append (garr/flatten (clj->js children)))))

(defn text [x]
  (gdom/createTextNode x))

(defn frag [& kids]
  (doto (js/document.createDocumentFragment) (append kids)))

(defn clear [node]
  (doto node gdom/removeChildren))

(defn elem? [x] (gdom/isElement x))

(defn node? [x] (gdom/isNodeLike x))

(defn vflatten
  "Takes a sequential collection and returns a flattened vector of any nested
  sequential collections."
  ([x] (vflatten [] x))
  ([acc x] (if (sequential? x) (reduce vflatten acc x) (conj acc x))))

;;from hoplon.
(defn hoplon-parse-args
  "Parses a sequence of element arguments into attributes and children."
  [args]
  (loop [attr         (transient {})
         kids         (transient [])
         muts         (transient [])
         [arg & args] args]
    (if-not (or arg args)
      [(persistent! attr) (persistent! kids) (persistent! muts)]
      (cond
        (map? arg)     (recur (reduce-kv assoc! attr arg) kids muts args)
        (set? arg)     (recur (reduce #(assoc! %1 %2 true) attr arg) kids muts args)
        ;;todo: allow non-keyword attrs. see hoplon IAttr type
        (keyword? arg) (recur (assoc! attr arg (first args)) kids muts (rest args))

        (fn? arg)      (recur attr kids (conj! muts arg (first args)) (rest args))

        (seq? arg)     (recur attr (reduce conj! kids (vflatten arg)) muts args)
        (vector? arg)  (recur attr (reduce conj! kids (vflatten arg)) muts args)
        :else          (recur attr (conj! kids arg) muts args)))))

;;alternative, less fancy arg parsing. Allows an optional attribute
;;map as a first argument only
#_
(defn opt-attr-parse-args [args]
  (let [maybe-attr (first args)
        has-attr?  (map? maybe-attr)
        attr       (if has-attr? maybe-attr {})
        kids       (flatten (if has-attr? (rest args) args))]
    [attr kids]))

(defmulti handle-attr! (fn [el k v] k) :default ::default)

(defmethod handle-attr! :width [el k v]
  (gsty/setWidth el v))

(defmethod handle-attr! :height [el k v]
  (gsty/setHeight el v))

(defmethod handle-attr! :class [el k v]
  (if (sequential? v)
    (class/addAll el (clj->js v))
    (class/set el v)))

(defn add-attr! [el attr]
  (when (not-empty attr)
    (let [unhandled (reduce-kv (fn [m k v]
                                  (if-let [f (get-method handle-attr! k)]
                                    (do (f el k v) m)
                                    (doto m (gobj/set (name k) v))))
                               #js{} attr)]
      (when-not (gobj/isEmpty unhandled)
        (gdom/setProperties el unhandled)))))

(defn add-kids! [el kids]
  (doto el (gdom/append (clj->js kids))))

(defn invoke! [el & args]
  (let [[attr kids muts] (hoplon-parse-args args)]
    (add-attr! el attr)
    (add-kids! el kids)
    (doseq [[f args] (partition 2 muts)]
      (apply f el (if (sequential? args) args [args])))
    el))

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
     (invoke! this a b c d e f g h i j k l m n o p q r s t rest))))

(defn elem-fn
  "Returns a function which creates a dom node of type tagname."
  [tag-name]
  (fn [& args]
    (apply invoke! (gdom/createElement tag-name) args)))


(def-elem-ctors elem-fn :rename {"meta" "html-meta"
                                 "time" "html-time"
                                 "map"  "html-map"})
