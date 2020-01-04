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
  "Parses a sequence of element arguments and returns a vector of
  properties, children and mutations"
  [args]
  (loop [props        (transient {})
         kids         (transient [])
         muts         (transient [])
         [arg & args] args]
    (if-not (or arg args)
      [(persistent! props) (persistent! kids) (persistent! muts)]
      (cond
        (map? arg)     (recur (reduce-kv assoc! props arg) kids muts args)
        (set? arg)     (recur (reduce #(assoc! %1 %2 true) props arg) kids muts args)
        ;;todo: allow non-keyword propss. see hoplon IProps type
        (keyword? arg) (recur (assoc! props arg (first args)) kids muts (rest args))

        (fn? arg) (recur props kids (conj! muts arg (first args)) (rest args))

        (seq? arg)    (recur props (reduce conj! kids (vflatten arg)) muts args)
        (vector? arg) (recur props (reduce conj! kids (vflatten arg)) muts args)
        :else         (recur props (conj! kids arg) muts args)))))

;;alternative, less fancy arg parsing. Allows an optional propibute
;;map as a first argument only
#_
(defn opt-prop-parse-args [args]
  (let [maybe-prop (first args)
        has-prop?  (map? maybe-prop)
        prop       (if has-prop? maybe-prop {})
        kids       (flatten (if has-prop? (rest args) args))]
    [prop kids]))

(defmulti handle-prop! (fn [el k v] k) :default ::default)

(defmethod handle-prop! :width [el k v]
  (gsty/setWidth el v))

(defmethod handle-prop! :height [el k v]
  (gsty/setHeight el v))

(defmethod handle-prop! :class [el k v]
  (if (sequential? v)
    (class/addAll el (clj->js v))
    (class/set el v)))

(defn add-props! [el props]
  (when (not-empty props)
    (let [unhandled (reduce-kv (fn [m k v]
                                  (if-let [f (get-method handle-prop! k)]
                                    (do (f el k v) m)
                                    (doto m (gobj/set (name k) v))))
                               #js{} props)]
      (when-not (gobj/isEmpty unhandled)
        (gdom/setProperties el unhandled)))))

(defn add-kids! [el kids]
  (append el kids))



(defn invoke! [el & args]
  (let [[props kids muts] (hoplon-parse-args args)]
    (add-props! el props)
    (add-kids! el kids)
    (doseq [[f arg] (partition 2 muts)]
      (f el arg))
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
