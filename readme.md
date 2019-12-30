# diy

## Do It Yourself DOM

`diy` is a minimum viable clojurescript dom library inspired by [hoplon](https://github.com/hoplon/hoplon). Unlike hoplon, `diy` doesn't directly help you with reactivity but only seeks to provide helpers for element creation and mutation.

## Deps

```
diy {:git/url "https://github.com/jjttjj/diy.git"
     :sha "<SHA>"}
```       

## Usage 

```
(:require [diy.dom :as d :refer [$ defelem div]])
```
All html element types are defined as functions in the diy.dom namespace.

Create a dom element:
```
(d/div) ;;=> #object[HTMLDivElement] ;; a regular old div
```

All key/value pairs and maps passed to an element are added as properties to the element. By default all properties are added to the element via the [`goog.dom/setProperties`](https://github.com/google/closure-library/blob/f4368f69e164b828e82f1a8b194adef967dd5d0f/closure/goog/dom/dom.js#L427-L473) function from Google Closure. 

```
(.-src (img :src "https://clojure.org/images/clojure-logo-120b.png"))
;;=> "https://clojure.org/images/clojure-logo-120b.png"

(.-style.cssText (d/div :style "background:blue" "hi"))
;;"background: blue;"
;;note: google closure's setProperties handles the setting of cssText for us
```

Any elements passed to an element as are appended to its children. Any strings are added as text nodes:

```
(def el1 (div "a" (span "b")))

(aget (.-childNodes el1) 0)
;;=> #object[Text [object Text]]


(aget (.-childNodes el1) 1)
;;=>#object[HTMLSpanElement [object HTMLSpanElement]]
```

Elements themselves are functions that can also be passed attributes and children which are then added to the element:

```
(aget (.-childNodes el1) 2)
;;=> nil

(el1 (p "hi"))
(aget (.-childNodes el1) 2)
;;=> #object[HTMLParagraphElement [object HTMLParagraphElement]]
;;
```

## Custom property handling

If you want to define custom handling for a property, you can extend the `diy.dom/handle-prop!` multimethod for a given key. `diy` currently defines a custom handler for the `:class` property which lets you add a sequence of classes instead of just a class string:

```
(:require 
  ...
  [goog.dom.classlist :as class]
  ...
  )
            
...
;;we pretend this isn't already implemented and defmethod for d/handle-prop
(defmethod d/handle-prop! :class [el k v]
  (if (sequential? v)
    (class/addAll el (clj->js v))
    (class/set el v)))
```

```
(.-className (d/div :class ["a" "b" "c"]))
;;"a b c"
```

## Mutators 

In the same way you can pass a sequential key-value pair to be interpreted as a property, you can pass a function followed by either a single argument or collection of arguments. After all attributes and children are added to an element, these functions are applied with the element as the first argument, followed by the single argument or vector of arguments. 

```
(:require 
  ...
  [goog.style :as gsty]
  ...
  )

(.-style.height (d/div gsty/setHeight 300))
;;=> "300px"
```


# License

Copyright © Justin Tirrell

Portions of code taken from [hoplon](https://github.com/hoplon/hoplon). Copyright (c) Alan Dipert and Micha Niskin.

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version. https://opensource.org/licenses/EPL-1.0
