;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns diy.hoplon.tpl
  (:require [diy.hoplon :refer [do-watch]]
            [javelin.core :refer [cell cell=]])
  (:require-macros [diy.hoplon.tpl]))

(defn loop-tpl*
  "Given a cell items containing a seqable collection, constructs a cell that
  works like a fill vector. The template `tpl` is a function of one argument: the
  formula cell containing the ith item in items. The tpl function is called
  once (and only once) for each index in items. When the items collection
  shrinks the DOM element created by the template is not destroyed--it is only
  removed from the DOM and cached. When the items collection grows again those
  cached elements will be reinserted into the DOM at their original index."
  [items tpl]
  (let [els         (cell [])
        itemsv      (cell= (vec items))
        items-count (cell= (count items))]
    (do-watch items-count
              (fn [_ n]
                (when (< (count @els) n)
                  (doseq [i (range (count @els) n)]
                    (swap! els assoc i (tpl (cell= (get itemsv i nil))))))))
    (cell= (subvec els 0 (min items-count (count els))))))
