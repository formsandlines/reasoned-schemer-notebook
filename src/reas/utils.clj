(ns reas.utils
  (:require
   [clojure.core.logic :as l :refer [and*]]))

;; replaced by and*:
; (defmacro lconj [& lvars]
;   `(conde [~@lvars]))

;; replaced by or*:
; (defmacro ldisj [& lvars]
;   `(conde ~@(map vector lvars)))

;; core.logic doesn’t have this macro (anymore?), so I recreated it:
(defmacro defrel [name params & goals]
  `(defn ~name ~params
     (fn [s#]
       (fn []
         ((and* ~(vec goals)) s#)))))

;; see chapter 2 and
;; https://github.com/clojure/core.logic/wiki/Differences-from-The-Reasoned-Schemer
(defn pair?
  "True if x is a pair, false otherwise."
  [x]
  (case (or (l/lcons? x) (and (coll? x) (seq x)))
    (nil false) false
    true))


