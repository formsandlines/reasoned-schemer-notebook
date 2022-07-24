(ns reas.cpt7
  (:require
   [clojure.core.logic :as l :refer [run run* succeed fail s# u# defne
                                     conde fresh lcons llist]]
   [reas.utils :as rs :refer [defrel]]))


;;=============================================================
;; CHAPTER 7 - A BIT TOO MUCH

(defn bit-xoro [x y r]
  (conde
    [(l/== 0 x) (l/== 0 y) (l/== 0 r)]
    [(l/== 0 x) (l/== 1 y) (l/== 1 r)]
    [(l/== 1 x) (l/== 0 y) (l/== 1 r)]
    [(l/== 1 x) (l/== 1 y) (l/== 0 r)]))

(comment
  ;; alternative, more concise definitions using defne:

  #_:clj-kondo/ignore
  (defne bit-xoro [x y r]
    ([0 0 0])
    ([0 1 1])
    ([1 0 1])
    ([1 1 0]))

  #_:clj-kondo/ignore
  (defne bit-nando [x y r]
    ([0 0 1])
    ([0 1 1])
    ([1 0 1])
    ([1 1 0]))

  #_:clj-kondo/ignore
  (defne bit-xoro-from-nando [x y r]
    ([_ _ _] (fresh [s1 s2 t u]
               (bit-nando x x s1)
               (bit-nando y y s2)
               (bit-nando s1 y t)
               (bit-nando s2 x u)
               (bit-nando t u r))))
  ;; As a logical proposition (‘|’ := NAND):
  ;; ((a|a) | b) | ((b|b) | a)

  ;; Alternative, (maybe not logically) simpler definition:
  #_:clj-kondo/ignore
  (defne bit-xoro-from-nando [x y r]
    ([_ _ _] (fresh [s t u]
               (bit-nando x y s)
               (bit-nando s y u)
               (bit-nando x s t)
               (bit-nando t u r))))
  ;; ((a|b) | b) | ((a|b) | a)

  )


(comment
  ;;-------------------------------------------------------------
  ;; bit-xoro

  (run* [x y]
    (fresh [r]
      (bit-xoro x y r)))
  ;=> ([0 0] [0 1] [1 0] [1 1])

  (run* [r]
    (fresh [x y]
      (bit-xoro x y r)))
  ;=> (0 1 1 0)

  (run* [x y]
    (bit-xoro x y 0))
  ;=> ([0 0] [1 1])

  (run* [x y r]
    (bit-xoro-from-nando x y r))
  ;=> ([0 0 0] [0 1 1] [1 0 1] [1 1 0])

  )
