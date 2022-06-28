(ns reas.cpt2
  (:require
   [clojure.core.logic :as l :refer [run* succeed fail s# u#
                                     conde fresh and* or* lcons]]
   [reas.utils :as rs :refer [defrel]]))


;;=============================================================
;; CHAPTER 1 - Playthings

(comment

  'firsto ;; is core.logic’s car^o

  (run* [q]
    (l/firsto '(a c o r n) q)) ;=> (a)

  (run* [q]
    (l/firsto '(a c o r n) 'a)) ;=> (_0)

  (run* [r]
    (fresh [x y]
      (l/firsto `(~r ~y) x)
      (l/== 'pear x))) ;=> (pear)

  'lcons ;; is core.logic’s way to create sequences with
  ;; improper tails (like pairs in Scheme), which can be 
  ;; used to create pairs of lvars
  'llist ;; is a macro to create nested lcons (lists)

  ;; l/firsto can be defined/implemented like this:
  #_:clj-kondo/ignore
  (defrel caro [p a]
    (fresh [d l]
      (l/== (lcons a d) p)))

  (run* [r]
    (fresh [x y]
      (caro '(grape raisin pear) x)
      (caro '((a) (b) (c)) y)
      (l/== (lcons x y) r))) ;=> ((grape a))



  )
