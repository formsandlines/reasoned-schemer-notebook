(ns reas.cpt6
  (:require
   [clojure.core.logic :as l :refer [run run* succeed fail s# u# defne
                                     conde fresh lcons llist]]
   [reas.utils :as rs :refer [defrel]]))


;;=============================================================
;; CHAPTER 6 - THE FUN NEVER ENDS

(defn alwayso []
  (conde
    [s#]
    [(alwayso)]))

;; nevero does not work with defn (?), so I used defrel:
#_:clj-kondo/ignore
(defrel nevero []
  (nevero))

#_:clj-kondo/ignore
(defrel very-recursiveo []
  (conde
    [(nevero)]
    [(very-recursiveo)]
    [(alwayso)]
    [(very-recursiveo)]
    [(nevero)]))

(comment
  ;; alternative, more concise definitions using defne:

  #_:clj-kondo/ignore
  (defne alwayso []
    ([] s#)
    ([] (alwayso)))

  ;; this also works like the defrel version:
  #_:clj-kondo/ignore
  (defne nevero []
    ([] (nevero)))
  
  )

(comment
  ;;-------------------------------------------------------------
  ;; alwayso

  (run 1 [q]
    (alwayso)) ;=> (_0)

  ;; the first conde line succeeds and run 1 is done:
  (run 1 [q]
    (conde
      [s#]
      [(alwayso)])) ;=> (_0)

  ;; alwayso succeeds any number of times because of the recursion
  (run 8 [q] s#) ;=> (_0)
  (run 8 [q] (alwayso)) ;=> (_0 _0 _0 _0 _0 _0 _0 _0)

  ;; with run*, infinite goals succeed and the list never finishes
  ; (run* [q]
  ;   (alwayso))
  ; no value

  ;; q is associated with onion and s# suceeds five times
  (run 5 [q]
    (l/== 'onion q)
    (alwayso)) ;=> (onion onion onion onion onion)

  ;; alwayso succeeds, followed by u#, which leads to failure,
  ;; which causes alwayso to recursively try again, without end…:
  ; (run 1 [q]
  ;   (alwayso)
  ;   u#)
  ; no value

  (run 1 [q]
    (l/== 'garlic q)
    s#
    (l/== 'onion q)) ;=> ()

  ; (run 1 [q]
  ;   (l/== 'garlic q)
  ;   (alwayso)
  ;   (l/== 'onion q))
  ; no value

  (run 1 [q]
    (conde
      [(l/== 'garlic q) (alwayso)]
      [(l/== 'onion q)])
    (l/== 'onion q)) ;=> (onion)

  ;; this will try again with first conde clause again and again
  ;; because the second clause already succeeded and alwayso is recursive
  ; (run 2 [q]
  ;   (conde
  ;     [(l/== 'garlic q) (alwayso)]
  ;     [(l/== 'onion q)])
  ;   (l/== 'onion q))
  ; no value

  ;; using alwayso in the succeeding clause, we can prevent this
  ;; since it can succeed multiple times now
  (run 5 [q]
    (conde
      [(l/== 'garlic q) (alwayso)]
      [(l/== 'onion q) (alwayso)])
    (l/== 'onion q)) ;=> (onion onion onion onion onion)

  )
(comment
  ;;-------------------------------------------------------------
  ;; nevero

  ;; nevero will neither succeed nor fail
  ; (run 1 [q]
  ;   (nevero))
  ; no value

  ;; u# here fails before nevero is attempted, so no infinite recursion:
  (run 1 [q]
    u#
    (nevero)) ;=> ()

  (run 1 [q]
    (conde
      [s#]
      [(nevero)])) ;=> (_0)

  (run 1 [q]
    (conde
      [(nevero)]
      [s#])) ;=> (_0)

  ;; like with the alwayso example above, this gets stuck,
  ;; because the second value can never be determined by nevero
  ; (run 2 [q]
  ;   (conde
  ;     [s#]
  ;     [(nevero)]))
  ; no value

  ; (run 1 [q]
  ;   (conde
  ;     [s#]
  ;     [(nevero)])
  ;   u#)
  ; no value

  (run 5 [q]
    (conde
      [(nevero)]
      [(alwayso)]
      [(nevero)])) ;=> (_0 _0 _0 _0 _0)

  (run 6 [q]
    (conde
      [(l/== 'spicy q) (nevero)]
      [(l/== 'hot q)   (nevero)]
      [(l/== 'apple q) (alwayso)]
      [(l/== 'cider q) (alwayso)]))
  ;=> (apple cider apple cider apple cider)

  ; (run 1000000 [q]
  ;   (very-recursiveo))
  ;=> (_0 _0 _0 … 1 mio. times) 

  )
