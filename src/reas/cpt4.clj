(ns reas.cpt4
  (:require
   [clojure.core.logic :as l :refer [run run* succeed fail s# u# defne
                                     conde fresh lcons llist]]
   [reas.utils :as rs :refer [defrel]]))


;;=============================================================
;; CHAPTER 4 - DOUBLE YOUR FUN

(defn appendo [l t out]
  (conde
    [(l/emptyo l) (l/== t out)]
    [(fresh [a d res]
       (l/conso a d l)
       (appendo d t res)
       (l/conso a res out))]))

;; doesn’t work in (run 7) example below, even with defrel:
(defn appendo-safe [l t out]
  (conde
    [(l/emptyo l) (l/== t out)]
    [(fresh [a d res]
       (l/conso a d l)
       (l/conso a res out)
       (appendo d t res))]))

(comment
  ;; alternative, more concise definitions using defne:

  #_:clj-kondo/ignore
  (defne appendo [l t out]
    ([() _ _]      (l/== t out))
    ([[a . d] _ _] (fresh [res]
                     (appendo d t res)
                     (l/conso a res out))))
  )

(comment
  ;;-------------------------------------------------------------
  ;; append as a relation

  (defn append [l t]
    (cond
      (empty? l) t
      :else (lcons
              (first l)
              (append (rest l) t))))

  (append '(a b c) '(d e)) ;=> (a b c d e)
  ;; the implementation is almost identical in Clojures concat:
  (concat '(a b c) '(d e)) ;=> (a b c d e)

  (append '(a b c) '()) ;=> (a b c)
  (concat '(a b c) '()) ;=> (a b c)

  (append '() '(d e)) ;=> (d e)
  (concat '() '(d e)) ;=> (d e)

  (append 'a '(d e)) ; error, a is no proper list!

  ;; but because of lcons (and no requirements about t),
  ;; we can create improper lists (contrary to concat):
  (append '(d e) 'a) ;=> (d e . a)

  'appendo ; re-implemented

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; THE TRANSLATION (FINAL)
  ;; To translate a function into a relation, first replace
  ;; `define` with `defrel`. Then unnest each expression in each
  ;; `cond` line, and replace each `cond` with `cond^e`.
  ;; To unnest a `true`/`:else`, replace it with s#. To unnest a
  ;; `false`, replace it with u#.
  ;; If the value of at least one `cond` line can be a non-Boolean,
  ;; add an argument, say `out`, to `defrel` to hold what would
  ;; have been the function’s value. When unnesting a line whose
  ;; value is not a Boolean, ensure that either some value is
  ;; associated with `out` or that `out` is the last argument
  ;; to a recursion.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (run 6 [x]
    (fresh [y z]
      (appendo x y z)))
  ;=> (() (_0) (_0 _1) (_0 _1 _2) (_0 _1 _2 _3) (_0 _1 _2 _3 _4))

  (run 6 [y]
    (fresh [x z]
      (appendo x y z)))
  ;=> (_0 _0 _0 _0 _0 _0)

  (run 6 [z]
    (fresh [x y]
      (appendo x y z)))
  ;=> (_0
  ;    (_0 . _1)
  ;    (_0 _1 . _2)
  ;    (_0 _1 _2 . _3)
  ;    (_0 _1 _2 _3 . _4)
  ;    (_0 _1 _2 _3 _4 . _5))

  (run 6 [x y z]
    (l/appendo x y z))
  ;=> ([() _0 _0]
  ;    [(_0) _1 (_0 . _1)]
  ;    [(_0 _1) _2 (_0 _1 . _2)]
  ;    [(_0 _1 _2) _3 (_0 _1 _2 . _3)]
  ;    [(_0 _1 _2 _3) _4 (_0 _1 _2 _3 . _4)]
  ;    [(_0 _1 _2 _3 _4) _5 (_0 _1 _2 _3 _4 . _5)])

  (run* [x]
    (l/appendo
      '(cake)
      '(tastes yummy)
      x)) ;=> ((cake tastes yummy))

  (run* [x]
    (fresh [y]
      (l/appendo
        `(cake & ice ~y)
        `(tastes yummy)
        x))) ;=> ((cake & ice _0 tastes yummy))

  ;; y doesn’t need to be a list in the t-position of appendo
  (run* [x]
    (fresh [y]
      (l/appendo
        '(cake & ice cream)
        y
        x))) ;=> ((cake & ice cream . _0))

  ;; here, y succeeds the first goal in appendo and thus gets
  ;; associated with the empty list
  (run 1 [x]
    (fresh [y]
      (l/appendo
        (llist 'cake '& 'ice y)
        '(d t)
        x))) ;=> ((cake & ice d t))

  (run 5 [x]
    (fresh [y]
      (l/appendo
        (llist 'cake '& 'ice y)
        '(d t)
        x)))
  ;=> ((cake & ice d t)
  ;    (cake & ice _0 d t)
  ;    (cake & ice _0 _1 d t)
  ;    (cake & ice _0 _1 _2 d t)
  ;    (cake & ice _0 _1 _2 _3 d t))

  (run 5 [y]
    (fresh [x]
      (l/appendo
        (llist 'cake '& 'ice y)
        '(d t)
        x)))
  ;=> (() (_0) (_0 _1) (_0 _1 _2) (_0 _1 _2 _3))

  (run 5 [x]
    (fresh [y]
      (l/appendo
        (llist 'cake '& 'ice y)
        (llist 'd 't y)
        x)))
  ;=> ((cake & ice d t)
  ;    (cake & ice _0 d t _0)
  ;    (cake & ice _0 _1 d t _0 _1)
  ;    (cake & ice _0 _1 _2 d t _0 _1 _2)
  ;    (cake & ice _0 _1 _2 _3 d t _0 _1 _2))

  (run* [x]
    (fresh [z]
      (l/appendo
        '(cake & ice cream)
        (llist 'd 't z)
        x)))
  ;=> ((cake & ice cream d t . _0))

  (run 6 [x]
    (fresh [y]
      (l/appendo x y '(cake & ice d t))))
  ;=> (() (cake) (cake &) (cake & ice) (cake & ice d) (cake & ice d t))

  (run 6 [y]
    (fresh [x]
      (l/appendo x y '(cake & ice d t))))
  ;=> ((cake & ice d t)
  ;    (& ice d t)
  ;    (ice d t)
  ;    (d t)
  ;    (t)
  ;    ())

  (run 6 [x y]
    (l/appendo x y '(cake & ice d t)))
  ;=> ([() (cake & ice d t)]
  ;    [(cake) (& ice d t)]
  ;    [(cake &) (ice d t)]
  ;    [(cake & ice) (d t)]
  ;    [(cake & ice d) (t)]
  ;    [(cake & ice d t) ()])

  (run 7 [x y]
    (appendo x y '(cake & ice d t)))
  ;=> no value (endless recursion, looking for the 7th value)

  ;; ? doesn’t work
  (run 7 [x y]
    (appendo-safe x y '(cake & ice d t)))
  ;=> same as with (run 6), since there is no possible 7th value

  (run 7 [x y]
    (l/appendo x y '(cake & ice d t)))
  ;=> same as with (run 6), since there is no possible 7th value

  

  )


