(ns reas.cpt4
  (:require
   [clojure.core.logic :as l :refer [run run* succeed fail s# u# defne
                                     conde fresh lcons llist]]
   [reas.utils :as rs :refer [defrel pair?]]))


;;=============================================================
;; CHAPTER 4 - DOUBLE YOUR FUN

(defn appendo [l t out]
  (conde
    [(l/emptyo l) (l/== t out)]
    [(fresh [a d res]
       (l/conso a d l)
       (appendo d t res)
       (l/conso a res out))]))

(defn appendo-safe [l t out]
  (conde
    [(l/emptyo l) (l/== t out)]
    [(fresh [a d res]
       (l/conso a d l)
       (l/conso a res out)
       (appendo-safe d t res))]))

(defn swappendo [l t out]
  (conde
    [(fresh [a d res]
       (l/conso a d l)
       (l/conso a res out)
       (swappendo d t res))]
    [(l/emptyo l) (l/== t out)]))

(defn unwrapo [x out]
  (conde
    [(fresh [a]
       (l/firsto x a) (unwrapo a out))]
    [(l/== x out)]))

(comment
  ;; alternative, more concise definitions using defne:

  #_:clj-kondo/ignore
  (defne appendo-safe [l t out]
    ([() _ _]      (l/== t out))
    ([[a . d] _ _] (fresh [res]
                     (l/conso a res out)
                     (appendo-safe d t res))))

  #_:clj-kondo/ignore
  (defne unwrapo [x out]
    ([[a . _] _] (unwrapo a out))
    ([_ x]))
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
        (list 'cake '& 'ice y)
        '(tastes yummy)
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

  ;; *1:
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

  ;; also stuck in endless recursion:
  (run* [x y]
    (appendo x y '(cake & ice d t)))

  ;; doesn’t get stuck, because the recursive line/goal comes last:
  (run* [x y]
    (appendo-safe x y '(cake & ice d t)))
  ;=> same as with (run 6), since there is no possible 7th value

  (run 7 [x y]
    (appendo-safe x y '(cake & ice d t)))
  ;=> see above

  (comment
    ;; the library function works the same way:
    (run 7 [x y]
      (l/appendo x y '(cake & ice d t)))
    )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; THE FIRST COMMANDMENT
  ;; Within each sequence of goals, move non-recursive goals
  ;; before recursive goals.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  )
(comment
  ;;-------------------------------------------------------------
  ;; order of results cond^e

  (comment
    ;; conso fails if the list (result) is empty:
    (run* [a d]
      (l/conso a d '())) ;=> ()

    )

  (run* [x y]
    (swappendo x y '(cake & ice d t)))
  ;=> same as *1 (appendo)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; THE LAW OF SWAPPING COND^e LINES
  ;; Swapping two cond^e lines does not affect the values
  ;; contributed by cond^e.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defn unwrap [x]
    (cond
      (pair? x) (unwrap (first x))
      :else x))

  (unwrap '((((pizza))))) ;=> pizza

  (unwrap '((((pizza pie) with)) garlic)) ;=> pizza

  (run* [x]
    (unwrapo '(((pizza))) x))
  ;=> ((((pizza))) ((pizza)) (pizza) pizza)

  (run 1 [x]
    (unwrapo x 'pizza)) ;=> (pizza)

  (run 1 [x]
    (unwrapo `((~x)) 'pizza)) ;=> (pizza)

  (run 5 [x]
    (unwrapo x 'pizza))
  ;=> (pizza
  ;    (pizza . _0)
  ;    ((pizza . _0) . _1)
  ;    (((pizza . _0) . _1) . _2)
  ;    ((((pizza . _0) . _1) . _2) . _3))

  (comment
    ;; which succeeding gaal will appear first in the output?
    (defn case-test [x]
      (conde
        [(l/== 'foo x)]
        [(l/== 'bar x)]))
    ;; -> same order as definition:
    (run* [x]
      (case-test x)) ;=> (foo bar)

    ;; how does recursions affect the order of results?
    (defn rec-test [x]
      (conde
        [(l/== 'foo x) (howo x)]
        [(l/== 'bar x) (howo x)]
        [s#]))
    ;; -> it seems like the non-recursive goal is evaluated first:
    (run 3 [x]
      (rec-test x)) ;=> (_0 foo bar)
    )

  ;; Note: the actual Clojure output has a weird lazy-seq reference
  ;;       instead of ((pizza)) for all results after the first
  (run 5 [x]
    (unwrapo x '((pizza))))
  ;=> (((pizza))
  ;    (((pizza)) . _0)
  ;    ((((pizza)) . _0) . _1)
  ;    (((((pizza)) . _0) . _1) . _2)
  ;    ((((((pizza)) . _0) . _1) . _2) . _3))

  (run 5 [x]
    (unwrapo `((~x)) 'pizza))
  ;=> (pizza
  ;    (pizza . _0)
  ;    ((pizza . _0) . _1)
  ;    (((pizza . _0) . _1) . _2)
  ;    ((((pizza . _0) . _1) . _2) . _3))

  )

