(ns reas.cpt2
  (:require
   [clojure.core.logic :as l :refer [run* succeed fail s# u# defne
                                     conde fresh and* or* lcons llist]]
   [reas.utils :as rs :refer [defrel]]))


;;=============================================================
;; CHAPTER 2 - TEACHING OLD TOYS NEW TRICKS

(declare pairo)
(declare singletono)

;; Note: in core.logic, defrel is actually not needed!

;; l/resto can be defined/implemented just like caro:
(defn cdro [p d]
  (fresh [a]
    (l/== (lcons a d) p)))

;; l/firsto can be defined/implemented like this:
(defn caro [p a]
  (fresh [d]
    (l/== (lcons a d) p)))

;; l/conso can be defined/implemented just like caro and cdro:
(defn conso [a d p]
  (l/== (lcons a d) p))

;; l/emptyo can be defined/implemented via unification:
(defn nullo [x]
  (l/== '() x))

(defn pairo [p]
  (fresh [a d]
    (l/conso a d p)))

(defn singletono [l]
  (fresh [d]
    (l/resto l d)
    (l/emptyo d)))

(comment
  ;; alternative, more concise definitions using defne:

  #_:clj-kondo/ignore
  (defne cdro [p d]
    ([[_ . d] _]))

  #_:clj-kondo/ignore
  (defne caro [p a]
    ([[a . _] _]))

  #_:clj-kondo/ignore
  (defne conso [a d p]
    ([_ _ [a . d]]))

  #_:clj-kondo/ignore
  (defne nullo [x]
    (['()]))

  #_:clj-kondo/ignore
  (defne pairo [p]
    ([[a . d]]))

  #_:clj-kondo/ignore
  (defne singletono [l]
    ([[_ . '()]]))
  )

(comment
  ;;-------------------------------------------------------------
  ;; List relations: car^o and cdr^o

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
  (lcons 'a 'b) ;=> (a . b)
  (lcons 'a (lcons 'b 'c)) ;=> (a b . c)

  'llist ;; is a macro to create nested lcons (lists)
  (llist 'a 'b 'c) ;=> (a b . c)

  'caro ;; re-implemented

  (run* [r]
    (fresh [x y]
      (caro '(grape raisin pear) x)
      (caro '((a) (b) (c)) y)
      (l/== (lcons x y) r))) ;=> ((grape a))

  'resto ;; is core.logic’s cdr^o

  ;; unnesting -> transforming nested cons into car/cdr relations:
  ;; (car (cdr (cdr l))) into (cdro l v), (cdro v w), (caro w r)
  (run* [r]
    (fresh [v]
      (l/resto '(a c o r n) v)
      (fresh [w]
        (l/resto v w)
        (l/firsto w r)))) ;=> (o)

  'cdro ;; re-implemented

  (run* [r]
    (fresh [x y]
      (cdro '(grape raisin pear) x)
      (caro '((a) (b) (c)) y)
      (l/== (lcons x y) r))) ;=> (((raisin pear) a))

  (run* [q]
    (cdro '(a c o r n) '(c o r n))) ;=> (_0)

  (run* [x]
    (cdro `(c o r n) `(~x r n))) ;=> (o)

  (run* [l]
    (fresh [x]
      (cdro l '(c o r n))
      (caro l x)
      (l/== 'a x))) ;=> ((a c o r n))

)
(comment
  ;;-------------------------------------------------------------
  ;; List relations: cons^o

  'conso ;; cons as a relation

  (run* [l]
    (l/conso '(a b c) '(d e) l)) ;=> (((a b c) d e))

  (run* [x]
    (l/conso x '(a b c) '(d a b c))) ;=> (d)

  (run* [r]
    (fresh [x y z]
      (l/== `(e a d ~x) r)
      (l/conso y `(a ~z c) r))) ;=> ((e a d c))

  (run* [x]
    (l/conso x `(a ~x c) `(d a ~x c))) ;=> (d)

  (run* [l]
    (fresh [x]
      (l/== `(d a ~x c) l)
      (l/conso x `(a ~x c) l))) ;=> ((d a d c))

  (run* [l]
    (fresh [x]
      (l/conso x `(a ~x c) l)
      (l/== `(d a ~x c) l))) ;=> ((d a d c))

  'conso ;; re-implemented

  (run* [l]
    (fresh [d t x y w]
      (conso w '(n u s) t)
      (cdro l t)
      (caro l x)
      (l/== 'b x)
      (cdro l d)
      (caro d y)
      (l/== 'o y))) ;=> ((b o n u s))

  )
(comment
  ;;-------------------------------------------------------------
  ;; Empty list: null^o

  ;; Schemes `null?` would be more like Clojures `empty?`:
  (nil? '()) ;=> false
  (empty? '()) ;=> true

  'emptyo ;; would therefore be core.logic’s null^o

  (run* [q]
    (l/emptyo '(grape raisin pear))) ;=> ()

  (run* [q]
    (l/emptyo '())) ;=> (_0)
  ;; but:
  (run* [q]
    (l/nilo '())) ;=> ()

  ;; if this goal is to succeed, x has to be associated with '():
  (run* [x]
    (l/emptyo x)) ;=> (())

  'nullo ;; re-implemented

  (run* [x]
    (nullo x)) ;=> (())

  )
(comment
  ;;-------------------------------------------------------------
  ;; Pairs

  (run* [r]
    (fresh [x y]
      (l/== (lcons x (lcons y 'salad)) r))) ;=> ((_0 _1 . salad))

  'pairo ;; implemened; there seems to be nothing similar in core.logic

  ;; to satisfy pairo, we need to provide a cons-cell/pair
  (run* [q]
    (pairo (lcons q q))) ;=> (_0)

  (run* [q]
    (pairo '())) ;=> ()

  (run* [q]
    (pairo 'pair)) ;=> ()

  (run* [x]
    (pairo x)) ;=> ((_0 . _1))

  (run* [r]
    (pairo (lcons r '()))) ;=> (_0)

  )
(comment
  ;;-------------------------------------------------------------
  ;; Singleton lists

  ;; similar to the book:
  (defn singleton?-1 [l]
    (let [pair? #(and (coll? %) (== 2 (count %)))]
      (cond
        (pair? l) (empty? (rest l))
        :else false)))

  ;; more concise:
  (defn singleton? [l]
    (and (coll? l) (== 1 (count l))))

  (singleton? '((a) (a b) c)) ;=> false
  (singleton? '()) ;=> false
  (singleton? (cons 'pea '())) ;=> true
  (singleton? '(sauerkraut)) ;=> true

  ;; translation of singletono-1 into a relation:
  #_:clj-kondo/ignore
  (defrel singletono-1 [l]
    (conde
      [(pairo l)
       (fresh [d] ;; unnesting of (empty? (rest l))
         (l/resto l d)
         (l/emptyo d))]
      [s# u#]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; THE TRANSLATION (INITIAL)
  ;; To translate a function into a relation, first replace
  ;; `define` with `defrel`. Then unnest each expression in each
  ;; `cond` line, and replace each `cond` with `cond^e`.
  ;; To unnest a `true`/`:else`, replace it with s#. To unnest a
  ;; `false`, replace it with u#.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; simplified:
  'singletono ;; implemented

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; THE LAW OF u#
  ;; Any `cond^e` line that has u# as a top-level goal
  ;; cannot contribute values.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; alternative version (?)
  #_:clj-kondo/ignore
  (defrel singletono-2 [l]
    (fresh [a]
      (l/conso a '() l)))

  (run* [q]
    (singletono '())) ;=> ()

  (run* [q]
    (singletono '(a))) ;=> (_0)

  (run* [q]
    (singletono q)) ;=> ((_0))

  (run* [q]
    (singletono (lcons q '()))) ;=> (_0)

  ;; x is associated with '() via l/conso in singletono
  (run* [q]
    (fresh [x]
      (singletono (lcons q x)))) ;=> (_0)
  ;; but here, the second goal contradicts this unification:
  (run* [q]
    (fresh [x]
      (singletono (lcons q x))
      (l/== x 'a))) ;=> ()

  ;; in this example, q is associated with '(), so q == '() == a
  (run* [q]
    (singletono (lcons q q))) ;=> (())
  ;; because:
  (lcons '() '()) ;=> (())

  )



