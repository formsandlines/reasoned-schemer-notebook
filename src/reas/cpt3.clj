(ns reas.cpt3
  (:require
   [clojure.core.logic :as l :refer [run run* succeed fail s# u#
                                     conde fresh lcons llist]]
   [reas.utils :as rs :refer [defrel]]
   [reas.cpt2 :refer [pairo]]))


;;=============================================================
;; CHAPTER 3 - SEEING OLD FRIENDS IN NEW WAYS

#_:clj-kondo/ignore
(defrel listo [l]
  (conde
    [(l/emptyo l)]
    [(fresh [d]
       (l/resto l d)
       (listo d))]))

#_:clj-kondo/ignore
(defrel lolo [l]
  (conde
    [(l/emptyo l)]
    [(fresh [a d]
       (l/conso a d l)
       (listo a)
       (lolo d))]))

;; redefined from cpt2
#_:clj-kondo/ignore
(defrel singletono [l]
  (fresh [a]
    (l/== `(~a) l)))

;; list of singletons
#_:clj-kondo/ignore
(defrel loso [l]
  (conde
    [(l/emptyo l)]
    [(fresh [a d]
       (l/conso a d l)
       (singletono a)
       (loso d))]))

#_:clj-kondo/ignore
(defrel membero [x l]
  (conde
    [(l/firsto l x)]
    [(fresh [d]
       (l/resto l d)
       (membero x d))]))

#_:clj-kondo/ignore
(defrel proper-membero [x l]
  (conde
    [(l/firsto l x)
     (fresh [d]
       (l/resto l d)
       (listo d))]
    [(fresh [d]
       (l/resto l d)
       (proper-membero x d))]))

(comment
  ;;-------------------------------------------------------------
  ;; List relations and (run n …)

  (llist 'a 'b 'c 'd)

  'listo ;; implemened

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; THE LAW OF s#
  ;; Any top-level s# can be removed from a `cond^e` line.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; the exact value of x is not necessary for listo to succeed
  (run* [x]
    (listo `(a b ~x d))) ;=> (_0)

  ;; x can be associated with an unbounded number of possible list-values
  ;; for the goal to succeed -> the expression has no value
  (run* [x]
    (listo (llist 'a 'b 'c x))) ;=> no value (!! infinite recursion !!)

  'run ;; like run*, but restricts the output to n possible values

  ;; when listo reaches the end of ('a 'b 'c x), (l/emptyo x) succeeds and
  ;; associates x with the empty list
  (run 1 [x]
    (listo (llist 'a 'b 'c x))) ;=> (())

  (run 5 [x]
    (listo (llist 'a 'b 'c x)))
  ;=> (() (_0) (_0 _1) (_0 _1 _2) (_0 _1 _2 _3))

  ;; if n is greater than the number of goals that succeed,
  ;; the output size will still be equal to the numeber of goals
  (run 7 [x y]
    (conde
      [(l/== 'split x) (l/== 'pea y)]
      [(l/== 'red x) (l/== 'bean y)]
      [(l/== 'green x) (l/== 'lentil y)]))
  ;=> ([split pea] [red bean] [green lentil])  (equal to cpt1, last expr.)

)
(comment
  ;;-------------------------------------------------------------
  ;; List of list

  (defn lol?
    "True if l is a list of lists"
    [l]
    (cond
      (empty? l) true
      (list? (first l)) (lol? (rest l))
      :else false))

  'lolo ; implemened

  (lol? '(a b c)) ;=> false
  (lol? '((a) b c)) ;=> false
  (lol? '((a) (b) (c))) ;=> true

  (run* [q]
    (fresh [x y]
      (lolo `((a b) (~x c) (d ~y))))) ;=> (_0)

  (run 1 [l]
    (lolo l)) ;=> (())

  (run 1 [q]
    (fresh [x]
      (lolo (lcons '(a b) x)))) ;=> (_0)

  (run 1 [x]
    (lolo (llist '(a b) '(c d) x))) ;=> (())

  (run 5 [x]
    (lolo (llist '(a b) '(c d) x))) ;=> (() (()) ((_0)) (() ()) ((_0 _1)))

  (run 5 [x]
    (lolo (llist '(a b) '(c d) '(() ())))) ;=> (_0)

  (run 5 [x]
    (lolo x)) ;=> (() (()) ((_0)) (() ()) ((_0 _1)))

  )
(comment
  ;;-------------------------------------------------------------
  ;; Singleton redefined

  (run 1 [z]
    (loso (lcons '(g) z))) ;=> (())

  (run 1 [z]
    (loso (lcons '(g) '()))) ;=> (_0)

  (run 5 [z]
    (loso (lcons '(g) z)))
  ;=> (() ((_0)) ((_0) (_1)) ((_0) (_1) (_2)) ((_0) (_1) (_2) (_3)))

  (run 5 [z]
    (loso (lcons '(g) '((_0) (_1) (_2))))) ;=> (_0)

  ;; z must be a list of singletons here, because it is in tail position
  ;; and loso expects a proper list of singletons
  (run 4 [r]
    (fresh [w x y z]
      (loso (llist `(g) (lcons `e w) (lcons x y) z))
      (l/== `(~w ~(lcons x y) ~z) r)))
  ;=> ((() (_0) ())
  ;    (() (_0) ((_1)))
  ;    (() (_0) ((_1) (_2)))
  ;    (() (_0) ((_1) (_2) (_3))))

  ;; without llist in loso (not a cons -> z must be a singleton):
  (run 4 [r]
    (fresh [w x y z]
      (loso `((g) ~(lcons `e w) ~(lcons x y) ~z))
      (l/== `(~w ~(lcons x y) ~z) r)))
  ;=> ((() (_0) (_1)))

  ;; in `out` we see z being merged into the list at tail position,
  ;; while it is actually a list of singletons
  (run 3 [out]
    (fresh [w x y z]
      (l/== (llist `(g) (lcons `e w) (lcons x y) z) out)
      (loso out)))
  ;=> (((g) (e) (_0))
  ;    ((g) (e) (_0) (_1))
  ;    ((g) (e) (_0) (_1) (_2)))

  )
(comment
  ;;-------------------------------------------------------------
  ;; Member redefined

  (defn member? [x l]
    (cond
      (empty? l)      false
      (= (first l) x) true
      :else (member? x (rest l))))

  (member? 'olive '(virgin olive oil)) ;=> true

  'membero ; re-implemented

  ;; in Clojure, member? can be replaced by:
  ;; - (contains l x)  if l is a map or set
  ;; - (some? (some #(= % x) l))  for other collections

  (run* [q]
    (membero 'olive '(virgin olive oil))) ;=> (_0)

  (run 1 [y]
    (membero y '(hummus with pita))) ;=> (hummus)

  (run 1 [y]
    (membero y '(with pita))) ;=> (with)

  (run 1 [y]
    (membero y '(pita))) ;=> (pita)

  (run 1 [y]
    (membero y '())) ;=> ()

  ;; when l is a proper list,
  ;; (run* [y] (membero y l)) always returns the value of l
  (run* [y]
    (membero y '(hummus with pita))) ;=> (hummus with pita)

  ;; core.logic also has this function:
  (run* [y]
    (l/membero y '(hummus with pita))) ;=> (hummus with pita)

  ;; in this case, 'peaches fails because it is not a proper list
  (run* [y]
    (l/membero y (llist 'pear 'grape 'peaches))) ;=> (pear grape)

  ;; here, x gets associated with e, because there must be a member e
  ;; in the list for the goal to succeed
  (run* [x]
    (l/membero `e `(pasta ~x fagioli))) ;=> (e)

  ;; here, the membero succeeds with e before it even reaches x,
  ;; so x remains fresh
  (run 1 [x]
    (l/membero `e `(pasta e ~x fagioli))) ;=> (_0)

  ;; even if x could be anything in this example, it is still associated
  ;; with e, which succeeds the first conde goal in membero
  ;; and there is no other appearance of x in the list
  (run 1 [x]
    (l/membero `e `(pasta ~x e fagioli))) ;=> (e)

  ;; x and y are determined separately in this expression so x succeeds
  ;; with e and then y succeeds with e, each leaving the other fresh
  (run* [x y]
    (membero `e `(pasta ~x fagioli ~y))) ;=> ([e _0] [_0 e])

  (run* [q]
    (fresh [x y]
      (l/== `(pasta ~x fagioli ~y) q)
      (l/membero `e q))) ;=> ((pasta e fagioli _0) (pasta _0 fagioli e))

  ;; this is not ((tofu)), because firsto in membero leaves the lvar
  ;; of the rest of the list fresh, so l can be an improper list
  (run 1 [l]
    (membero 'tofu l)) ;=> ((tofu . _0))

  (comment
    ;; this example makes it clear:
    (run 1 [q]
      (l/firsto q 'apple))) ;=> ((apple . _0))

  (run* [l]
    (membero 'tofu l)) ; no value (list can be infinitely long)

  ;; membero always ends with firsto, where the first element gets
  ;; associated with tofu and the rest with anything
  (run 5 [l]
    (membero 'tofu l))
  ;=> ((tofu . _0)
  ;    (_0 tofu . _1)
  ;    (_0 _1 tofu . _2)
  ;    (_0 _1 _2 tofu . _3)
  ;    (_0 _1 _2 _3 tofu . _4))

  )
(comment
  ;;-------------------------------------------------------------
  ;; Member with proper lists required

  'proper-membero ; implemened

  ;; no improper lists with proper-membero:
  (run 12 [l]
    (proper-membero 'tofu l))
  ;=> ((tofu)
  ;    (tofu _0)
  ;    (_0 tofu)
  ;    (tofu _0 _1)
  ;    (tofu _0 _1 _2)
  ;    (_0 tofu _1) 
  ;    (tofu _0 _1 _2 _3)
  ;    (tofu _0 _1 _2 _3 _4)
  ;    (_0 _1 tofu)
  ;    (_0 tofu _1 _2)
  ;    (tofu _0 _1 _2 _3 _4 _5)
  ;    (tofu _0 _1 _2 _3 _4 _5 _6))
  ;; -> is it going to generate all permutations with tofu?

  ;; proper-membero can be derived (+ simplification) from this function:
  (defn proper-member? [x l]
    (cond
      (empty? l) false
      (= (first l) x) (list? (rest l))
      :else (proper-member? x (rest l))))

  (comment
    ;; I think the resto part in the first conde case is not needed,
    ;; because listo derives the rest and makes sure it is a proper list
    ;; fresh variables are also already declared in listo

    ;; simpler version:
    #_:clj-kondo/ignore
    (defrel proper-membero-1 [x l]
      (conde
        [(l/firsto l x) (listo l)]
        [(fresh [d]
           (l/resto l d)
           (member-propero x d))]))

    (run 5 [l]
      (proper-membero-1 'tofu l))
    ;=> ((tofu) (tofu _0) (tofu _0 _1) (_0 tofu) (tofu _0 _1 _2))
    )

  )

