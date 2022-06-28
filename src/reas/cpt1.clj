(ns reas.cpt1
  (:require
   [clojure.core.logic :as l :refer [run* succeed fail s# u#
                                     conde fresh and* or*]]
   [reas.utils :as rs :refer [defrel]]))


;;=============================================================
;; CHAPTER 1 - Playthings

(comment
  ;;-------------------------------------------------------------
  ;; Goals

  'succeed ;; is a goal, that never fails
  's# ;; alternative syntax

  'fail ;; is a goal, that never succeeds
  'u# ;; alternative syntax

  ;; run* produces a list for all successful associations with q
  (run* [q] succeed) ;=> (_0)
  (run* [q] s#) ;=> (_0)

  ;; run* produces an empty list, if one of its goals fails
  (run* [q] fail) ;=> ()
  (run* [q] u#) ;=> ()

  'l/== ;; is a goal that attempts to unify two values

  ;; for unification to succeed, both values must be equal
  (run* [q] (l/== 'pea 'pod)) ;=> ()

  ;; if a fresh lvar appears in ==, it is associated with the other value
  ;; as a means to unify both values
  (run* [q] (l/== q 'pea)) ;=> (pea)

  ;; order doesn’t matter in ==
  (= (run* [q] (l/== q 'pea)) (run* [q] (l/== 'pea q))) ;=> true

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; THE FIRST LAW OF ≡
  ;; (≡ v w) can be replaced by (≡ w v)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  )
(comment
  ;;-------------------------------------------------------------
  ;; lvars (logical variables)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Every variable is initially fresh.
  ;; A variable is no longer fresh if it becomes associated with
  ;; a non-variable value or if it becomes associated with a
  ;; variable that, itself, is no longer fresh.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  'q ;; and any other symbol that appears in a goal, is an lvar
  'fresh ;; introduces new lvars (like let)

  ;; if a goal succeeds independently of q, q can take any value to succeed
  '_0 ;; (or generally _n) reifies the possible association with any value
  (run* [q] (l/== 'pea 'pea))

  ;; if a fresh lvar is introduced but never appears in a goal,
  ;; it does not affect the result
  (run* [q]
    (fresh [x] (l/== 'pea q))) ;=> (pea)

  ;; if an lvar remains fresh and succeeds, it gets reified as _n
  (run* [q]
    (fresh [x] (l/== (cons x '()) q))) ;=> ((_0))

  ;; unifying two fresh variables “fuses” them
  (run* [q]
    (fresh [x]
      (l/== x q)))
  )
(comment
  ;;-------------------------------------------------------------
  ;; patterns in unification

  ;; beware namespaces in symbols for ` vs ' !
  (= `a 'a) ;=> false (in `x the symbol is fully qualified)

  ;; if two patterns match in == and all appearing lvars can be unified,
  ;; the goal will succeed
  (run* [q]
    (l/== `(((pea)) pod) `(((pea)) ~q))) ;=> (cpt1/pod) 
  (run* [q]
    (l/== `(((~q)) pod) `(((pea)) pod))) ;=> (cpt1/pea)
  ;; fusing fresh lvars works the same when nested:
  (run* [q]
    (fresh [x]
      (l/== `(((~q)) pod) `(((~x)) pod)))) ;=> (_0)

  ;; fused lvars get associated with the same values if they are
  ;; no longer fresh

  ;; here, the fresh lvar x gets associated with pod and since
  ;; q gets fused with x, it will also be associated with pod
  (run* [q]
    (fresh [x]
      (l/== `(((~q)) ~x) `(((~x)) pod)))) ;=> (cpt1/pod)

  (run* [q]
    (fresh [x]
      (l/== `(~x ~x) q))) ;=> ((_0 _0))

  (run* [q]
    (fresh [x]
      (fresh [y]
        (l/== `(~q ~y) `((~x ~y) ~x))))) ;=> ((_0 _0))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; THE SECOND LAW OF ≡
  ;; If x is fresh, then (≡ v x) succeeds and associates v with x,
  ;; unless x occurs in v.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  )
(comment
  ;;-------------------------------------------------------------
  ;; conjunction

  'and* ;; is core.logic’s conjunction goal

  ;; in cldisj.logic:
  (run* [q]
    (conde [s# s#])) ;=> (_0)
  ;; or just:
  (run* [q] s# s#) ;=> (_0)
  ;; with my own macro:
  (run* [q]
    (and* [s# s#])) ;=> (_0)

  ;; if both goals in conj succeed, conj will succeed
  ;; the value associated with q will be returned
  (run* [q]
    (and* [s# (l/== 'corn q)])) ;=> (corn)

  (run* [q]
    (and* [u# (l/== 'corn q)])) ;=> ()

  ;; this fails, because q is no longer fresh in the second unification
  ;; and cannot be unified with 'meal
  (run* [q]
    (and* [(l/== 'corn q) (l/== 'meal q)])) ;=> ()

  ;; this succeeds, because q has the same associated value as 'corn
  (run* [q]
    (and* [(l/== 'corn q) (l/== 'corn q)])) ;=> (corn)

  )
(comment
  ;;-------------------------------------------------------------
  ;; disjunction

  'or* ;; is core.logic’s disjunction goal

  ;; with conde:
  (run* [q]
    (conde [u#] [u#])) ;=> ()
  ;; with or*:
  (run* [q]
    (or* [u# u#])) ;=> ()

  ;; if one of the goals in disj succeeds, disj itself will succeed
  ;; all values (alternatively) associated with q will be returned
  (run* [q]
    (or* [(l/== 'olive q) u#])) ;=> (olive)

  (run* [q]
    (or* [s# (l/== 'oil q)])) ;=> (_0 oil)

  (run* [q]
    (or* [(l/== 'olive q) (l/== 'oil q)])) ;=> (olive oil)

  ;; here, q gets independently associated with a tuple of two fresh lvars
  ;; which get reified as _0 or _1 depending on their order in the form
  (run* [q]
    (fresh [x]
      (fresh [y]
        (or*
          [(l/== `(~x ~y) q)
           (l/== `(~y ~x) q)])))) ;=> ((_0 _1) (_0 _1))

  ;; different orders of return values are considered to be equal
  ('equal? (run* [x]
             (or* [(l/== 'olive x) (l/== 'oil x)])) ;=> (olive oil)
           (run* [x]
             (or* [(l/== 'oil x) (l/== 'olive x)]))) ;=> (oil olive)
  ;=> true (if equal? was defined)

  )
(comment
  ;;-------------------------------------------------------------
  ;; combining conjunction and disjunction

  (run* [x]
    (or*
      [(and* [(l/== 'olive x)
              u#])
       (l/== 'oil x)])) ;=> (oil)

  (run* [x]
    (or*
      [(and* [(l/== 'olive x) s#])
       (l/== 'oil x)])) ;=> (olive oil)

  (run* [x]
    (or*
      [(l/== 'oil x)
       (and* [(l/== 'olive x) s#])])) ;=> (oil olive)

  (run* [x]
    (or*
      [(and* [(l/== 'virgin x) u#])
       (or*
         [(l/== 'olive x)
          (or*
            [s#
             (l/== 'oil x)])])])) ;=> (olive _0 oil)

  (run* [r]
    (fresh [x]
      (fresh [y]
        (and*
          [(l/== 'split x)
           (and*
             [(l/== 'pea y)
              (l/== `(~x ~y) r)])])))) ;=> ((split pea))

  )
(comment
  ;;-------------------------------------------------------------
  ;; simplifications 

  (run* [r]
    (fresh [x]
      (fresh [y]
        (and*
          [(and*
             [(l/== 'split x)
              (l/== 'pea y)])
           (l/== `(~x ~y) r)])))) ;=> ((split pea))

  ;; can be shortened to:
  (run* [r]
    (fresh [x y]
      (and*
        [(l/== 'split x)
         (l/== 'pea y)
         (l/== `(~x ~y) r)]))) ;=> ((split pea))

  ;; fresh lvars in run* arguments

  ;; if fresh lvars are declared in arguments to run*,
  ;; their values will be returned in a vector in the same order
  ;; -> NOT as alternatively succeeding values!
  (run* [r x y]
    (and*
      [(l/== 'split x)
       (l/== 'pea y)
       (l/== `(~x ~y) r)])) ;=> ([(split pea) split pea])

  ;; without r, we get closer to the first result:
  (run* [x y]
    (and*
      [(l/== 'split x)
       (l/== 'pea y)])) ;=> ([split pea])

  (run* [x y]
    (or*
      [(and* [(l/== 'split x) (l/== 'pea y)])
       (and* [(l/== 'red x) (l/== 'bean y)])])) ;=> ([split pea] [red bean])

  ;; if an lvar can be associated alternatively in two goals,
  ;; lvars associated with it succeed with both associations alternatively
  (run* [r]
    (fresh [x y]
      (and*
        [(or*
           [(and* [(l/== 'split x) (l/== 'pea y)])
            (and* [(l/== 'red x) (l/== 'bean y)])])
         (l/== `(~x ~y soup) r)])))
  ;=> ((split pea soup) (red bean soup))

  ;; fresh behaves like a conj, so this has the same value:
  (run* [r]
    (fresh [x y]
      (or*
        [(and* [(l/== 'split x) (l/== 'pea y)])
         (and* [(l/== 'red x) (l/== 'bean y)])])
      (l/== `(~x ~y soup) r)))

  ;; run* also behaves like a conj
  (run* [x y z]
    (or*
      [(and* [(l/== 'split x) (l/== 'pea y)])
       (and* [(l/== 'red x) (l/== 'bean y)])])
    (l/== 'soup z)) ;=> ([split pea soup] [red bean soup])

  )
(comment
  ;;-------------------------------------------------------------
  ;; defrel 

  (defn teacup-proto [t]
    (fn [s]
      (fn []
        ((or* [(l/== 'tea t) (l/== 'cup t)])
         s))))

  (run* [x]
    (teacup-proto x)) ;=> (tea cup)

  'defrel ;; (not in core.logic) is a macro that can be used to
  ;; define new relations in terms of primitive/other relations

  #_:clj-kondo/ignore
  (defrel teacupo [t]
    (or* [(l/== 'tea t) (l/== 'cup t)]))

  (run* [x]
    (teacupo x)) ;=> (tea cup)

  (run* [x y]
    (or*
      [(and* [(teacupo x) (l/== true y)])
       (and* [(l/== false x) (l/== true y)])]))
  ;=> ([false true] [tea true] [cup true])

  (run* [x y]
    (teacupo x)
    (teacupo y)) ;=> ([tea tea] [tea cup] [cup tea] [cup cup])

  (run* [x y]
    (teacupo x)
    (teacupo x)) ;=> ([tea _0] [cup _0])

  (run* [x y]
    (or*
      [(and* [(teacupo x) (teacupo x)])
       (and* [(l/== false x) (teacupo y)])]))
  ;=> ([false tea] [false cup] [tea _0] [cup _0])

  )
(comment
  ;;-------------------------------------------------------------
  ;; cond^e pattern

  'conde ;; -> disjunction of conjunctions

  ;; the last expression can be reformulated in terms of conde:
  (run* [x y]
    (conde
      [(teacupo x) (teacupo x)]
      [(l/== false x) (teacupo y)]))
  ;=> ([false tea] [false cup] [tea _0] [cup _0])

  (run* [x y]
    (conde
      [(l/== 'split x) (l/== 'pea y)]
      [(l/== 'red x) (l/== 'bean y)])) ;=> ([split pea] [red bean])

  (run* [x]
    (conde
      [(l/== 'olive x) u#]
      [(l/== 'oil x)])) ;=> (oil)

  (run* [x y]
    (conde
      [(fresh [z]
         (l/== 'lentil z))]
      [(l/== x y)])) ;=> ([_0 _0] [_0 _1])

  (run* [x y]
    (conde
      [(l/== 'split x) (l/== 'pea y)]
      [(l/== 'red x) (l/== 'bean y)]
      [(l/== 'green x) (l/== 'lentil y)]))
  ;=> ([split pea] [red bean] [green lentil])

  ;; this means we don’t even need conj/and* and disj/or* anymore!

  ;; the 'e' in conde stands for “everything”:
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; THE LAW OF cond^e
  ;; Every successful cond^e line contributes one or more values.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  )

