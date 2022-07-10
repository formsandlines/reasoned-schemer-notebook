(ns reas.cpt5
  (:require
   [clojure.core.logic :as l :refer [run run* succeed fail s# u# defne
                                     conde fresh lcons llist]]
   [reas.utils :as rs :refer [defrel]]))


;;=============================================================
;; CHAPTER 5 - MEMBERS ONLY

(defn memo [x l out]
  (conde
    [(l/firsto l x) (l/== l out)]
    [(fresh [d]
       (l/resto l d)
       (memo x d out))]))

(comment
  ;; alternative, more concise definitions using defne:

  #_:clj-kondo/ignore
  (defne memo [x l out]
    ([_ [x . _] l])
    ([_ [_ . d] _] (memo x d out)))

  )

(comment
  ;;-------------------------------------------------------------
  ;; membero

  (defn mem [x l]
    (cond
      (empty? l) false
      (= (first l) x) l
      :else (mem x (rest l))))

  (mem 'fig
    '(roll okra fig beet roll pea)) ;=> (fig beet roll pea)

  (mem 'fig
    '(roll okra beet beet roll pea)) ;=> false

  (mem 'roll
    (mem 'fig
      '(roll okra fig beet roll pea))) ;=> (roll pea)

  (run* [q]
    (memo 'fig '(pea) '(pea))) ;=> ()

  (run* [out]
    (memo 'fig '(fig) out)) ;=> ((fig))

  (run* [out]
    (memo 'fig '(fig pea) out)) ;=> ((fig pea))

  (run* [r]
    (memo r
      '(roll okra fig beet fig pea)
      '(fig beet fig pea))) ;=> (fig)

  (run* [x]
    (memo 'fig '(fig pea) (list 'pea x))) ;=> ()

  (run* [x]
    (memo 'fig '(fig pea) (list x 'pea))) ;=> (fig)

  (run* [out]
    (memo 'fig '(beet fig pea) out)) ;=> ((fig pea))

  (run 1 [out]
    (memo 'fig '(fig fig pea) out)) ;=> ((fig fig pea))

  ;; the second conde line also succeeds in this case, so we have 2 values:
  (run* [out]
    (memo 'fig '(fig fig pea) out)) ;=> ((fig fig pea) (fig pea))

  ;; the fresh lvar x will be fused with fig for the goal to succeed:
  (run* [out]
    (fresh [x]
      (memo 'fig (list 'a x 'c 'fig 'e) out))) ;=> ((fig c fig e) (fig e))

  ;; fig must appear in y in order to contribute values beyond the first two
  ;; ? not sure why it is always _0 in (fig . _0) and not _1, etc.
  (run 5 [x y]
    (memo 'fig (llist 'fig 'd 'fig 'e y) x))
  ;=> ([(fig d fig e . _0) _0]
  ;    [(fig e . _0) _0]
  ;    [(fig . _0) (fig . _0)]
  ;    [(fig . _0) (_1 fig . _0)]
  ;    [(fig . _0) (_1 _2 fig . _0)])

  )

