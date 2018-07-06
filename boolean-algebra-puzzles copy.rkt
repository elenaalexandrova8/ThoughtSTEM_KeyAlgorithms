#lang racket

(require
  "../card-designs.rkt"
  "../rules.rkt"
  (prefix-in bool: "../boolean-algebra-cards.rkt")
  (prefix-in bool: "../themes/emoji-boolean-algebra.rkt"))

(define boolean-algebra
  (bool:render bool:theme))


(define (take-n-random lst n)
  (take (shuffle lst) n))

(define testlist (list '(and T(or F T(not T F)))
                       '(or F(not T))
                       '(and(and F F)(or T (and T T)))
                       '(and(not (or T F))(and T (or F T)))
                       '(not(not(and F(and (or T T)F))))
                       '(not(not(not T)))
                       '(not(not(not T)))
                       '(and(or F(not F))(or T(not F)))
                       '(or(not F) (and(and T T)F))
                       '(or F(and F (or T T)))
                       '(not(not(and(or F F)T)))
                       '(or T(and T(or F T)))
                       '(and(not(and T T))(not (and F F)))
                       '(or(not T)((and(not F)(F))))
                       '(or(not(not(T)))(not (not (F))))
                       '(or (and T F)(and T T))
                       '(and(not T)(or ((not) T)(and T T)))
                       '(and(or (not T) T)(and T (not F)))
                       '(and(not(not F))(not (not T)))
                       '(not(and F(or F T)))
                       '(and(or F (and(or F T)F))T)
                       '(and T(or(and(not F) F)(not T)))
                       '(or(and T T)(not T))
                       '(not(or(and T F)(and F F)))
                       '(or(not(not F))(not F))
                       '(not(or(and(T)(or F T))(not F)))
                       '(not(and T (not(or F (and T T)))))
                       '(or(or T T)(or(or F F)T))
                       '(and F(and F(and T T)))
                       '(not(or(or F F)(and F F)))
                       '(not(and T(and T(or T F))))
                       '(or (not(not(not F)))(not(not(not T))))
                       '(or T(not (and T F)))
                       '(not(or(or F F))(and F(not F)))
                       '(and(not F)(not(or T F)))
                       '(not(or(or T F))(and T(not(and T T))))
                       '(or(or(and T F)(or T F))(and(or T F)(not T)))
                       '(and(not(or T)(and T F))(and(and T F)(or F)))
                       '(not(and(or(not T)(not F))(and(not F)(not F))))                   
                       '(not(or(and T F)(or T F))(and(not T F)(not T)))
                       '(and(not F)(not(or T F)))
                       '(or (and T F)(or (or (or T F))))
                       '(not(or(and T F)(or T F))(and(not T F)(not T)))
                       '(or(or(or F)(and F T))(and(and T F)(or F)))
                       '(and(not (or T F))(and T (or F T)))
                       '(not(not(and F(and (or T T)F))))
                       '(not(and(and T(and T (or F T))))) ))



(puzzle-card (rule

              (first (take-n-random testlist 1))

                   '?))

#|
(define test1 (puzzle-card (rule '(or F(not T))'?)))
(define test2 (puzzle-card (rule '(and(and F F)(or T (and T T)))'?)))
(append(test1)(test2))|#

;More puzzles here...
;(puzzle-card (rule '(and T(or F T(not T F)))'?))


#|
(puzzle-card (rule '(or F(not T))'?))

;adding my puzzle here...
(puzzle-card (rule '(and(and F F)(or T (and T T)))'?))


(puzzle-card (rule '(and(not (or T F))(and T (or F T)))'?))

(puzzle-card (rule '(not(not(and F(and (or T T)F))))'?))

(puzzle-card (rule '(or F T)'?))

(puzzle-card (rule '(and T(or F F))'?))

(puzzle-card (rule '(or T(not F))'?))

(puzzle-card (rule '(not(or T(not T)))'?))

(puzzle-card (rule '(or(not(and T F))(or F(and F T)))'?))

(puzzle-card (rule '(not(not(not T)))'?))

(puzzle-card (rule '(and(or F(not F))(or T(not F))) 
                   '?))|#

#|(puzzle-card (rule '(or(not F) (and(and T T)F))
                   '?))

(puzzle-card (rule '(not(or(and T(and F F)T))
                   '?))

(puzzle-card (rule '(or F(and F (or T T)))
                   '?))

(puzzle-card (rule '(not(not(and(or F F)T)))
                   '?))

(puzzle-card (rule '(or T(and T(or F T)))
                   '?))

(puzzle-card (rule '(and(not(and T T))(not (and F F)))
                   '?))

(puzzle-card (rule '(or(not T)((and(not F)(F))))
                   '?))

(puzzle-card (rule ' (or(not(not(T)))(not (not (F))))
                   '?))

(puzzle-card (rule ' (or (and T F)(and T T))
                   '?))

(puzzle-card (rule ' (and(not T)(or ((not) T)(and T T)))
                   '?))
(puzzle-card (rule '(and(or (not T) T)(and T (not F)))
                   '?))

(puzzle-card (rule '(and(not(not F))(not (not T)))
'?))

(puzzle-card (rule '(not(and F(or F T)))
                   '?))

(puzzle-card (rule '(and(or F (and(or F T)F))T)
'?))

(puzzle-card (rule '(and T(or(and(not F) F)(not T)))

'?))

(puzzle-card (rule '(or(and T T)(not T))
                   '?))

(puzzle-card (rule '(not(or(and T F)(and F F)))
                   '?))

(puzzle-card (rule '(or(not(not F))(not F))
                   '?))

(puzzle-card (rule '(not(or(and(T)(or F T))(not F)))
                   '?))

(puzzle-card (rule '(not(and T (not(or F (and T T)))))
                   '?))

;1
(puzzle-card (rule '(or(or T T)(or(or F F)T))
                   '?))

;2
(puzzle-card (rule '(and F(and F(and T T)))
                   '?))

;3
(puzzle-card (rule '(not(or(or F F)(and F F)))
                   '?))

;4
(puzzle-card (rule '(not(and T(and T(or T F))))
                   '?))

;5
(puzzle-card (rule '(or (not(not(not F)))(not(not(not T))))
                   '?))

(puzzle-card (rule '(or T(not (and T F)))
                   '?))

(puzzle-card (rule '(not(or(or F F))(and F(not F)))
                   '?))

(puzzle-card (rule ' (and(not F)(not(or T F)))
                   '?))

(puzzle-card (rule ' (not(or(or T F))(and T(not(and T T))))
                   '?))

(puzzle-card (rule ' (or(or(and T F)(or T F))(and(or T F)(not T)))
                   '?))


(puzzle-card (rule ' (and(not(or T)(and T F))(and(and T F)(or F)))
                   '?))


(puzzle-card (rule ' (not(and(or(not T)(not F))(and(not F)(not F))))
                   ' ?))



(puzzle-card (rule ' (not(or(and T F)(or T F))(and(not T F)(not T)))
                   ' ?))


(puzzle-card (rule ' (and(not F)(not(or T F)))
                   ' ?))

                   
(puzzle-card (rule ' (or (and T F)(or (or (or T F))))
                      '?))

(puzzle-card (rule ' (not(or(and T F)(or T F))(and(not T F)(not T)))
                   ' ?))

(puzzle-card (rule ' (or(or(or F)(and F T))(and(and T F)(or F)))
                   '?))


;Puzzles

(puzzle-card (rule '(and(not (or T F))(and T (or F T)))
                   '?))

(puzzle-card (rule '(not(not(and F(and (or T T)F))))
                   '?))

(puzzle-card (rule '(not(and(and T(and T (or F T)))))
                   '?))|#
