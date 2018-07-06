#lang racket
(require racket/list)
(require plot)
(define mylist (list '(and T (not F))
                     '(or F (not T))
                     '(and (and F F)(or T (and T T)))
                     '(or F (not T))
                     '(T)
                     '(T T T T T T T T T T T)))
(define levels
(map (lambda (i)
         (length (flatten i)))
       (remove-duplicates mylist)))
;(define levels (list '1 '1 '2 '3 '3 '3 '4 '4 '4 '4 '6 '10 '10 '10 '10 '10))
; 11 is max difficulty of the puzzles encountered in the current dataset
(define out-of 11)
(define breaks '((0 4) (5 8) (9 15)))
(define (label l)
  (format "~a-~a" (first l) (second l)))
(define (buckets l)
  (let ((sorted (sort l <)))
    (for/list ([b breaks])
          (vector (label b)
           (count (lambda (x) (and 
                    (<= x ( second b))
                    (>= x ( first b))))
               levels)))))
(plot
 (list
  (discrete-histogram 
  (buckets levels)))
 #:out-file "hist.png"
#:title "Plot of difficulty ratings and amount it occurs"
#:x-label "difficulty rating"
#:y-label "# of occurances")