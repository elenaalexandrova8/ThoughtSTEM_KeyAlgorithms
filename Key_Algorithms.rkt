#lang racket

(require racket/list) ;needed library

(require plot) ;needed for any type of graph

(define mylist (list '(and T(or F T(not T F))) ;list of a list - mylist contains data set from other group. should be updated with more puzzles as they are created

                       '(not(and(and T(and T (or F T)))))))

(define (make-graph l)  ;defines the function to produce the entire graph with given list 'l'.

(define levels ;define levels as a list to save results of lambda function below

(map (lambda (i) ;map applies to all elements in the list, lambda is used to take in arguement and procedure  

         (length (flatten i))) ;flatten ignored syntax and takes number of words ie AND, OR, T, F etc. used to determine difficulty 

       (remove-duplicates l)));removes all duplicates i


; 11 is max difficulty of the puzzles encountered in the current dataset

(define out-of 11)

(define breaks '((0 4) (5 8) (9 15))) ;used as groupings in graph. must be 2 numbers ie 0-4, 5-8 etc. can have more groupings of two

(define (label l)

  (format "~a-~a" (first l) (second l))) ;correct version of formatting

(define (buckets l) ;procedure for creating graph

  (let ((sorted (sort l <)))

    (for/list ([b breaks])

          (vector (label b)

           (count (lambda (x) (and 

                    (<= x ( second b))

                    (>= x ( first b))))

               levels)))))

(plot ;actually plot the graph

 (list

  (discrete-histogram 

  (buckets levels)))

 #:out-file "hist.png" ;additional info, how to name title, x-axis, Y-axis, etc

#:title "Plot of difficulty ratings and amount it occurs"

#:x-label "difficulty rating"

#:y-label "# of occurances"))

(make-graph mylist)