#lang racket
(provide (all-defined-out))

#|
 empty? function, checks if agiven list has a value diferent to _, if all the list
 is made out of _'s the function returns true, if not returns false
|#
(define (empty? grid)
   (cond ((null? grid)
           #t)
          ((empty?-aux (car grid))
           #f)
          (else
           (empty? (cdr grid)))))

; Given a list checks if the are spaces on a list diferent to _, if there are returns true, if not returns false
(define (empty?-aux list)
   (cond ((null? list)
          #f)
         ((not (equal? '_ (car list)))
          #t)
         (else
          (empty?-aux (cdr list)))))

; TESTS

#|
(empty? '((_ _ _)
          (_ _ _)
          (_ _ _)))

(empty? '((_ o _ x)
          (o _ x o)
          (o _ o _)))

(empty? '((_ _ _ _)
          (_ _ _ _)
          (_ _ _ _)
          (_ _ _ _)
          (_ _ _ _)))

(empty? '((_ _ _ _)
          (_ _ _ _)
          (_ _ _ _)
          (_ _ _ _)
          (_ _ _ o)))
|#