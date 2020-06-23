#lang racket
(provide (all-defined-out))

#|
; Draw function, checks a if a given grid has space left to keep playing.
; If its full there would be a draw and the function returns true, if there is
; space left on the gird the function returns false. This function should always
; be used after the winner? function, this because our defined state for a draw is
; simply a lack of spaces to keep playing and there could be a winner in a grid
; with no spaces left
|#

(define (draw? grid)
   (cond ((null? grid)
           #t)
          ((draw?-aux (car grid))
           #f)
          (else
           (draw? (cdr grid)))))

; Given a list checks if the are empty spaces on a list, if there are returns true, if not returns false
(define (draw?-aux list)
   (cond ((null? list)
          #f)
         ((equal? '_ (car list))
          #t)
         (else+3
          (draw?-aux (cdr list)))))
          

; TESTS

#|
(draw? '((_ o _)
         (o o x)
         (o _ o)))

(draw? '((_ o x x)
         (o _ x o)
         (o _ o _)))

(draw? '((x o x o)
         (o o x x)
         (o o x x)
         (o o x x)
         (o x o o)))
|#