#lang racket
(provide (all-defined-out))

#|
 winner?, function that takes a grid to analize and returns true if there is a horizontal, vertical or diagonal line
 that goes from side to side in the grid.
|#

(define (winner? grid)
   (cond ((or (winner?-h grid)(winner?-v grid)) 
          #t)
         ;((winner?-d grid)
         ; #t)
         (else
          #f)))

; HORIZONTAL check - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (winner?-h grid)
   (cond ((null? grid)
          #f)
         ((or (same-element? (car grid) 'x) (same-element? (car grid) 'o))
          #t)
         (else
          (winner?-h (cdr grid)))))

; Returns true if all the columns of a row are the same as the given element, flase otherwise
(define (same-element? list element)
   (cond ((null? list)
          #t)
         ((equal? element (car list))
          (same-element? (cdr list) element))
         (else
          #f)))

; VERTICAL check - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (winner?-v grid)
   (cond ((or (null? grid) (null? (car grid)))
          #f)
         ((or (winner?-v-aux1 grid 'x) (winner?-v-aux1 grid 'o))
          #t)
         (else
          (winner?-v (winner?-v-aux2 grid '())))))

; This looks at the first column of every row and returns true if all the columns are the same as the given element
(define (winner?-v-aux1 grid element)
   (cond ((null? grid)
          #t)
         ((equal? element (caar grid))
          (winner?-v-aux1 (cdr grid) element))
         (else
          #f)))

; This takes a grid and creates a new grid without the first column of each row
(define (winner?-v-aux2 grid nGrid)
   (cond ((not (null? grid))
          (winner?-v-aux2 (cdr grid) (cons (cdar grid) nGrid)))
         (else
          nGrid)))

; DIAGONAL check - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (winner?-d grid)
   (cond ((or (winner?-d-aux1 grid)(winner?-d-aux1 (change-grid grid '())))
          #t)
         (else
          #f)))

(define (winner?-d-aux1 grid)
   (cond ((analizeDiagonals (getDiagonals grid))
          #t)
         (else
          #f)))          

; Checks if a giveng list of lists has a list with both a length greater or equal to three and all its elements the same
(define (analizeDiagonals lists)
   (cond ((null? lists)
          #f)
         ((and (<= 3 (length (car lists)))(or (same-element? (car lists) 'x)(same-element? (car lists) 'o)))
          #t)
         (else
          (analizeDiagonals (cdr lists)))))

; Takes the diagonals of a grid and puts them in a list
(define (getDiagonals grid)
   (#f))

; This function takes a grid and inverts its x axis, this is used to inspect the antidiagonals
(define (change-grid grid nGrid)
   (cond ((null? grid)
          nGrid)
         (else
          (change-grid (cdr grid)(append nGrid (list (reverse-list (car grid) '())))))))

; Takes a list and inverts the order of its elements
(define (reverse-list oList rList)
   (cond ((null? oList)
          rList)
         (else
          (reverse-list (cdr oList)(cons (car oList) rList)))))






(analizeDiagonals '((o o o o)(x x x x)(o o o o)))

; TESTS
#|
(winner? '((x o o)
           (o x o)
           (x o x)))

(winner? '((o x o o)
           (x o x x)
           (o o o o)))

(winner? '((o x o)
           (o x o)
           (o x x)
           (x x o)))

(winner? '((o x o o)
           (o x o x)
           (o o x x)
           (x x o o)))
|#