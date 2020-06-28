#lang racket
(provide (all-defined-out))

#|
 winner?, function that takes a grid to analize and returns true if there is a horizontal, vertical or diagonal line
 that goes from side to side in the grid.
|#

(define (winner? grid)
   (cond ((or (winner?-h grid)(winner?-v grid)(winner?-d grid)) 
          #t)
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
          (same-element? (cdr list)
                         element))
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
          (winner?-v-aux1 (cdr grid)
                          element))
         (else
          #f)))

; This takes a grid and creates a new grid without the first column of each row
(define (winner?-v-aux2 grid nGrid)
   (cond ((not (null? grid))
          (winner?-v-aux2 (cdr grid)
                          (cons (cdar grid) nGrid)))
         (else
          nGrid)))

; DIAGONAL check - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (winner?-d grid)
   (cond ((or (winner?-d-aux1 grid)(winner?-d-aux1 (change-grid grid '())))
          #t)
         (else
          #f)))

(define (winner?-d-aux1 grid)
   (cond ((analize-diagonals (get-diagonals grid '() '()))
          #t)
         (else
          #f)))          

; Checks if a giveng list of lists contains a sub list with both a length greater, or equal, than three and all its elements the same
(define (analize-diagonals lists)
   (cond ((null? lists)
          #f)
         ((and (<= 3 (length (car lists)))(or (same-element? (car lists) 'x)(same-element? (car lists) 'o)))
          #t)
         (else
          (analize-diagonals (cdr lists)))))

; This function takes a grid and inverts its x axis, this is used to inspect the antidiagonals
(define (change-grid grid nGrid)
   (cond ((null? grid)
          nGrid)
         (else
          (change-grid (cdr grid)
                       (append nGrid (list (reverse-list (car grid) '())))))))

; Takes a list and inverts the order of its elements
(define (reverse-list oList rList)
   (cond ((null? oList)
          rList)
         (else
          (reverse-list (cdr oList)
                        (cons (car oList) rList)))))

; Takes a grid and gives us a list of diagonal lists of the grid
(define (get-diagonals grid nGrid diagonals)
   (cond ((and (null? grid)(null? nGrid))
          diagonals)
         ((null? nGrid)
          (get-diagonals grid
                         (append nGrid (list (car grid)))
                         diagonals))
         ((null? grid)
          (get-Diagonals-aux grid
                             (delete-first-of-lists nGrid '())
                             (append diagonals (list (get-first-of-lists nGrid '())))))
         (else
          (get-Diagonals-aux (cdr grid)
                             (delete-first-of-lists nGrid '())
                             (append diagonals (list (get-first-of-lists nGrid '())))))))

(define (get-Diagonals-aux grid nGrid diagonals)
   (cond ((null? grid)
          (get-diagonals grid
                         nGrid
                         diagonals))
         (else
          (get-diagonals grid
                         (append nGrid (list (car grid)))
                         diagonals))))

; Takes a list of lists and returns a list made of the fisrt element from each sub list
(define (get-first-of-lists lists nLists)
   (cond ((null? lists)
          nLists)
         (else
          (get-first-of-lists (cdr lists)
                              (append nLists (list (caar lists)))))))

; Takes a list of lists and deletes all the first elements each sub list
(define (delete-first-of-lists lists nLists)
   (cond ((null? lists)
          nLists)
         ((equal? 1 (length  (car lists)))
          (delete-first-of-lists (cdr lists)
                                 nLists))
         (else
          (delete-first-of-lists (cdr lists)
                                 (append nLists (list (cdar lists)))))))




; TESTS

#|
(winner? '((o o x)
           (o _ o)
           (x o o)))

(winner? '((o x o o)
           (x o x x)
           (o o o o)))

(winner? '((o x o)
           (o x o)
           (o x x)
           (x x o)))

(winner? '((o x o o)
           (x _ o x)
           (o _ x x)
           (x x o o)))

(winner? '((x o o)
           (o x x)
           (x o o)))

(winner? '((o x o o)
           (x o x x)
           (o x o o)))

(winner? '((o x o o x)
           (o o _ x x)
           (x x x _ x)
           (x _ o x x)))

(winner? '((o x o o)
           (x x o x)
           (o o x x)
           (_ x o o)
           (x _ o o)
           (o x o o)))
|#