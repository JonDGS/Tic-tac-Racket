;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname winner) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
#lang racket
(provide (all-defined-out))

#|
 winner?, function that takes a grid to analize and returns true if there is a horizontal, vertical or diagonal line
 that goes from side to side in the grid.
|#

(define (winner? grid)
   (cond ((winner?-h grid) 
          #t)
         ((winner?-v grid)
          #t)
         ;((winner?-d grid)
         ; #t)
         (else
          #f)))

; HORIZONTAL check
(define (winner?-h grid)
   (cond ((null? grid)
          #f)
         ((or (winner?-h-aux1 (car grid) 'x) (winner?-h-aux1 (car grid) 'o))
          #t)
         (else
          (winner?-h (cdr grid)))))

; Returns true if all the columns of a row are the same as the given element, flase otherwise
(define (winner?-h-aux1 list element)
   (cond ((null? list)
          #t)
         ((equal? element (car list))
          (winner?-h-aux1 (cdr list) element))
         (else
          #f)))

; VERTICAL check
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