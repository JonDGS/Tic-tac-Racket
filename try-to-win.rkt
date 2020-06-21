#lang racket/base

(require "logic-utilities.rkt")


;;Loop for get-value-in-list-by-index function which searches within a list with a given index
(define (get-value-in-list-by-index-aux lista index current)
  (cond
    ((null? lista) -1)
    ((= index current) (car lista))
    (else (get-value-in-list-by-index-aux (cdr lista) index (+ current 1)))))

;;Gets a value within a list given an index
(define (get-value-in-list-by-index lista index)
  (get-value-in-list-by-index-aux lista index 0))
  

;;Loop for getting value within a matrix based on an index
(define (get-value-base-on-index-aux matrix targetRow targetColumn currentRow currentAnswer)
  (cond
    ((not (equal? currentAnswer -1)) currentAnswer)
    ((null? (car matrix)) -1)
    ((= targetRow currentRow) (get-value-in-list-by-index (car matrix) targetColumn))
    (else (get-value-base-on-index-aux (cdr matrix) targetRow targetColumn (+ currentRow 1) -1))))

;;Gets the value on an index in a given matrix
(define (get-value-base-on-index matrix row column)
  (get-value-base-on-index-aux matrix row column 0 -1))
  

;;Loop for detecting the possibilty of horizontal lines within the grid
(define (place-next-n-aux grid numColumns n symbol currentM)
  (cond
    ((>= currentM numColumns) -1)
    ((equal? (get-value-base-on-index grid n currentM)  '_) (list n currentM))
    (else (place-next-n-aux grid numColumns n symbol (+ currentM 1)))))

;;Gets the n and m coordinates for placing a symbol to complete a horizontal line
(define (place-next-n grid numColumns n symbol)
  (place-next-n-aux grid numColumns n symbol 0))

;;Loop for detecting the possibility of vertical lines within the grid
(define (place-next-m-aux grid numRows m symbol currentN)
  (cond
    ((>= currentN numRows) -1)
    ((equal? (get-value-base-on-index grid currentN m) '_) (list currentN m))
    (else (place-next-m-aux grid numRows m symbol (+ currentN 1)))))

;;Gets the n and m coordinates for placing a symbol to complete a vertical line
(define (place-next-m grid numRows m symbol)
  (place-next-m-aux grid numRows m symbol 0))
  

;;Gets the n and m coordinates for a given move by the computer
(define (get-move grid numRows numColumns symbol start finish)
  (cond
    ((= (car start) (car finish)) (place-next-n grid numColumns (car start) symbol))
    ((= (cadr start) (cadr finish)) (place-next-m grid numRows (cadr start) symbol))))

(define (get-updated-grid grid )



;;(get-value-base-on-index (list '(1 2 3) '(4 5 6) '(7 8 9)) 1 1)
(get-move (list (list 'x '_ '_) (list 'x '_ '_) (list 'x '_ '_)) 3 3 'x '(0 0) '(2 0))