#lang racket

(require "logic-utilities.rkt")


;;Loop for finding a list length
(define (get-list-len-aux currentList currentLength)
  (cond
    ((null? currentList) currentLength)
    (else (get-list-len-aux (cdr currentList) (+ currentLength 1)))))

;;Gets the length of a list
(define (get-list-len currentList)
  (get-list-len-aux currentList 0))

;;Adds element to the back of the list instead of the beginning
(define (cons-at-tail element currentList)
  (cond
    ((null? currentList) (list element))
    (else (cons (car currentList) (cons-at-tail element (cdr currentList)))))) 

;;Finds if an element is a member of a list
(define (miembro element elementList)
  (cond ((null? elementList) #f)
        ((equal? element (car elementList)) #t)
        (else (miembro element (cdr elementList)))))

;;Gets all the symbol of a row with their n and m coordinates
(define (get-symbol-from-row rowList currentRow currentColumn symbol currentPairs prevOriginList)
  (cond
    ((null? rowList) currentPairs)
    ((equal? (car rowList) '_) (get-symbol-from-row (cdr rowList) currentRow (+ currentColumn 1) symbol currentPairs prevOriginList))
    ((miembro (list currentRow currentColumn) prevOriginList) (get-symbol-from-row (cdr rowList) currentRow (+ currentColumn 1) symbol currentPairs prevOriginList))
    ((equal? (car rowList) symbol) (get-symbol-from-row (cdr rowList) currentRow (+ currentColumn 1) symbol (cons-at-tail (list currentRow currentColumn) currentPairs) prevOriginList))
    (else (get-symbol-from-row (cdr rowList) currentRow (+ currentColumn 1) symbol currentPairs prevOriginList))))

;;Loop for finding all possible pairs for a point given said point as input
(define (get-pairs-aux grid currentRow symbol currentPairs prevOriginList)
  (cond
    ((null? grid) (list currentPairs prevOriginList))
    (else (get-pairs-aux (cdr grid) (+ currentRow 1) symbol (get-symbol-from-row (car grid) (+ currentRow 1) 0 symbol currentPairs prevOriginList) prevOriginList))))

(define (get-pairs-without-origin grid symbol)
  (get-pairs-aux grid -1 symbol '() '()))


;;Finds all possible pairs of a point in a grid
(define (get-pairs grid origin symbol prevOriginList)
  (get-pairs-aux grid -1 symbol '() (cons origin prevOriginList)))


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


;;Verifies that the line hasn't been cut off earlier
(define (verify-single-solution resultList)
  (cond
    ((= 1 (get-list-len resultList)) (car resultList))
    (else -1)))


;;Loop for detecting the possibilty of horizontal lines within the grid
(define (place-next-n-aux grid numColumns n symbol otherSymbol currentM resultList)
  (cond
    ((>= currentM numColumns) (verify-single-solution resultList))
    ((equal? (get-value-base-on-index grid n currentM)  '_) (place-next-n-aux grid numColumns n symbol otherSymbol (+ currentM 1) (cons (list n currentM) resultList)))
    ((equal? (get-value-base-on-index grid n currentM) otherSymbol) -1)
    (else (place-next-n-aux grid numColumns n symbol otherSymbol (+ currentM 1) resultList))))

;;Gets the n and m coordinates for placing a symbol to complete a horizontal line
(define (place-next-n grid numColumns n symbol otherSymbol)
  (place-next-n-aux grid numColumns n symbol otherSymbol 0 '()))

;;Loop for detecting the possibility of vertical lines within the grid
(define (place-next-m-aux grid numRows m symbol otherSymbol currentN resultList)
  (cond
    ((>= currentN numRows) (verify-single-solution resultList))
    ((equal? (get-value-base-on-index grid currentN m) '_) (place-next-m-aux grid numRows m symbol otherSymbol (+ currentN 1) (cons (list currentN m) resultList)))
    ((equal? (get-value-base-on-index grid currentN m) otherSymbol) -1)
    (else (place-next-m-aux grid numRows m symbol otherSymbol (+ currentN 1) resultList))))

;;Gets the n and m coordinates for placing a symbol to complete a vertical line
(define (place-next-m grid numRows m symbol otherSymbol)
  (place-next-m-aux grid numRows m symbol otherSymbol 0 '()))
  

;;Gets the opposite symbol
(define (get-other-symbol symbol)
  (cond
    ((equal? symbol 'x) 'o)
    ((equal? symbol 'o) 'x)))


;;Gets the n and m coordinates for a given move by the computer
(define (get-move grid numRows numColumns symbol start finish)
  (cond
    ((= (car start) (car finish)) (place-next-n grid numColumns (car start) symbol (get-other-symbol symbol)))
    ((= (cadr start) (cadr finish)) (place-next-m grid numRows (cadr start) symbol (get-other-symbol symbol)))
    (else -1)))

;;Helper function for storing values in recursive calls for get-solution-for-symbol
(define (get-solution-for-symbol-aux-helper grid numRows numColumns symbol origin possiblePairsList lastResult)
  (cond
    ((and (null? possiblePairsList) (equal? -1 lastResult)) -1)
    ((not (equal? -1 lastResult)) lastResult)
    (else (get-solution-for-symbol-aux-helper grid numRows numColumns symbol origin (cdr possiblePairsList) (get-move grid numRows numColumns symbol origin (car possiblePairsList))))))
    
  

;;Helper function for storing values in recursive calls for get-solution-for-symbol
(define (get-solution-for-symbol-mid-helper grid  numRows numColumns symbol originList  pairsInfo)
  (get-solution-for-symbol-aux grid numRows numColumns symbol (cdr originList) (cons (car originList) (cadr pairsInfo))
                                (get-solution-for-symbol-aux-helper grid numRows numColumns symbol (car originList) (car (get-pairs grid (car originList) symbol (cons (car originList) (cadr pairsInfo)))) -1)))


;;Loop for finding solution to the grid given an specific symbol
(define (get-solution-for-symbol-aux grid numRows numColumns symbol originList prevOriginList lastResult)
  (cond
    ((null? originList) -1)
    ((not (equal? -1 lastResult)) lastResult)
    (else (get-solution-for-symbol-mid-helper grid numRows numColumns symbol originList (get-pairs grid (car originList) symbol prevOriginList)))))
  


;;Finds the solution to a grid given itself, the number of rows and columns and the symbol
(define (get-solution-for-symbol grid numRows numColumns symbol)
  (get-solution-for-symbol-aux grid numRows numColumns symbol (car (get-pairs-without-origin grid symbol)) '() -1))

;;Loops that finds an open position within the grid
(define (computer-random-attack-aux grid numRows numColumns result)
  (cond
    ((equal? (get-value-base-on-index grid (car result) (cadr result)) '_) result)
    (else (computer-random-attack grid numRows numColumns))))

;;Randomizes a response by the computer
(define (computer-random-attack grid numRows numColumns)
  (computer-random-attack-aux grid numRows numColumns (list (random numRows) (random numColumns))))
  

;;Determines whether or not an attack is possible
(define (computer-attack-aux grid numRows numColumns result)
  (cond
    ((equal? -1 result) (computer-random-attack grid numRows numColumns))
    (else result)))

;;Gets a attack move by the computer
(define (computer-attack grid numRows numColumns)
  (computer-attack-aux grid numRows numColumns (get-solution-for-symbol grid numRows numColumns 'o)))

;;Loop that determines whether or not computer is losing
(define (computer-counter-aux grid numRows numColumns result)
  (cond
    ((equal? -1 result) (computer-attack grid numRows numColumns))
    (else result)))

;;Gets a counter move by the computer to avoid losing
(define (computer-counter grid numRows numColumns)
  (computer-counter-aux grid numRows numColumns (get-solution-for-symbol grid numRows numColumns 'x)))
  
  

;;Gets the next possible state of the grid
(define (get-computer-next-move grid numRows numColumns)
  (computer-counter grid numRows numColumns))
  


;;Test cases
;;(get-value-base-on-index (list '(1 2 3) '(4 5 6) '(7 8 9)) 1 1)
;;(get-move (list (list 'x '_ '_) (list 'x '_ '_) (list 'x '_ '_)) 3 3 'x '(0 0) '(2 0))
;;(get-solution-for-symbol (list (list 'x '_ '_) (list 'o 'o '_) (list 'x '_ '_)) 3 3 'o)
;;(get-pairs-without-origin (list (list 'x '_ '_) (list 'x '_ '_) (list '_ '_ '_)) 'x)
;;(get-computer-next-move (list (list 'x '_ '_ '_) (list 'o '_ '_ '_) (list 'x '_ '_ '_) (list '_ '_ '_ '_)) 4 4)

(provide (all-defined-out))