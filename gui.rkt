#lang racket/gui

;; Global Variables
(define rows 3)
(define columns 3)

(define user-move #\X)
(define computer-move #\O)
(define new-game #t)
(define current-move user-move)


; Creates a n lenght list with a given value
(define (getList n value)
  (cond ((< n 1) '())
        (else (cons value (getList (- n 1) value)))))
; Creates a matrix of given rows and columns with a given value
(define (getMatrix rows columns value)
  (cond ((< rows 1) '())
        (else (cons (getList columns value) (getMatrix (- rows 1) columns value)))))

; Returns the matrix with the value setted at the given row and column
(define (matrix-set-at matrix row column value)
  (cond ((null? matrix) '())
        ((equal? row 0) (cons (list-set-at (car matrix) column value) (matrix-set-at (cdr matrix) (- row 1) column value)))
        (cons (cons (car matrix) (matrix-set-at (cdr matrix) (- row 1) column value)))))

; Returns the list with the value setted at the given index
(define (list-set-at list index value)
  (cond ((null? list) '())
        ((equal? index 0) (cons value (list-set-at (cdr list) (- index 1) value)))
        (else (cons (car list) (list-set-at (cdr list) (- index 1) value)))))

(define game-grid (getMatrix rows columns "_"))

;; Frame 
(define frame (new frame%
                   [label "Tic Tac Racket"]
                   [width 400]
                   [height 400]))

;; Panes
;; Main Pane
(define main-pane (new vertical-pane%
                      [parent frame]
                      [vert-margin 5]
                      [horiz-margin 5]
                      [spacing 5]))
;; Child Panes
(define upper-pane (new horizontal-pane%
                        [parent main-pane]
                        [spacing 5]))

(define middle-pane
  (new horizontal-pane%
       [parent main-pane]
       [spacing 5]))
(define lower-pane
  (new horizontal-pane%
       [parent main-pane]
       [spacing 5]))

;; Draw Canvas
;; Canvas-box class
(define canvas-box%
  (class canvas%
    (init-field [character #\Space]
                [row 0]
                [column 0]
                [position 0]
                [color (make-object color% "black")])
   
    (inherit get-dc)

    (define/override (on-event e)
      ; mouse left button clicked
      (when (equal? (send e get-event-type) 'left-down)


        (set! game-grid (matrix-set-at game-grid row column "x"))
        (displayln game-grid)
        
        (send this set-character #\X)
        (send this refresh)
 
        (let ((dc (get-dc)))
          (send dc clear))
        (send this refresh)))
   
    (define/override  (on-paint)
      ;; Handles Drawing of char X or O according to color
      (let ((dc (get-dc)))        
        (let-values (((x y) (send this get-size)))
          (send dc set-text-foreground color)
          (send dc set-font (make-object font% 50 'default))
          (send dc draw-text (string character) (/ (- x 50) 2) (/ (- y 75) 2)))))
   
    (define/public (set-color c)
      (set! color c))
   
    (define/public (set-character char)
      (set! character char))
   
    (super-new)))

;; Instance canvases
(define canvases
  (let ((canvas-one (new canvas-box%
                         [position 0]
                         [row 0]
                         [column 0]
                         [parent upper-pane]))
        (canvas-two (new canvas-box%
                         [position 1]
                         [row 0]
                         [column 1]
                         [parent upper-pane]))
        (canvas-three (new canvas-box%
                           [position 2]
                           [row 0]
                           [column 2]
                           [parent upper-pane]))
        (canvas-four (new canvas-box%
                          [position 3]
                          [row 1]
                          [column 0]
                          [parent middle-pane]))
        (canvas-five (new canvas-box%
                          [position 4]
                          [row 1]
                          [column 1]
                          [parent middle-pane]))
        (canvas-six (new canvas-box%
                         [position 5]
                         [row 1]
                         [column 2]
                         [parent middle-pane]))
        (canvas-seven (new canvas-box%
                           [position 6]
                           [row 2]
                           [column 0]
                           [parent lower-pane]))
        (canvas-eight (new canvas-box%
                           [position 7]
                           [row 2]
                           [column 1]
                           [parent lower-pane]))
        (canvas-nine (new canvas-box%
                          [position 8]
                          [row 2]
                          [column 2]
                          [parent lower-pane])))  
    (list canvas-one canvas-two canvas-three
          canvas-four canvas-five canvas-six
          canvas-seven canvas-eight canvas-nine)))

; method get-parent-panel

(send frame center 'both) ; center frame
(send frame show #t)  ; show frame