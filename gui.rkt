#lang racket/gui

(require "logic-utilities.rkt")

;; Global Variables
(define global-rows 10)
(define global-columns 10)


(define user-move #\X)
(define computer-move #\O)
(define new-game #t)
(define current-move user-move)

(define game-grid (get-matrix global-rows global-columns "_"))

;; Window to play the game
(define game-frame (new frame%
                   [label "Tic Tac Racket"]
                   [width 600]
                   [height 600]))

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
      ; handles Drawing of char X or O according to color
      (let ((dc (get-dc)))        
        (let-values (((x y) (send this get-size)))
          (send dc set-text-foreground color)
          (send dc set-font (make-object font% (+ 60 (* -2 (+ global-rows global-columns))) 'default))
          (send dc draw-text (string character) (/ (- x (+ 60 (* -2 (+ global-rows global-columns)))) 2) (/ (- y (+ 93 (* -3 (+ global-rows global-columns)))) 2)))))
   
    (define/public (set-color c)
      (set! color c))
   
    (define/public (set-character char)
      (set! character char))
   
    (super-new)))

;; Panes
;; Main Pane
(define main-pane (new vertical-pane%
                      [parent game-frame]
                      [vert-margin 5]
                      [horiz-margin 5]
                      [spacing 5]))

;; Creates and links the game grid panes to hold the canvases
(define (get-panes row)
  (cond
    ((>= row global-rows) '())
    (else (cons (new horizontal-pane% [parent main-pane] [spacing 5])
                (get-panes (+ row 1))))))

;; Creates and links the game grid canvases to the panes
(define (get-panes-canvases panes row)
  (cond
    ((null? panes) '())
    (else (cons (get-pane-canvases (car panes) row 0) (get-panes-canvases (cdr panes) (+ row 1))))))

;; Creates and links the canvases to a single pane
(define (get-pane-canvases pane row column)
  (cond
    ((>= column global-columns) '())
    (else (cons (new canvas-box%
                   [row row]
                   [column column]
                   [parent pane])
                (get-pane-canvases pane row (+ column 1))))))


(define panes (get-panes 0))
(define canvases (get-panes-canvases panes 0))

;; Window to input the game grid's rows and columns
(define input-frame (new frame%
                      [label "Tic Tact Racket - Dimensions"]
                      [width 400]
                      [height 200]))

;; Input dimensions explanatory message
(define message (new message%
                     (parent input-frame)
                     (label "Tic Tac Racket grid dimensions:")
                     [vert-margin 15]))

;; List-control to select the number of rows
(define rows-input (new choice%
                     (label "Rows       ")
                     (parent input-frame)
                     (choices (list "3" "4" "5" "6" "7" "8" "9" "10"))))

;; List-control to select the number of columns
(define columns-input (new choice%
                        (label "Columns ")
                        (parent input-frame)
                        (choices (list "3" "4" "5" "6" "7" "8" "9" "10"))))

;; Button to set the grid dimensions and start playing the game
(define play-btn (new button%
                   [parent input-frame]
                   [label "Play"]
                   [min-width 75]
                   [min-height 50]
                   [vert-margin 15]
                   [callback (λ (b e) 
                     (set! global-rows (string->number (send rows-input get-string-selection)))
                     (set! global-columns (string->number (send columns-input get-string-selection)))
                     ; set game-grid
                     ; set panes
                     ; set canvases
                     
                     (send game-frame show #t)
                     (send input-frame show #f) )]))

(send game-frame center 'both)
(send input-frame center 'both)

;(send input-frame show #t)
(send game-frame show #t)