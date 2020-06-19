#lang racket/gui

(require "logic-utilities.rkt")

;; Global Variables
(define rows 3)
(define columns 3)


(define user-move #\X)
(define computer-move #\O)
(define new-game #t)
(define current-move user-move)

(define game-grid (get-matrix 3 3 "_"))

;; Window to play the game
(define game-frame (new frame%
                   [label "Tic Tac Racket"]
                   [width 400]
                   [height 400]))

;; Panes
;; Main Pane
(define main-pane (new vertical-pane%
                      [parent game-frame]
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
      ; handles Drawing of char X or O according to color
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
                     (set! rows (string->number (send rows-input get-string-selection)))
                     (set! columns (string->number (send columns-input get-string-selection)))
                     ; set game-grid
                     ; set panes
                     ; set canvases
                     
                     (send game-frame show #t)
                     (send input-frame show #f) )]))

(send game-frame center 'both)
(send input-frame center 'both)

;(send input-frame show #t)
(send game-frame show #t)