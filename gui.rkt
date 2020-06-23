#lang racket/gui

(require "logic-utilities.rkt")
(require "draw.rkt")
(require "winner.rkt")
(require "try-to-win.rkt")
(require "empty.rkt")

;; Global Variables
(define global-rows 3)
(define global-columns 3)
(define blue (make-object color% 102 140 255 1))
(define red (make-object color% 255 102 102 1))

(define game-grid (get-matrix global-rows global-columns '_))

;; Window to play the game
(define game-frame (new frame%
                   [label "Tic Tac Racket"]
                   [width 600]
                   [height 600]))

;; Canvas-box class
(define canvas-box%
  (class canvas%
    (init-field [character #\space]
                [row 0]
                [column 0]
                [position 0]
                [color blue])
    (inherit get-dc)

    ;; Event listener
    (define/override (on-event e)
      ; mouse left button clicked
      (when (and (equal? (send e get-event-type) 'left-down) (equal? character #\space))
        
        ; process player move
        (set! game-grid (matrix-set-at game-grid row column 'x))
        (send this set-character #\x)
        (send this refresh)
        (displayln " ")
        (displayln game-grid)
        (sleep/yield 0.1)

        (when (draw? game-grid)
            (message-box "Draw" "It's a draw!")
            (reset-game))

        (when (winner? game-grid)
            (message-box "Win" "You won!")
            (reset-game))

        (when (and (not (draw? game-grid)) (not (winner? game-grid)) (not (empty? game-grid)))
          (set-computer-move (get-computer-next-move game-grid global-rows global-columns)))
            
        (displayln game-grid)
        
        (when (winner? game-grid)
            (message-box "Win" "The computer won!")
            (reset-game))
        
        (when (draw? game-grid)
            (message-box "Draw" "It's a draw!")
            (reset-game))
        
        (send (get-dc) clear)
        (send this refresh)))

    ;; Draw 
    (define/override  (on-paint)
      (let ((dc (get-dc)))        
        (let-values (((x y) (send this get-size)))
          (send dc set-text-foreground (get-canvas-color character))
          (send dc set-font (make-object font% (+ 60 (* -2 (+ global-rows global-columns))) 'default))
          (send dc draw-text (string character) (/ (- x (+ 60 (* -2 (+ global-rows global-columns)))) 2) (/ (- y (+ 93 (* -3 (+ global-rows global-columns)))) 2)))))
   
    (define/public (set-color c)
      (set! color c))
   
    (define/public (set-character char)
      (set! character char))
   
    (super-new)))

;; Returns the color associated with the playable character
(define (get-canvas-color character)
  (cond ((equal? character #\x) blue)
        (else red)))

;; Panes
;; Main Pane
(define main-pane (new vertical-pane% [parent game-frame] [vert-margin 5] [horiz-margin 5] [spacing 5]))

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
    (else (append (get-pane-canvases (car panes) row 0) (get-panes-canvases (cdr panes) (+ row 1))))))

;; Creates and links the canvases to a single pane
(define (get-pane-canvases pane row column)
  (cond
    ((>= column global-columns) '())
    (else (cons (new canvas-box%
                   [row row]
                   [column column]
                   [parent pane])
                (get-pane-canvases pane row (+ column 1))))))


; Declare default panes and its canvases
(define panes (get-panes 0))
(define canvases (get-panes-canvases panes 0))

;; Resets all game setup for a new game
(define (reset-game)
  ; reset canvas characters
  (for [(canvas  canvases)]
      (send canvas set-character #\Space)
      (send canvas set-color (make-object color% "gray"))
      (send canvas refresh))
  ; reset game grid
  (set! game-grid (get-matrix global-rows global-columns '_))
  (send game-frame refresh))

;; Sets a computer move
(define (set-computer-move row-and-column)
  (displayln row-and-column)
  (set! game-grid (matrix-set-at game-grid (car row-and-column) (cadr row-and-column) 'o))
  (send (list-ref canvases (+ (cadr row-and-column) (* (car row-and-column) global-columns))) set-character #\o)
  (send (list-ref canvases (+ (cadr row-and-column) (* (car row-and-column) global-columns))) refresh))

;;----------------------- Input Frame -------------------------

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
                     ; Overriding the default game-frame contents
                     (set! global-rows (string->number (send rows-input get-string-selection)))
                     (set! global-columns (string->number (send columns-input get-string-selection)))
                     (set! game-frame (new frame% [label "Tic Tac Racket"] [width 600] [height 600]))
                     (send game-frame center 'both)
                     (set! game-grid (get-matrix global-rows global-columns '_))
                     (set! main-pane (new vertical-pane% [parent game-frame] [vert-margin 5] [horiz-margin 5] [spacing 5]))
                     (set! panes (get-panes 0))
                     (set! canvases (get-panes-canvases panes 0))

                     ; Switching from input-frame to game-frame
                     (send game-frame show #t)
                     (send input-frame show #f) )]))

; Center both frames as default
(send game-frame center 'both)
(send input-frame center 'both)

;(send input-frame show #t)
(send game-frame show #t)