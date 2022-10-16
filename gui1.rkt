#lang racket
(require racket/gui/base)
;(require "algortimo-genetico.rkt")

;(nueva-generacion '((1 1 3 0 5 1) (1 2 7 8 9 6) (1 3 8 0 1 5) (1 4 0 7 10 6) (2 5 7 10 1 0) (2 6 4 0 0 4) (2 7 5 8 4 1) (3 8 2 5 3 3) (3 9 6 1 5 7) (3 10 1 5 10 3) (0 11 4 2 6 9)) 1)

;Define variables
(define ballX 492)
(define ballY 365)
(define player1X 50)
(define player1Y 340)
(define player2X 180)
(define player2Y 340)
(define player3X 180)
(define player3Y 500)
(define player4X 180)
(define player4Y 180)
(define player5X 400)
(define player5Y 100)
(define player6X 400)
(define player6Y 600)
(define player7X 300)
(define player7Y 100)
(define player8X 300)
(define player8Y 600)
(define player9X 300)
(define player9Y 340)
(define player10X 430)
(define player10Y 380)
(define player11X 430)
(define player11Y 280)

(define team1Score 0)
(define team2Score 0)

(define green (make-object color% 0 255 0 1))

(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))



(new style-list%)
(define font (make-object font% 10 'modern 'normal))
(define fontScore (make-object font% 9 'modern 'normal 'bold))
; Make a frame by instantiating the frame% class
(define frame (new frame%
                   [label "WCQTec"]
                   [width 999]
                   [height 800]))
  
; Make a button in the frame
(new button% [parent frame]
             [label "Update"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)                         
                         (set! team1Score (+ team1Score 1))
                         (set! team2Score (+ team2Score 2))
                         (send (send canvas get-dc) draw-rectangle 0 0 999 800)
                         (let ([i 0])
                           (while (< i  100)
                                  (sleep/yield 0.005)
                                  (set! ballX (+ ballX 3))
                                  (send canvas refresh-now)
                                  (set! i (add1 i))))
                         
                         (send canvas refresh-now)
                         
                         )])
 
; Make a canvas that handles events in the frame
(define canvas
(new canvas% 
     [parent frame]
     [paint-callback
      (lambda (my-canvas dc)
        (send dc set-brush "green" 'solid)
        (send dc set-pen "black" 3 'solid)
        (send dc draw-rectangle 0 0 999 800)
        (send (send canvas get-dc) erase)
        (send (send canvas get-dc) draw-rectangle 0 0 999 800)
        (send (send canvas get-dc) draw-rectangle player1X player1Y 30 30)
        (send (send canvas get-dc) draw-rectangle player2X player2Y 30 30)
        (send (send canvas get-dc) draw-rectangle player3X player3Y 30 30)
        (send (send canvas get-dc) draw-rectangle player4X player4Y 30 30)
        (send (send canvas get-dc) draw-rectangle player5X player5Y 30 30)
        (send (send canvas get-dc) draw-rectangle player6X player6Y 30 30)
        (send (send canvas get-dc) draw-rectangle player7X player7Y 30 30)
        (send (send canvas get-dc) draw-rectangle player8X player8Y 30 30)
        (send (send canvas get-dc) draw-rectangle player9X player9Y 30 30)
        (send (send canvas get-dc) draw-rectangle player10X player10Y 30 30)
        (send (send canvas get-dc) draw-rectangle player11X player11Y 30 30)
        (send dc draw-text "Jugador 1" (- player1X 20) (+ player1Y 35))
        
        (send (send canvas get-dc) draw-ellipse ballX ballY 20 20)
        )]
        ))

; Show the frame by calling its show method
(send frame show #t)