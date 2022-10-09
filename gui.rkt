#lang racket
(require racket/gui/base)

(define ballX 492)
(define ballY 365)
(define player1X 100)
(define player1Y 100)
(define player2X 200)
(define player2Y 100)
(define player3X 300)
(define player3Y 100)
(define player4X 400)
(define player4Y 100)
(define team1Score 0)
(define team2Score 0)

(new style-list%)
(define font (make-object font% 10 'modern 'normal))
(define fontScore (make-object font% 9 'modern 'normal))
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
                         ;(send my-canvas% draw-bitmap)
                         (set! ballX (random 500))
                         (set! ballY (random 500))
                         (set! player1Y (random 300))
                         (set! player1X (random 300))
                         (set! player2Y (random 300))
                         (set! player2X (random 300))
                         (set! team1Score (+ team1Score 1))
                         ;(send canvas get-dc)
                         (send canvas refresh-now)
                         )])
 
 
; Make a canvas that handles events in the frame
(define canvas
(new canvas% 
     [parent frame]
     [paint-callback
      (lambda (my-canvas dc)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Downloads/field.jpg") 0 0)        
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Downloads/ball1.png" #:backing-scale 15) ballX ballY)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Downloads/player.png" #:backing-scale 10) player1X player1Y)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Downloads/player2.png" #:backing-scale 10) player2X player2Y)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Downloads/redStripe.png" #:backing-scale 10) 400 0)
        (send dc set-font fontScore)
        (send dc draw-text (string-append "Team 1:" (number->string team1Score)) 430 15)
        (send dc draw-text "Team 2:0" 515 15)
        (send dc set-font font)
        (send dc draw-text "Jugador 1" player1X (+ player1Y 60))
        (send dc draw-text "Jugador2" player2X (+ player2Y 60))
        )]))


; Show the frame by calling its show method
(send frame show #t)