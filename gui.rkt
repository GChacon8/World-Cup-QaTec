#lang racket
(require racket/gui/base)

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
                         ;(send my-canvas% draw-bitmap)
                         
                         ;(set! ballY (random 500))
                         (set! player1Y (random 300))
                         (set! player1X (random 300))
                         (set! player2Y (random 300))
                         (set! player2X (random 300))
                         
                         (set! team1Score (+ team1Score 1))
                         (set! team2Score (+ team2Score 2))
                         ;(send canvas get-dc)
                         (let ([i 0])
                           (while (< i  100)
                                  (set! ballX (+ ballX 1))
                                  (set! i (add1 i))))
                         (send canvas refresh-now)
                         )])
 
; Make a canvas that handles events in the frame
(define canvas
(new canvas% 
     [parent frame]
     [paint-callback
      (lambda (my-canvas dc)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Desktop/TEC/4 Semestre/Lenguajes/Racket/images/field.jpg") 0 0)        
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Desktop/TEC/4 Semestre/Lenguajes/Racket/images/ball1.png" #:backing-scale 15) ballX ballY)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Desktop/TEC/4 Semestre/Lenguajes/Racket/images/player.png" #:backing-scale 10) (+ player1X 852) player1Y)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Desktop/TEC/4 Semestre/Lenguajes/Racket/images/player.png" #:backing-scale 10) (+ player1X 722) player2Y)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Desktop/TEC/4 Semestre/Lenguajes/Racket/images/player.png" #:backing-scale 10) (+ player1X 722) player3Y)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Desktop/TEC/4 Semestre/Lenguajes/Racket/images/player.png" #:backing-scale 10) (+ player1X 722) player4Y)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Desktop/TEC/4 Semestre/Lenguajes/Racket/images/player2.png" #:backing-scale 10) player1X player1Y)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Desktop/TEC/4 Semestre/Lenguajes/Racket/images/player2.png" #:backing-scale 10) player2X player2Y)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Desktop/TEC/4 Semestre/Lenguajes/Racket/images/player2.png" #:backing-scale 10) player3X player3Y)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Desktop/TEC/4 Semestre/Lenguajes/Racket/images/player2.png" #:backing-scale 10) player4X player4Y)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Desktop/TEC/4 Semestre/Lenguajes/Racket/images/player2.png" #:backing-scale 10) player5X player5Y)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Desktop/TEC/4 Semestre/Lenguajes/Racket/images/player2.png" #:backing-scale 10) player6X player6Y)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Desktop/TEC/4 Semestre/Lenguajes/Racket/images/player2.png" #:backing-scale 10) player7X player7Y)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Desktop/TEC/4 Semestre/Lenguajes/Racket/images/player2.png" #:backing-scale 10) player8X player8Y)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Desktop/TEC/4 Semestre/Lenguajes/Racket/images/player2.png" #:backing-scale 10) player9X player9Y)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Desktop/TEC/4 Semestre/Lenguajes/Racket/images/player2.png" #:backing-scale 10) player10X player10Y)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Desktop/TEC/4 Semestre/Lenguajes/Racket/images/player2.png" #:backing-scale 10) player11X player11Y)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Desktop/TEC/4 Semestre/Lenguajes/Racket/images/redStripe.png" #:backing-scale 10) 400 0)
        (send dc set-font fontScore)
        (send dc draw-text (string-append "Team 1:" (number->string team1Score)) 430 15)
        (send dc draw-text (string-append "Team 2:" (number->string team2Score)) 515 15)
        (send dc set-font font)
        (send dc draw-text "Jugador 1" player1X (+ player1Y 60))
        (send dc draw-text "Jugador 2" player2X (+ player2Y 60))
        (send dc draw-text "Jugador 3" player3X (+ player3Y 60))
        (send dc draw-text "Jugador 4" player4X (+ player4Y 60))
        (send dc draw-text "Jugador 5" player5X (+ player5Y 60))
        (send dc draw-text "Jugador 6" player6X (+ player6Y 60))
        (send dc draw-text "Jugador 7" player7X (+ player7Y 60))
        (send dc draw-text "Jugador 8" player8X (+ player8Y 60))
        (send dc draw-text "Jugador 9" player9X (+ player9Y 60))
        (send dc draw-text "Jugador 10" player10X (+ player10Y 60))
        (send dc draw-text "Jugador 11" player11X (+ player11Y 60))
        )]))

; Show the frame by calling its show method
(send frame show #t)