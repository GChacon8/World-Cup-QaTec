#lang racket
(require racket/gui/base)
(require "algoritmo-genetico.rkt")

;(define generacion2Vieja (nueva-generacion '((1 1 3 0 5 1) (1 2 7 8 9 6) (1 3 8 0 1 5) (1 4 0 7 10 6) (2 5 7 10 1 0) (2 6 4 0 0 4) (2 7 5 8 4 1) (3 8 2 5 3 3) (3 9 6 1 5 7) (3 10 1 5 10 3) (0 11 4 2 6 9)) 1))
;Posicion, numero jugador, fuerza, habilidad, velocidad y desplazamiento
;(display (list-ref generacion2 0))
;(define generacion2Nueva (agregarPosicion generacion2Vieja '() 4 3 3))

;(define pos1 (list-ref (list-ref generacion2Nueva 0) 0))
;(define num1 (list-ref (list-ref generacion2Nueva 0) 1))
;(define str1 (list-ref (list-ref generacion2Nueva 0) 2))
;(define skill1 (list-ref (list-ref generacion2Nueva 0) 3))
;(define speed1 (list-ref (list-ref generacion2Nueva 0) 4))
;(define movement1 (list-ref (list-ref generacion2Nueva 0) 5))
;(display generacion2Nueva)
;(display str1)

;Define variables
(define ballX 492)
(define ballY 365)
(define defensa 200)
(define medio 500)
(define delantero 700)
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
(define ballVelX 3)
(define ballVelY 3)

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
                        
                         ;(drawField (send canvas get-dc))
                         (set! team1Score (+ team1Score 1))
                         (set! team2Score (+ team2Score 2))
                         ;(drawPlayer (send canvas get-dc) player1X player1Y 1)
                         ;(send canvas get-dc)
                         
                         ;(clean-canvas)
                         ;(colocar '((1 1 3 0 5 1) (1 2 7 8 9 6) (1 3 8 0 1 5) (1 4 0 7 10 6) (2 5 7 10 1 0) (2 6 4 0 0 4) (2 7 5 8 4 1) (3 8 2 5 3 3) (3 9 6 1 5 7) (3 10 1 5 10 3) (0 11 4 2 6 9)))
                         ;(thread prueba)
                         (thread moverBola)
                         (thread moverJugador)
                         ;(moverBola)
                         ;(send canvas refresh-now)
                         
                         )])

(define (moverBola)
  (let ([i 0])
    (while (< i  1000)
           (cond
             ((< ballX 0) (set! ballVelX 3))
             ((> ballX 965) (set! ballVelX -3))
             ((< ballY 0) (set! ballVelY 3))
             ((> ballY 715) (set! ballVelY -3)))
           (set! ballX (+ ballX ballVelX))
           (set! ballY (+ ballY ballVelY))
           (drawBall (send canvas get-dc) ballX ballY)
           (set! i (add1 i)))))


(define (moverJugador)
  (define y 0)
  (let ([i 0])
    (while (< i 1000)
           (set! y (+ y 2))
           (drawPlayer (send canvas get-dc) 100 y 1)
           )))

(define (colocar equipo)
  (define jugador (car equipo))
  (cond [(null? equipo)
         0
         ]
        [
         (cond [(= (car jugador) 1)
         (drawPlayer (send canvas get-dc) defensa (random 500) (cadr jugador))
         (set! defensa (+ defensa 50))
         (colocar (cdr equipo))]
               [(= (car jugador) 2)
                (drawPlayer (send canvas get-dc) medio (random 500) (cadr jugador))
                (set! medio (+ medio 50))
                (colocar (cdr equipo))]
               [(= (car jugador) 3)
                (drawPlayer (send canvas get-dc)delantero (random 500) (cadr jugador))
                (set! delantero (+ delantero 50))
                (colocar (cdr equipo))]
               [(= (car jugador) 0)
                (drawPlayer (send canvas get-dc)100 250 (cadr jugador))
                ])
         ]))

(define (prueba)
  (colocar '((1 1 3 0 5 1) (1 2 7 8 9 6) (1 3 8 0 1 5) (1 4 0 7 10 6) (2 5 7 10 1 0) (2 6 4 0 0 4) (2 7 5 8 4 1) (3 8 2 5 3 3) (3 9 6 1 5 7) (3 10 1 5 10 3) (0 11 4 2 6 9)))
                         )

; Make a canvas that handles events in the frame
(define canvas
(new canvas% 
     [parent frame]
     [paint-callback
      (lambda (my-canvas dc)
        (send dc draw-bitmap (read-bitmap "./images/field.jpg") 0 0))]
     ))

(define (drawPlayer dc x y num)
  (send dc draw-bitmap (read-bitmap "./images/player2.png" #:backing-scale 10) x y)
  (send dc draw-text (string-append "Jugador" (number->string num)) x (+ y 60)))

(define (drawField dc)
  (send dc draw-bitmap (read-bitmap "./images/field.jpg") 0 0))

(define (drawBall dc x y)
  (send dc draw-bitmap (read-bitmap "./images/ball1.png" #:backing-scale 10) x y)
  (send canvas on-paint)
  )

(define (clean-canvas)
  (send (send canvas get-dc) erase)
  (send canvas on-paint)
  )

; Show the frame by calling its show method
(send frame show #t)