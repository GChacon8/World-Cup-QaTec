#lang racket
(require racket/gui/base)
(require "algoritmo-genetico.rkt")

;(nueva-generacion '((1 1 3 0 5 1) (1 2 7 8 9 6) (1 3 8 0 1 5) (1 4 0 7 10 6) (2 5 7 10 1 0) (2 6 4 0 0 4) (2 7 5 8 4 1) (3 8 2 5 3 3) (3 9 6 1 5 7) (3 10 1 5 10 3) (0 11 4 2 6 9)) 1)


(define generacion2Vieja (nueva-generacion '((1 1 3 0 5 1) (1 2 7 8 9 6) (1 3 8 0 1 5) (1 4 0 7 10 6) (2 5 7 10 1 0) (2 6 4 0 0 4) (2 7 5 8 4 1) (3 8 2 5 3 3) (3 9 6 1 5 7) (3 10 1 5 10 3) (0 11 4 2 6 9)) 1))
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
;(define listaX '())
(define defensa 100)

(define playerVel 10)
(define ballVel 10)

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
(define (checkPlayer)
  (cond [(<= player1Y 100)
         (set! playerVel (* playerVel -1))]
        [(>= player1Y 600)
         (set! playerVel (* playerVel -1))]
        [(<= player2Y 100)
         (set! playerVel (* playerVel -1))]
        [(>= player2Y 600)
         (set! playerVel (* playerVel -1))]
        [(<= player3Y 100)
         (set! playerVel (* playerVel -1))]
        [(>= player3Y 600)
         (set! playerVel (* playerVel -1))]
        ))
(define (checkBall)
  (cond [(<= ballX 0)
         (set! ballVel (* ballVel -1))]
        [(>= ballX 900)
         (set! ballVel (* ballVel -1))])
  )
(new button% [parent frame]
             [label "Update"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)                         
                         (set! team1Score (+ team1Score 1))
                         (set! team2Score (+ team2Score 2))
                         
                         (let ([i 0])
                           (while (< i  500)
                                  (sleep/yield 0.005)
                                  (thread checkPlayer)
                                  (thread checkBall)
                                  (set! ballX (+ ballX ballVel))
                                  (set! player1Y (+ player1Y playerVel))
                                  (set! player2Y (+ player2Y playerVel))
                                  (set! player3Y (+ player3Y playerVel))
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
        (send dc set-pen "black" 1 'solid)
        (send dc draw-rectangle 0 0 999 800)
        (send (send canvas get-dc) erase)
        (define listaX (colocar '((1 1 3 0 5 1) (1 2 7 8 9 6) (1 3 8 0 1 5) (1 4 0 7 10 6) (2 5 7 10 1 0) (2 6 4 0 0 4) (2 7 5 8 4 1) (3 8 2 5 3 3) (3 9 6 1 5 7) (3 10 1 5 10 3) (0 11 4 2 6 9)) '()))
        (set! defensa 100)
        (send (send canvas get-dc) draw-rectangle 0 0 999 800)
        (send (send canvas get-dc) draw-rectangle (list-ref listaX 0) player1Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref listaX 1) player2Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref listaX 2) player3Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref listaX 3) player4Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref listaX 4) player5Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref listaX 5) player6Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref listaX 6) player7Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref listaX 7) player8Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref listaX 8) player9Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref listaX 9) player10Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref listaX 10) player11Y 30 30)
        
        (send dc draw-text "Jugador 1" (- (list-ref listaX 0) 20) (+ player1Y 35))
        
        (send (send canvas get-dc) draw-ellipse ballX ballY 20 20)
        )]
        ))


(define (colocar equipo listaX)
  (cond [(null? equipo)
         listaX
         ]
        [
         (cond [(= (car (car equipo)) 1) 
         (set! defensa (+ defensa 50))
         (cons listaX defensa)
         (colocar (cdr equipo) (append listaX (list defensa)))]
               [(= (car (car equipo)) 2) 
                (set! defensa (+ defensa 50))
               
                (colocar (cdr equipo) (append listaX (list defensa)))]
               [(= (car (car equipo)) 3)
                (set! defensa (+ defensa 50))
               
                (colocar (cdr equipo) (append listaX (list defensa)))]
               [(= (car (car equipo)) 0)
                (colocar (cdr equipo) (append listaX (list 50)))]
               )
         ]))

; Show the frame by calling its show method
(send frame show #t)