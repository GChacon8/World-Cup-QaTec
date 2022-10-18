#lang racket
(require racket/gui/base)
(require "algoritmo-genetico.rkt")

;(nueva-generacion '((1 1 3 0 5 1) (1 2 7 8 9 6) (1 3 8 0 1 5) (1 4 0 7 10 6) (2 5 7 10 1 0) (2 6 4 0 0 4) (2 7 5 8 4 1) (3 8 2 5 3 3) (3 9 6 1 5 7) (3 10 1 5 10 3) (0 11 4 2 6 9)) 1)


(define generacion2Vieja (nueva-generacion '((1 1 3 0 5 1) (1 2 7 8 9 6) (1 3 8 0 1 5) (1 4 0 7 10 6) (2 5 7 10 1 0) (2 6 4 0 0 4) (2 7 5 8 4 1) (3 8 2 5 3 3) (3 9 6 1 5 7) (3 10 1 5 10 3) (0 11 4 2 6 9)) 1))
;(define jugador1 (new jugador% (posicion 1) (numero 1) (fuerza 1) (habilidad 1) (velocidad 7) (desplazamiento 1)))

;Define variables
(define jugadores (agregarPosicion (crear_equipo '() 1) '() 5 3 2))
(define ballX 492)
(define ballY 365)
(define player1X 50)
(define player1Y 300)
(define player2X 180)
(define player2Y 300)
(define player3X 180)
(define player3Y 300)
(define player4X 180)
(define player4Y 300)
(define player5X 400)
(define player5Y 300)
(define player6X 400)
(define player6Y 300)
(define player7X 300)
(define player7Y 300)
(define player8X 300)
(define player8Y 300)
(define player9X 300)
(define player9Y 300)
(define player10X 430)
(define player10Y 300)
(define player11X 430)
(define player11Y 300)
(define defensa 50)
(define medio 350)
(define delantero 550)
(define playerVel1 (list-ref (list-ref jugadores 0) 3))
(define playerVel2 (list-ref (list-ref jugadores 1) 3))
(define playerVel3 (list-ref (list-ref jugadores 2) 3))
(define playerVel4 (list-ref (list-ref jugadores 3) 3))
(define playerVel5 (list-ref (list-ref jugadores 4) 3))
(define playerVel6 (list-ref (list-ref jugadores 5) 3))
(define playerVel7 (list-ref (list-ref jugadores 6) 3))
(define playerVel8 (list-ref (list-ref jugadores 7) 3))
(define playerVel9 (list-ref (list-ref jugadores 8) 3))
(define playerVel10 (list-ref (list-ref jugadores 9) 3))
(define playerVel11 (list-ref (list-ref jugadores 10) 3))

(define ballVelX 10)
(define ballVelY 10)

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
                   [width 1400]
                   [height 800]))
  
; Make a button in the frame
(define (checkPlayer)
  (cond [(< player1Y 100)
         (set! playerVel1 (abs playerVel1))]
        [(> player1Y 600)
         (set! playerVel1 (* (abs playerVel1) -1))])

  (cond [(< player2Y 100)
         (set! playerVel2 (abs playerVel2))]
        [(> player2Y 600)
         (set! playerVel2 (* (abs playerVel2) -1))])

  (cond [(< player3Y 100)
         (set! playerVel3 (abs playerVel3))]
        [(> player3Y 600)
         (set! playerVel3 (* (abs playerVel3) -1))])

  (cond [(< player4Y 100)
         (set! playerVel4 (abs playerVel4))]
        [(> player4Y 600)
         (set! playerVel4 (* (abs playerVel4) -1))])

  (cond [(< player5Y 100)
         (set! playerVel5 (abs playerVel5))]
        [(> player5Y 600)
         (set! playerVel5 (* (abs playerVel5) -1))])

  (cond [(< player6Y 100)
         (set! playerVel6 (abs playerVel6))]
        [(> player6Y 650)
         (set! playerVel6 (* (abs playerVel6) -1))])

  (cond [(< player7Y 100)
         (set! playerVel7 (abs playerVel7))]
        [(> player7Y 600)
         (set! playerVel7 (* (abs playerVel7) -1))])

  (cond [(< player8Y 100)
         (set! playerVel8 (abs playerVel8))]
        [(> player8Y 650)
         (set! playerVel8 (* (abs playerVel8) -1))])

  (cond [(< player9Y 100)
         (set! playerVel9 (abs playerVel9))]
        [(> player9Y 600)
         (set! playerVel9 (* (abs playerVel9) -1))])

  (cond [(< player10Y 100)
         (set! playerVel10 (abs playerVel10 ))]
        [(> player10Y 600)
         (set! playerVel10 (* (abs playerVel10) -1))])
  
  (cond [(< player11Y 100)
         (set! playerVel11 (abs playerVel11))]
        [(> player11Y 600)
         (set! playerVel11 (* (abs playerVel11) -1))])
  )

(define (colisionPrueba)
  (cond
    ((and (> (+ ballX 15) (+ player1X 10)) (< (- ballX 15) (+ player1X 40)) (> (+ ballY 15) (+ player1Y 10)) (< (- ballY 15) (+ player1Y 40))) (disparo))
    (else (quote 0))))

(define (disparo)
  (set! ballVelX 10)
  (set! ballVelY 0)
  (sleep 1)
  (set! ballVelX 0)
  (set! ballVelY 0)
  )
         

(define (checkBall)
  (cond [(< ballX 0)
         (set! ballVelX (abs (* ballVelX -1)))]
        [(> ballX 1370)
         (set! ballVelX (* (abs ballVelX) -1))])
  (cond [(< ballY 0)
         (set! ballVelY (abs (* ballVelY -1)))]
        [(> ballY 720)
         (set! ballVelY (* (abs ballVelY) -1))])
  )
(new button% [parent frame]
             [label "Update"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)                         
                         (set! team1Score (+ team1Score 1))
                         (set! team2Score (+ team2Score 2))
                         
                         
                         (let ([i 0])
                           (while (< i 100)
                                  (sleep/yield 0.005)
                                  (thread checkPlayer)
                                  (thread checkBall)
                                  (thread colisionPrueba)
                                  (set! ballX (+ ballX ballVelX))
                                  (set! ballY (+ ballY ballVelY))
                                  (set! player1Y (+ player1Y playerVel1))
                                  (set! player2Y (+ player2Y playerVel2))
                                  (set! player3Y (+ player3Y playerVel3))
                                  (set! player4Y (+ player4Y playerVel4))
                                  (set! player5Y (+ player5Y playerVel5))
                                  (set! player6Y (+ player6Y playerVel6))
                                  (set! player7Y (+ player7Y playerVel7))
                                  (set! player8Y (+ player8Y playerVel8))
                                  (set! player9Y (+ player9Y playerVel9))
                                  (set! player10Y (+ player10Y playerVel10))
                                  (set! player11Y (+ player11Y playerVel11))
                                  (send canvas refresh-now)
                                  (set! i (add1 i))))
                         
                         (send canvas refresh-now)
                         ;(set! jugadores (nueva-generacion jugadores 1))
                         
                         )])
 
; Make a canvas that handles events in the frame
(define canvas
(new canvas% 
     [parent frame]
     [paint-callback
      (lambda (my-canvas dc)
        (send dc set-brush "green" 'solid)
        (send dc set-pen "green" 0 'solid)
        
        (send (send canvas get-dc) erase)
        (send dc draw-rectangle 0 0 1400 800)
        (define listaX (colocar jugadores '() 100 500 900 1))
        (send dc set-brush "white" 'solid)
        (send dc set-pen "white" 0 'solid)
        
        ;Dibujar el centro
        (send dc draw-rectangle 690 0 20 800)
        ;Dibujar la cancha izquierda
        (send dc draw-rectangle 0 300 50 20)
        (send dc draw-rectangle 30 300 20 200)
        (send dc draw-rectangle 0 480 50 20)
        ;Dibujar la cancha derecha
        (send dc draw-rectangle 1350 300 50 20)
        (send dc draw-rectangle 1350 300 20 200)
        (send dc draw-rectangle 1350 480 50 20)
        ;Dibujar medio circulo
        (send dc set-brush "white" 'transparent)
        (send dc set-pen "white" 4 'solid)
        (send dc draw-arc 15 365 70 70 (/ (* 3 pi) 2) (/ pi 2))
        (send dc draw-arc 1315 365 70 70 (/ pi 2) (/ (* 3 pi) 2))
        (send dc draw-arc 600 300 200 200 0 (* 2 pi))
        
        (send dc set-pen "black" 0 'solid)
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
        (send (send canvas get-dc) draw-ellipse ballX ballY 20 20)
        
        (send dc set-pen "white" 1 'solid)
        (define listaY (colocar jugadores '() 1260 860 460 -1))
        (send (send canvas get-dc) draw-rectangle (list-ref listaY 0) player1Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref listaY 1) player2Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref listaY 2) player3Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref listaY 3) player4Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref listaY 4) player5Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref listaY 5) player6Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref listaY 6) player7Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref listaY 7) player8Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref listaY 8) player9Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref listaY 9) player10Y 30 30)
        (send (send canvas get-dc) draw-rectangle 1320 player11Y 30 30)
        
        
        (send dc draw-text "Jugador 1" (- (list-ref listaX 0) 20) (+ player1Y 35))
        
        
        )]
        ))


(define (colocar equipo listaX defensa medio delantero tipo)
  (cond [(null? equipo)
         listaX
         ]
        [
         (cond [(= (car (car equipo)) 1) 
         ;(set! defensa (+ defensa 50))
         (cons listaX defensa)
         (colocar (cdr equipo) (append listaX (list defensa)) (+ defensa (* 80 tipo)) medio delantero tipo)]
               [(= (car (car equipo)) 2) 
                ;(set! medio (+ medio 50))
               
                (colocar (cdr equipo) (append listaX (list medio)) defensa (+ medio (* 80 tipo)) delantero tipo)]
               [(= (car (car equipo)) 3)
                ;(set! delantero (+ delantero 50))
               
                (colocar (cdr equipo) (append listaX (list delantero)) defensa medio (+ (* 80 tipo) delantero) tipo)]
               [(= (car (car equipo)) 0)
                (colocar (cdr equipo) (append listaX (list 50)) defensa medio delantero tipo)]
               )
         ]))


; Show the frame by calling its show method
(send frame show #t)