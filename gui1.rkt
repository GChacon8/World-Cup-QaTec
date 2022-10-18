#lang racket
(require racket/gui/base)
(require "algoritmo-genetico.rkt")

(define (colocar equipo X-equipo defensa medio delantero tipo)
  (cond [(null? equipo)
         X-equipo
         ]
        [
         (cond [(= (car (car equipo)) 1) 
         ;(set! defensa (+ defensa 50))
         (cons X-equipo defensa)
         (colocar (cdr equipo) (append X-equipo (list defensa)) (+ defensa (* 80 tipo)) medio delantero tipo)]
               [(= (car (car equipo)) 2) 
                ;(set! medio (+ medio 50))
               
                (colocar (cdr equipo) (append X-equipo (list medio)) defensa (+ medio (* 80 tipo)) delantero tipo)]
               [(= (car (car equipo)) 3)
                ;(set! delantero (+ delantero 50))
               
                (colocar (cdr equipo) (append X-equipo (list delantero)) defensa medio (+ (* 80 tipo) delantero) tipo)]
               [(= (car (car equipo)) 0)
                (colocar (cdr equipo) (append X-equipo (list 50)) defensa medio delantero tipo)]
               )
         ]))

(define (crear-lista-velocidad i lista)
  (cond ((< i 11)
        (crear-lista-velocidad (+ i 1) (cons (list-ref (list-ref jugadores i) 3) lista)))
        (else lista)
  ))
  


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
(define Y-equipo-1 (list 300 300 300 300 300 300 300 300 300 300 300))
(define Y-equipo-2 (list 300 300 300 300 300 300 300 300 300 300 300))
(define X-equipo-1 (colocar jugadores '() 100 500 900 1))
(define X-equipo-2 (colocar jugadores '() 1260 860 460 -1))
(define velocidad-equipo-1 (crear-lista-velocidad 0 '()))
(define defensa 50)
(define medio 350)
(define delantero 550)


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
(define (checkPlayerAux i)
  
  (cond([< i 11]
        (cond [(< (list-ref Y-equipo-1 i) 100)
         (set! velocidad-equipo-1 (cambiar velocidad-equipo-1 '() i
                                           (abs (list-ref velocidad-equipo-1 i)) 0))
         ]
        [(> (list-ref Y-equipo-1 i) 600)
         (set! velocidad-equipo-1 (cambiar velocidad-equipo-1 '() i
                                           (* (abs (list-ref velocidad-equipo-1 i)) -1) 0))
         ])
  (checkPlayerAux (+ i 1))))
  )

(define (checkPlayer)
  (checkPlayerAux 0))

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
                                  ;(set! player1Y (+ player1Y playerVel1))
                                  (mover-equipo-1 0)
                                  
                                
                                  (send canvas refresh-now)
                                  (set! i (add1 i))))
                         
                         (send canvas refresh-now)
                         ;(set! jugadores (nueva-generacion jugadores 1))
                         
                         )])

(define (mover-equipo-1 i)
  
  (cond ((< i 11)
         
        (set! Y-equipo-1 (cambiar Y-equipo-1 '() i (+ (list-ref Y-equipo-1 i) (list-ref velocidad-equipo-1 i)) 0))
        (mover-equipo-1 (+ i 1)))
  ))
 
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
        
        (dibujar 0 (send canvas get-dc) "red")
        
        
        (send dc set-pen "blue" 1 'solid)
        (send (send canvas get-dc) draw-rectangle (list-ref X-equipo-2 0) player1Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref X-equipo-2 1) player2Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref X-equipo-2 2) player3Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref X-equipo-2 3) player4Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref X-equipo-2 4) player5Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref X-equipo-2 5) player6Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref X-equipo-2 6) player7Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref X-equipo-2 7) player8Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref X-equipo-2 8) player9Y 30 30)
        (send (send canvas get-dc) draw-rectangle (list-ref X-equipo-2 9) player10Y 30 30)
        (send (send canvas get-dc) draw-rectangle 1320 player11Y 30 30)
        
        (send dc set-pen "black" 1 'solid)
        (send (send canvas get-dc) draw-ellipse ballX ballY 20 20)
        
        (send dc draw-text "Jugador 1" (- (list-ref X-equipo-1 0) 20) (+ player1Y 35))
        
        
        )]
        ))
(define (dibujar i dc color)
  (cond ((< i 11)
         (send dc set-pen color 0 'solid)
        (send dc draw-rectangle (list-ref X-equipo-1 i) (list-ref Y-equipo-1 i) 30 30)
        (dibujar (+ i 1) dc color)))
  )



; Show the frame by calling its show method
(send frame show #t)