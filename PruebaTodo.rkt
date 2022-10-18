#lang racket
(require racket/gui/base)

;While hecho a mano
(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))

;ALG GENETICO

(define (jugador numero fuerza habilidad velocidad desplazamiento)
  (append (list numero) (list fuerza) (list habilidad) (list velocidad) (list desplazamiento))
  )

(define (agregarPosicion listaVieja listaNueva contador1 contador2 contador3)
  (cond
    ((null? listaVieja) (reverse listaNueva))
    ((> contador1 0) (agregarPosicion (cdr listaVieja) (cons (cons 1 (car listaVieja)) listaNueva) (- contador1 1) contador2 contador3))
    ((> contador2 0) (agregarPosicion (cdr listaVieja) (cons (cons 2 (car listaVieja)) listaNueva) contador1 (- contador2 1) contador3))
    ((> contador3 0) (agregarPosicion (cdr listaVieja) (cons (cons 3 (car listaVieja)) listaNueva) contador1 contador2 (- contador3 1)))
    ((= 0 contador3) (agregarPosicion (cdr listaVieja) (cons (cons 0 (car listaVieja)) listaNueva) contador1 contador2 contador3))))


(define (genera_jugador numero)
  [list(jugador numero (random 11) (random 11) (random 11) (random 11))]
  )

(define (crear_equipo jugadores i)
  (cond[(equal? i 12) jugadores]
       (else [crear_equipo (append jugadores (genera_jugador i)) (+ i 1)])))

(define (promedio jugador)
 (/ [+ (car jugador) (cadr jugador) (caddr jugador) (cadddr jugador)] 4)
 )


(define (promedios poblacion lista-promedios)
  (cond((null? poblacion) (reverse lista-promedios))
       (else (promedios (cdr poblacion) (cons (promedio (cddar poblacion)) lista-promedios)))))



(define (seleccion poblacion promedios mejor1 mejor2 mejor3)
  (cond
    ((null? promedios) (list mejor1 mejor2 mejor3))
    ((> (car promedios) (promedio (cdr mejor1))) (seleccion (cdr poblacion) (cdr promedios) (car poblacion) mejor2 mejor3))
    ((> (car promedios) (promedio (cdr mejor2))) (seleccion (cdr poblacion) (cdr promedios) mejor1 (car poblacion) mejor3))
    ((> (car promedios) (promedio (cdr mejor3))) (seleccion (cdr poblacion) (cdr promedios) mejor1 mejor2 (car poblacion)))
    (else (seleccion (cdr poblacion) (cdr promedios) mejor1 mejor2 mejor3))))


(define (reproduccion seleccionados hijo i)
  (cond
    ((= i 6) (reverse hijo))
    (else (reproduccion seleccionados (cons (list-ref (list-ref seleccionados (random 3)) i) hijo) (+ i 1)))))




(define (mutacion hijo i)
  (cond[(> i 3) hijo]
       [(= (random 3) 0)
        (cond [(= (list-ref hijo i) 10) hijo]
              (else(cambiar hijo '() i (random 11) 0)))]
        (else (mutacion hijo (+ i 1)))
       ))

(define (cambiar lista-original lista-nueva indice elemento i)
  (cond ((= indice i) (append (reverse(cons elemento lista-nueva)) (cdr lista-original)))
        (else(cambiar (cdr lista-original) (cons (car lista-original) lista-nueva) indice elemento (+ i 1)))))
 

(define (prueba poblacion)
  (promedios poblacion '())
  )

(define(nueva-generacion equipo-actual i)
  (cond[(< 11 i) '()]
       (else (append (list (cons i (mutacion(reproduccion (seleccion equipo-actual (promedios equipo-actual '()) '(0 0 0 0 0) '(0 0 0 0 0) '(0 0 0 0 0)) '() 2) 0))) (nueva-generacion equipo-actual (+ i 1))))
  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (QaTec formaciones generaciones)
  (send frame show #t)
  (send canvas refresh-now)
  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define jugadores (agregarPosicion (crear_equipo '() 1) '() 4 4 2))
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

(new style-list%)
(define font (make-object font% 10 'modern 'normal))
(define fontScore (make-object font% 9 'modern 'normal 'bold))
; Make a frame by instantiating the frame% class
(define frame (new frame%
                   [label "WCQTec"]
                   [width 1400]
                   [height 800]))

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

        (send dc set-brush "gray" 'solid)
        (send dc set-pen "gray" 4 'solid)
        (send dc draw-rounded-rectangle 550 0 300 30)
        (send dc draw-text (string-append "Equipo 1:" (number->string team1Score)) 560 10)
        (send dc draw-text (string-append "Equipo 2:" (number->string team2Score)) 750 10)
        (send dc set-brush "white" 'transparent)
        (send dc set-pen "black" 0 'solid)
        
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

(QaTec '((4 3 3) (5 3 2)) 15)