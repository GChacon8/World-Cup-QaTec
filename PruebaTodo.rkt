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
  [list(jugador numero (+ 1 (random 10)) (random 11) (random 11) (random 11))]
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

(define X-equipo-1 '())
(define X-equipo-2 '())
(define Y-equipo-1 (list 300 300 300 300 300 300 300 300 300 300 300))
(define Y-equipo-2 (list 300 300 300 300 300 300 300 300 300 300 300))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (QaTec formaciones generaciones)
  (send frame show #t)
  
  (QatecAux (crear_equipo '() 1) (crear_equipo '() 1))

  (set! ballX (+ ballX ballVelX))
  (set! ballY (+ ballY ballVelY))
  (send canvas refresh-now)
  
  ;(send frame show #t)
  ;(send canvas refresh-now)
  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (colision lista1X lista1Y lista2X lista2Y ballX ballY fuerza1 habilidad1 fuerza2 habilidad2)
  ;(display lista1X)
  (cond
    ((and (null? lista1X) (null? lista2X)) 0)
    ((and (> (+ ballX 15) (+ (car lista1X) 10)) (< (- ballX 15) (+ (car lista1X) 40)) (> (+ ballY 15) (+ (car lista1Y) 10)) (< (- ballY 15) (+ (car lista1Y) 40))) (disparo 1 (car fuerza1) (car habilidad1)))
    ((and (> (+ ballX 15) (+ (car lista2X) 10)) (< (- ballX 15) (+ (car lista2X) 40)) (> (+ ballY 15) (+ (car lista2Y) 10)) (< (- ballY 15) (+ (car lista2Y) 40))) (disparo -1 (car fuerza2) (car habilidad2)))
    (else (colision (cdr lista1X) (cdr lista1Y) (cdr lista2X) (cdr lista2Y) ballX ballY (cdr fuerza1) (cdr habilidad1) (cdr fuerza2) (cdr habilidad2)))))

(define (colisionAux)
  (colision X-equipo-1 Y-equipo-1 X-equipo-2 Y-equipo-2 ballX ballY))

(define (disparo equipo fuerza habilidad)
  (set! ballVelX (* equipo fuerza))
  (set! ballVelY (- 10 habilidad)))

(define (deteccionGol)
  (cond
   ((and (>= (+ ballX 10) 0) (<= (+ ballX 10) 30) (<= (+ ballY 10) 500) (>= (- ballY 10) 300)) (set! team2Score (+ team2Score 1)) (set! ballX 700))
   ((and (>= (+ ballX 10) 1370) (<= (+ ballX 10) 1400) (<= (+ ballY 10) 500) (>= (- ballY 10) 300)) (set! team1Score (+ team1Score 1)) (set! ballX 700))
   ))

(define (checkPlayerAux i velocidad-equipo Y-equipo)
  (cond([< i 11]
        (cond [(< (list-ref Y-equipo i) 100)
         (checkPlayerAux (+ i 1) (cambiar velocidad-equipo '() i
                                           (abs (list-ref velocidad-equipo i)) 0) Y-equipo )
         ]
        [(> (list-ref Y-equipo i) 600)
         (checkPlayerAux (+ i 1) (cambiar velocidad-equipo '() i
                                           (* (abs (list-ref velocidad-equipo i)) -1) 0) Y-equipo)
        ]
        [else (checkPlayerAux (+ i 1) velocidad-equipo Y-equipo) ]))
       (else velocidad-equipo)
       )
  
  )
(define switch #f)
(define (checkTime)
  (cond
    ((equal? switch #f)
     (set! switch #t)
     (while (<= timeSecs 15)
           (sleep/yield 1)
           (set! timeSecs (+ timeSecs 1))
           (display timeSecs))
           )))


(define (QatecAux equipo1 equipo2)
  (set! X-equipo-1 (colocar (agregarPosicion equipo1 '() 5 3 2) '() 100 500 900 1))
  (set! X-equipo-2 (colocar (agregarPosicion equipo2 '() 5 3 2) '() 1260 860 460 -1))
  (actualizar equipo1 equipo2 (crear-lista-velocidad 0 '() equipo1) (crear-lista-velocidad 0 '() equipo2) 0 0)
 
  )

(define (actualizar equipo1 equipo2 velocidad-1 velocidad-2 i reloj)
  
  (cond[(<= i 5)
        (sleep/yield 0.001)
        (set! Y-equipo-1 (mover-equipo velocidad-1 Y-equipo-1 0))
        (set! Y-equipo-2 (mover-equipo velocidad-2 Y-equipo-2 0))
        (send canvas refresh-now)
        (thread checkBall)
        (thread deteccionGol)
        ;(display reloj)
        
        (colision X-equipo-1 Y-equipo-1 X-equipo-2 Y-equipo-2 ballX ballY
                  (crear-lista-fuerza 0 '() equipo1)
                  (crear-lista-habilidad 0 '() equipo1)
                  (crear-lista-fuerza 0 '() equipo2)
                  (crear-lista-habilidad 0 '() equipo2))
        (set! ballX (+ ballX ballVelX))
        (set! ballY (+ ballY ballVelY))
        (cond[(= reloj 100)
              (display i)
                          (actualizar equipo1 equipo2 (checkPlayerAux 0 velocidad-1 Y-equipo-1) (checkPlayerAux 0 velocidad-2 Y-equipo-2) (+ i 1) 0)]
             (else(actualizar equipo1 equipo2 (checkPlayerAux 0 velocidad-1 Y-equipo-1) (checkPlayerAux 0 velocidad-2 Y-equipo-2) i (+ reloj 1)) ))
        ]
       ))


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

(define (crear-lista-velocidad i lista equipo)
  (cond ((< i 11)
        (crear-lista-velocidad (+ i 1) (cons (list-ref (list-ref equipo i) 3) lista) equipo))
        (else lista)
  ))

(define (crear-lista-fuerza i lista equipo)
  (cond ((< i 11)
        (crear-lista-fuerza (+ i 1) (cons (list-ref (list-ref equipo i) 1) lista) equipo))
        (else lista)
  ))

(define (crear-lista-habilidad i lista equipo)
  (cond ((< i 11)
        (crear-lista-habilidad (+ i 1) (cons (list-ref (list-ref equipo i) 2) lista) equipo))
        (else lista)
  ))

(define (mover-equipo velocidad-equipo-1 Y-equipo-1 i)
  
  (cond ((< i 11)
        (mover-equipo velocidad-equipo-1 (cambiar Y-equipo-1 '() i (+ (list-ref Y-equipo-1 i) (list-ref velocidad-equipo-1 i)) 0) (+ i 1)))
        (else Y-equipo-1)))



(define player1X 50)
(define player1Y 300)
(define ballX 492)
(define ballY 365)
;(define velocidad-equipo-1 (crear-lista-velocidad 0 '()))
(define timeSecs 0)
(define ballVelX 0)
(define ballVelY 0)
(define team1Score 0)
(define team2Score 0)
(define col 0)

(new style-list%)
(define font (make-object font% 10 'modern 'normal))
(define fontScore (make-object font% 9 'modern 'normal 'bold))
; Make a frame by instantiating the frame% class
(define frame (new frame%
                   [label "WCQTec"]
                   [width 1400]
                   [height 800]))

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
        
        ;(dibujar 0 (send canvas get-dc))

        (send dc set-brush "gray" 'solid)
        (send dc set-pen "gray" 4 'solid)
        (send dc draw-rounded-rectangle 550 0 300 30)
        (send dc draw-text (string-append "Equipo 1:" (number->string team1Score)) 560 10)
        (send dc draw-text (string-append "Equipo 2:" (number->string team2Score)) 750 10)
        (send dc set-brush "white" 'transparent)
        (send dc set-pen "black" 0 'solid)
        
        (send dc set-pen "blue" 1 'solid)
        
        (send dc set-pen "black" 1 'solid)
        (send (send canvas get-dc) draw-ellipse ballX ballY 20 20)
        (dibujar 0 X-equipo-1 Y-equipo-1  X-equipo-2 Y-equipo-2)
        
        ;(send dc draw-text "Jugador 1" (- (list-ref X-equipo-1 0) 20) (+ player1Y 35))
        
        
        )]
        ))

(define (dibujar i X-equipo-1 Y-equipo-1  X-equipo-2 Y-equipo-2)
  (cond ((< i 11)
        (send (send canvas get-dc) set-pen "red" 0 'solid)
        (send (send canvas get-dc) set-brush "red" 'solid)
        (send (send canvas get-dc) draw-rectangle (list-ref X-equipo-1 i) (list-ref Y-equipo-1 i) 30 30)
        (send (send canvas get-dc) draw-text (string-append "J" (number->string (+ i 1))) (+ (list-ref X-equipo-1 i) 2) (+ (list-ref Y-equipo-1 i) 35))
        (send (send canvas get-dc) set-pen "blue" 0 'solid)
        (send (send canvas get-dc) set-brush "blue" 'solid)
        (send (send canvas get-dc) draw-rectangle (list-ref X-equipo-2 i) (list-ref Y-equipo-2 i) 30 30)
        (send (send canvas get-dc) draw-text (string-append "J" (number->string (+ i 12))) (+ (list-ref X-equipo-2 i) 2) (+ (list-ref Y-equipo-2 i) 35))
        (dibujar (+ i 1) X-equipo-1 Y-equipo-1  X-equipo-2 Y-equipo-2))
        )
  
  )
(define(aux)
  (QaTec '((4 3 3) (5 3 2)) 15)
  )
;(thread checkTime)
;(thread aux)

(QaTec '((4 3 3) (5 3 2)) 15)
