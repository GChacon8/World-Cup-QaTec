#lang slideshow

(provide nueva-generacion crear_equipo agregarPosicion cambiar)

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


;(crear_equipo '() 1)
;(promedio(cdar (crear_equipo '() 1)))
;(prueba '((1 3 0 5 1) (2 7 8 9 6) (3 8 0 1 5) (4 0 7 10 6) (5 7 10 1 0) (6 4 0 0 4) (7 5 8 4 1) (8 2 5 3 3) (9 6 1 5 7) (10 1 5 10 3) (11 4 2 6 9)))
;(define lista '((1 3 0 5 1) (2 7 8 9 6) (3 8 0 1 5) (4 0 7 10 6) (5 7 10 1 0) (6 4 0 0 4) (7 5 8 4 1) (8 2 5 3 3) (9 6 1 5 7) (10 1 5 10 3) (11 4 2 6 9)))
;(agregarPosicion lista '() 4 3 3 )
;(promedios(agregarPosicion lista '() 4 3 3) '())
;(seleccion listabuena (promedios listabuena '()) '(0 0 0 0 0) '(0 0 0 0 0) '(0 0 0 0 0))
;(seleccion(crear_equipo '() 1) '(5 4) 0)
;(reproduccion '((1 2 7 8 9 6) (1 4 0 7 10 6) (2 5 7 10 1 0)) '() 2)
;(cambiar '(1 2 3 4) '() 2 7 0)
;(mutacion '(0 2 6 6) 0)
;(nueva-generacion '((1 1 3 0 5 1) (1 2 7 8 9 6) (1 3 8 0 1 5) (1 4 0 7 10 6) (2 5 7 10 1 0) (2 6 4 0 0 4) (2 7 5 8 4 1) (3 8 2 5 3 3) (3 9 6 1 5 7) (3 10 1 5 10 3) (0 11 4 2 6 9)) 1)