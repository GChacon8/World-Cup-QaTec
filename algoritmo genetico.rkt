#lang slideshow

(define (jugador numero fuerza habilidad velocidad desplazamiento)
  (append (list numero) (list fuerza) (list habilidad) (list velocidad) (list desplazamiento))
  )

(define (agregarPosicion listaVieja listaNueva contador1 contador2 contador3)
  (cond
    ((null? listaVieja) (reverse listaNueva))
    ((> contador1 0) (agregarPosicion (cdr listaVieja) (cons (cons 1 (car listaVieja)) listaNueva) (- contador1 1) contador2 contador3))
    ((> contador2 0) (agregarPosicion (cdr listaVieja) (cons (cons 2 (car listaVieja)) listaNueva) contador1 (- contador2 1) contador3))
    ((> contador3 0) (agregarPosicion (cdr listaVieja) (cons (cons 3 (car listaVieja)) listaNueva) contador1 contador2 (- contador3 1)))))

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
       (else (promedios (cdr poblacion) (cons (promedio (cdar poblacion)) lista-promedios)))))

(define (prueba poblacion)
  (promedios poblacion '())
  )

(crear_equipo '() 1)
(promedio(cdar (crear_equipo '() 1)))
(prueba '((1 3 0 5 1) (2 7 8 9 6) (3 8 0 1 5) (4 0 7 10 6) (5 7 10 1 0) (6 4 0 0 4) (7 5 8 4 1) (8 2 5 3 3) (9 6 1 5 7) (10 1 5 10 3) (11 4 2 6 9)))
;(seleccion(crear_equipo '() 1) '(5 4) 0)