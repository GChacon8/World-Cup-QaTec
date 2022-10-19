#lang slideshow

(provide nueva-generacion crear_equipo agregarPosicion cambiar)

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
       (else (promedios (cdr poblacion) (cons (promedio (cdar poblacion)) lista-promedios)))))



(define (seleccion poblacion promedios mejor1 mejor2 mejor3)
  (cond
    ((null? promedios) (list mejor1 mejor2 mejor3))
    ((> (car promedios) (promedio (cdr mejor1))) (seleccion (cdr poblacion) (cdr promedios) (car poblacion) mejor2 mejor3))
    ((> (car promedios) (promedio (cdr mejor2))) (seleccion (cdr poblacion) (cdr promedios) mejor1 (car poblacion) mejor3))
    ((> (car promedios) (promedio (cdr mejor3))) (seleccion (cdr poblacion) (cdr promedios) mejor1 mejor2 (car poblacion)))
    (else (seleccion (cdr poblacion) (cdr promedios) mejor1 mejor2 mejor3))))


(define (reproduccion seleccionados hijo i)
  (cond
    ((= i 5) (reverse hijo))
    (else (reproduccion seleccionados (cons (list-ref (list-ref seleccionados (random 3)) i) hijo) (+ i 1)))))




(define (mutacion hijo i)
  (cond[(> i 3) hijo]
       [(= (random 3) 0)
        (cond [(= (list-ref hijo i) 10) hijo]
              (else(cambiar hijo '() i (+ (random 10) 1) 0)))]
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
       (else (append (list (cons i (mutacion(reproduccion (seleccion equipo-actual (promedios equipo-actual '()) '(0 0 0 0 0) '(0 0 0 0 0) '(0 0 0 0 0)) '() 1) 0))) (nueva-generacion equipo-actual (+ i 1))))
  ))