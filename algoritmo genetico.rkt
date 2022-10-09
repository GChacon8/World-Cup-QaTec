#lang slideshow
(define (jugador numero fuerza habilidad velocidad desplazamiento)
  (append (list numero)(list fuerza) (list habilidad) (list velocidad) (list desplazamiento))
  )

(define (genera_jugador numero)
  [list(jugador numero (random 11) (random 11) (random 11) (random 11))]
  )

(define (crear_equipo jugadores i)
  (cond[(equal? i 11) jugadores]
       (else [crear_equipo (append jugadores (genera_jugador i)) (+ i 1)])))

(define (promedio jugador)
 (/ [+ (car jugador) (cadr jugador) (caddr jugador) (cadddr jugador)] 4)
 )

;(define (seleccion poblacion elegidos)
 ; (cond [[null? poblacion] elegidos]
  ;      [cond[[null? elegidos] [seleccion (cdr poblacion)(append elegidos (car poblacion))]
   ;          [[>= (promedio(cdar poblacion) (promeido(car elegidos)))]
    ;          (cond([ >= (length elegidos) 3]
     ;               (append elegidos (car poblacion))
      ;              (seleccion (cdr poblacion) (cdr elegidos)))
       ;            [else(seleccion (cdr poblacion) (append elegidos (car poblacion)))])]
        ;     (else[seleccion (cdr poblacion) elegidos])]]))
  

(define(seleccion poblacion elegidos)
  (cond([null? poblacion] elegidos 
        )
    )
  )


(crear_equipo '() 1)
(promedio(cdar (crear_equipo '() 1)))
(seleccion(crear_equipo '() 1) '())