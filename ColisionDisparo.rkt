#lang racket
(define (colision lista ballX ballY)
  (cond
    ((null? lista))
    (((> ballX (caar lista)) and (> ballX (+ (caar lista) 30)) and (> ballY (cadar lista)) and (< ballY (+ (cadar lista) 30))) (disparo (caddar lista) (cadddar lista) (caddddar lista)))
    (else (colision (car lista) ballX ballY))))

(define (disparo equipo fuerza habilidad)
  (!set ballVelX fuerza)
  (!set ballVelY habilidad)) 