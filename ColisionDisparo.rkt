#lang racket
(define (colision lista ballX ballY)
  (cond
    ((null? lista))
    ((and (> ballX (caar lista)) (> ballX (+ (caar lista) 30)) (> ballY (cadar lista)) (< ballY (+ (cadar lista) 30))) (disparo (caddar lista) (car (cadddr lista)) (cdar (cadddr lista))))
    (else (colision (car lista) ballX ballY))))

(define (disparo equipo fuerza habilidad)
  (!set ballVelX fuerza)
  (!set ballVelY habilidad)) 