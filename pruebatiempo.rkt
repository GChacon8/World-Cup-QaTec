#lang racket

(define ballX 100)
(define ballY 100)
(define canchaX 0)
(define canchaY 300)
(define col 0)

(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))

(define timeSecs 0)
(define (checkTime)
  (let ([i 0])
    (while (= 1  1)
           (sleep 1)
           (set! timeSecs (+ timeSecs 1))
           (cond [(= timeSecs 15)
                  (display timeSecs)
                  (set! timeSecs 0)])
          
           (set! i (add1 i)))))

;(checkTime)

(define (colisionPrueba)
  (cond
    ((and (>= (+ ballX 15) canchaX) (<= (+ ballX 15) (+ canchaX 50)) (>= (+ ballY 15) (+ canchaY 70)) (<= (- ballY 15) canchaY)) (set! col 1)
                                                                                                                                           ;(display 1)
                                                                                                                                           (display ballX))
    (else (set! col 0))))

(while (= col  0)
       ;(sleep 1)
           (set! ballX (- ballX 1))
           (colisionPrueba)
          
           )
