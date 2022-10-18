#lang racket
(require racket/gui/base)
(require "algoritmo-genetico.rkt")


(define playerVel1  3)
(define playerVel2 3)
(define playerVel3 3)
(define playerVel4 3)
(define playerVel5 3)
(define playerVel6 3)
(define playerVel7 3)
(define playerVel8 3)
(define playerVel9 3)
(define playerVel10 2)
(define playerVel11 45)
(define player1X 50)
(define player1Y 340)
(define player2X 180)
(define player2Y 340)
(define player3X 180)
(define player3Y 500)
(define player4X 180)
(define player4Y 180)
(define player5X 400)
(define player5Y 100)
(define player6X 400)
(define player6Y 600)
(define player7X 300)
(define player7Y 100)
(define player8X 300)
(define player8Y 600)
(define player9X 300)
(define player9Y 340)
(define player10X 430)
(define player10Y 380)
(define player11X 430)
(define player11Y 280)

(new style-list%)
(define font (make-object font% 10 'modern 'normal))
(define fontScore (make-object font% 9 'modern 'normal 'bold))
; Make a frame by instantiating the frame% class
(define frame (new frame%
                   [label "WCQTec"]
                   [width 1200]
                   [height 800]))

; Make a canvas that handles events in the frame
(define canvas 
(new canvas% 
     [parent frame]
     [paint-callback
      (lambda (my-canvas dc)
        (send dc set-brush "green" 'solid)
        (send dc set-pen "black" 1 'solid)
        (send dc draw-rectangle 0 0 1200 800)
        ;(send (send canvas get-dc) erase)
        )]
        ))

(define (colocar equipo lista_posiciones defensa medio delantero)
  (cond [(null? equipo)
         (reverse lista_posiciones)
         ]
        [
         (cond [(= (car (car equipo)) 1)   
         (colocar (cdr equipo) (cons (list defensa (random 750) (list-ref (car equipo) 3)) lista_posiciones) (+ defensa 50) medio delantero)]
               
               [(= (car (car equipo)) 2) 

                (colocar (cdr equipo) (cons (list medio (random 750)) lista_posiciones ) defensa (+ medio 50) delantero)]
               [(= (car (car equipo)) 3)
                (colocar (cdr equipo) (cons (list delantero (random 750) ) lista_posiciones ) defensa medio (+ delantero 50))]
               
               [(= (car (car equipo)) 0)
                (colocar (cdr equipo) (cons (list 50 50) lista_posiciones) defensa medio delantero)]
               )
         ]))



(define (checkPlayer_aux listaX)
  (cond [(< (list-ref (list-ref listaX 0) 1) 100)
         (cambiar listaX '() 0 (cambiar (list-ref listaX 0) '() 2 (abs (list-ref (list-ref listaX 0) 2)) 0) 0)]
        [(> (list-ref (list-ref listaX 0) 1) 600)
         (cambiar listaX '() 0 (cambiar (list-ref listaX 0) '() 2 (* (abs (list-ref (list-ref listaX 0) 2)) -1) 0) 0)
         ])
  )

;(colocar '((1 1 3 3 5 8) (1 2 6 6 8 6) (1 3 7 1 2 8) (1 4 2 2 0 9) (1 5 4 6 0 10) (2 6 7 2 1 5) (2 7 5 5 9 9) (3 8 4 5 3 5) (3 9 6 6 9 9) (3 10 0 8 3 1) (0 11 8 1 8 1)) '() 100 400 700) 
(checkPlayer_aux '((100 673 3) (150 712 6) (200 112 1) (250 264 2) (300 300 6) (400 315) (450 426) (700 415) (750 722) (800 101) (50 50)))
;(send frame show #t)