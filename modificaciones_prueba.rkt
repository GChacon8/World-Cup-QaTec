#lang racket
(require racket/gui/base)
(require "algoritmo-genetico.rkt")

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

(define marco%
  (class object%
    (init-field widht
                height)
    (define/public (create-canvas) (send canvas refresh-now))
    (super-new)))


(define marco (new marco% [widht 1200] [height 800]))
(send marco create-canvas)
(send frame show #t)