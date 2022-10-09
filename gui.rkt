#lang racket
(require racket/gui/base)

(define ballX 0)
(define ballY 0)
; Make a frame by instantiating the frame% class
(define frame (new frame%
                   [label "WCQTec"]
                   [width 600]
                   [height 600]))
 
; Make a static text message in the frame
 
; Make a button in the frame
(new button% [parent frame]
             [label "Update"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         ;(send my-canvas% draw-bitmap)
                         (set! ballX (random 500))
                         (set! ballY (random 500))
                         ;(send canvas get-dc)
                         (send canvas refresh-now)
                         )])
 
 
; Make a canvas that handles events in the frame
(define canvas
(new canvas% 
     [parent frame]
     [paint-callback
      (lambda (my-canvas dc)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Downloads/ball1.png" #:backing-scale 20) ballX ballY)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Downloads/player.jpg" #:backing-scale 10) 100 100)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Downloads/player.jpg" #:backing-scale 10) 200 100)
        )]))


; Show the frame by calling its show method
(send frame show #t)