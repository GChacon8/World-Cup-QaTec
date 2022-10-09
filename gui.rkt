#lang racket
(require racket/gui/base)

(define x 2)
(for ([i '(1 2 3)])
  (define x i)
    (display i))


; Make a frame by instantiating the frame% class
(define frame (new frame%
                   [label "Example"]
                   [width 600]
                   [height 600]))
 
; Make a static text message in the frame
(define msg (new message% [parent frame]
                          [label "No events so far..."]))
 
; Make a button in the frame
(new button% [parent frame]
             [label "Click Me"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msg set-label "Button click"))])
 
; Show the frame by calling its show method
(send frame show #t)

; Derive a new canvas (a drawing window) class to handle events
(define my-canvas%
  (class canvas% ; The base class is canvas%
    ; Define overriding method to handle mouse events
    (define/override (on-event event)
      (send msg set-label "Canvas mouse"))
    ; Define overriding method to handle keyboard events
    (define/override (on-char event)
      (send msg set-label "Canvas keyboard"))
    ; Call the superclass init, passing on all init args
    (super-new)))
 
; Make a canvas that handles events in the frame
(new my-canvas% 
     [parent frame]
     [paint-callback
      (λ (canvas dc)
        (send dc draw-bitmap (read-bitmap "C:/Users/Usuario/Downloads/soccerball.png") 10 20))])
