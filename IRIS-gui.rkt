#lang racket
(require racket/gui/base)

(define frame (new frame% ; this creates the full window on which we will make the visualizer on
  [label "IRIS"]
  [width 900]
  [height 600]))

(define r0-value 0) ; we will change this when step is clicked, also this is temporary for now, later will make something for vector for register


(define main-panel (new vertical-panel% 
                        [parent frame])) ; this shows that this panel is in frame


(define toolbar (new horizontal-panel%
                     [parent main-panel]))



(define content (new horizontal-panel%
                     [parent main-panel]))

(define registers (new group-box-panel%
                       [parent content]
                       [label "Registers"]))

(define (make-registers

(define r0-label (new message%
                      [parent registers]
                      [label "R0: 0"]))

(define (change-r0)
  (send r0-label set-label
        (format "R0: ~a" r0-value))) ; the ~a puts r0-value instead of it, the ~a sort of works like an f string in python


(define r1-label (new message%
                      [parent registers]
                      [label "R1: 0"]))

(define r2-label (new message%
                      [parent registers]
                      [label "R2: 0"]))

(define r3-label (new message%
                      [parent registers]
                      [label "R3: 0"]))


(new button%
     [parent toolbar]
     [label "Run"]
     [callback
      (lambda (button event) ; the button and event are two inputs the function takes, button: the button that was pressed, event: what should happen when the button is pressed
        (displayln "Run Clicked"))]) ; prints this string when the button is pressed. added this for now to test, can be removed later

(new button%
     [parent toolbar]
     [label "Step"]
     [callback
      (lambda (button event) ; the button and event are two inputs the function takes, button: the button that was pressed, event: what should happen when the button is pressed
        (set r0-value (+ r0-value 1))
        (change-r0))])

(new button%
     [parent toolbar]
     [label "Reset"]
     [callback
      (lambda (button event) ; the button and event are two inputs the function takes, button: the button that was pressed, event: what should happen when the button is pressed
        (displayln "Reset Clicked"))]) ; prints this string when the button is pressed. added this for now to test, can be removed later




(send frame show #t) ; this shows the window ; #t means true