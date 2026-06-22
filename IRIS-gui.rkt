; content
;├── code-panel
;├── middle-panel
;│   ├── register-panel
;│   └── stack-panel
;└── memory-panel

#lang racket
(require racket/gui/base)

(define frame (new frame% ; this creates the full window on which we will make the visualizer on
  [label "IRIS"]
  [width 900]
  [height 600]))

; (define r0-value 0) ; we will change this when step is clicked, also this is temporary for now, later will make something for vector for register


(define main-panel (new vertical-panel% 
                        [parent frame])) ; this shows that this panel is in frame


(define toolbar (new horizontal-panel%
                     [parent main-panel]))


(define content (new horizontal-panel%
                     [parent main-panel]))

(define middle-panel (new vertical-panel%
                          [parent content]))

(define register-panel (new group-box-panel%
                       [parent middle-panel]
                       [label "Registers"]))

(define stack-panel (new group-box-panel%
                       [parent middle-panel]
                       [label "Stack"]))
(define (make-stack-labels i)
  (cond
    [(< i 0) '()]
    [else
     (cons (new message%
                [parent stack-panel]
                [label (format "Stack[~a]: -" i)]
                [auto-resize #t])
           (make-register-labels (- i 1)))]))



(define (make-register-labels i)
  (cond
    [(= i 16) '()]
    [else
     (cons (new message%
                [parent register-panel]
                [label (format "R~a: 0" i)]
                [auto-resize #t])
           (make-register-labels (+ i 1)))]))

(define register-labels
  (make-register-labels 0))

;(define (change-r0)
 ; (define r0-label (list-ref register-labels 0))
  ;(send r0-label set-label
   ;     (format "R0: ~a" r0-value))) ; the ~a puts r0-value instead of it, the ~a sort of works like an f string in python

; first made a specific function to update the r0 register, now will make a general function to update any register

(define (set-register-value! reg-num value)
  (define label-of-register (list-ref register-labels reg-num))
  (send label-of-register set-label
        (format "R~a: ~a" reg-num value)))


(new button%
     [parent toolbar]
     [label "Run"]
     [callback
      (lambda (button event) ; the button and event are two inputs the function takes, button: the button that was pressed, event: information about the click
        (displayln "Run Clicked"))]) ; prints this string when the button is pressed. added this for now to test, can be removed later

(new button%
     [parent toolbar]
     [label "Step"]
     [callback
      (lambda (button event) ; the button and event are two inputs the function takes, button: the button that was pressed, event: information about the click
        (set-register-value! 0 123))])

(new button%
     [parent toolbar]
     [label "Reset"]
     [callback
      (lambda (button event) ; the button and event are two inputs the function takes, button: the button that was pressed, event: information about the click
        (displayln "Reset Clicked"))]) ; prints this string when the button is pressed. added this for now to test, can be removed later




(send frame show #t) ; this shows the window ; #t means true