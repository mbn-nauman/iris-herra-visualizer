;frame
;└── main-panel
;    ├── toolbar
;    └── content
;        ├── code-panel
;        ├── register-panel
;        └── memory-panel

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

(define code-panel (new group-box-panel%
                      [parent content]
                      [label "Code / File"]))

(define code-text ; making a code-text object of text% to add some sample text to test codebox
  (new text%))

;(send code-text insert ; sample/test text
 ;     "heylol")

(define (set-code-display! code-string)
  (send code-text erase)
  (send code-text insert code-string))

(set-code-display!
 "Welcome to Iris, by David Wonnacott and Muhammad Bin Nauman")

(new editor-canvas% ; this object is used when we need to show some text to the user
     [parent code-panel]
     [editor code-text]
     [min-width 300]
     [min-height 400])


(define register-panel (new group-box-panel%
                       [parent content]
                       [label "Registers"]))

(new message%
     [parent register-panel]
     [label "Register    Value"]
     [auto-resize #t])

(define (make-register-labels i)
  (cond
    [(= i 16) '()]
    [else
     (cons (new message%
                [parent register-panel]
                [label (format "R~a             0" i)]
                [auto-resize #t])
           (make-register-labels (+ i 1)))]))


(define register-labels
  (make-register-labels 0))

(define (pad-left str target-length char) ; this function is used to add characters to the left of a string so registers can be written as 0x0043 instead of 0x41
  (if (>= (string-length str) target-length)
      str
      (string-append
       (make-string (- target-length (string-length str)) char)
       str)))

(define (hex-display value) ; this converts a number to hex
  (string-append
   "0x"
   (pad-left (string-upcase (number->string value 16)) 4 #\0))) ; converting to hex here ; string-upcase is used to change letters to uppercase

(define (set-register-value! reg-num value)
  (define label-of-register (list-ref register-labels reg-num))
  (send label-of-register set-label
        (format "R~a             ~a" reg-num value)))

(define (reset-registers! i)
  (cond
    [(= i 16) void]
    [else
     (set-register-value! i 0)
     (reset-registers! (+ i 1))]))



(define memory-panel (new group-box-panel%
                      [parent content]
                      [label "Memory"]))

(new message%
     [parent memory-panel]
     [label "Address    Value"]
     [auto-resize #t])

(define (make-memory-labels i)
  [cond
    [(= i 8) '()]
    [else
     (cons (new message%
            [parent memory-panel]
            [label (format "0x000~a      -" i)]
            [auto-resize #t])
           (make-memory-labels (+ i 1)))]])

(define memory-labels
  (make-memory-labels 0))

(define (set-memory-value! mem-num value)
  (define label-of-memory (list-ref memory-labels mem-num))
  (send label-of-memory set-label
        (format "0x000~a      ~a" mem-num value)))

(define (reset-memory! i)
  (cond
    [(= i 8) void]
    [else
     (set-memory-value! i "-")
     (reset-memory! (+ i 1))]))

(define (reset-display!)
  (reset-registers! 0)
  (reset-memory! 0))

(new button%
     [parent toolbar]
     [label "Run"]
     [callback
      (lambda (button event) ; the button and event are two inputs the function takes, button: the button that was pressed, event: information about the click
        ;(displayln "Run Clicked"))]) ; prints this string when the button is pressed. added this for now to test, can be removed later
        (set-code-display!
         "'the code in the file'"))])


(new button%
     [parent toolbar]
     [label "Step"]
     [callback
      (lambda (button event) ; the button and event are two inputs the function takes, button: the button that was pressed, event: information about the click
        (set-register-value! 0 123)
        (set-memory-value! 2 555))])

(new button%
     [parent toolbar]
     [label "Reset"]
     [callback
      (lambda (button event) ; the button and event are two inputs the function takes, button: the button that was pressed, event: information about the click
        ;(displayln "Reset Clicked"))]) ; prints this string when the button is pressed. added this for now to test, can be removed later
        (reset-display!))])




(send frame show #t) ; this shows the window ; #t means true