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

(define register-table (new horizontal-panel% ; decided to make a new panel to make different types of formats as different panels inside this as that would make it easier to filter/show different types of formats (dec/hex/ascii/all)
       [parent register-panel]))

; now adding vertical panels for each format

(define reg-name-column (new vertical-panel%
       [parent register-table]))

(define reg-dec-column (new vertical-panel%
       [parent register-table]))

(define reg-hex-column (new vertical-panel%
       [parent register-table]))

(define reg-ascii-column (new vertical-panel%
       [parent register-table]))

; making column headers for each format column now

(new message%
     [parent reg-name-column]
     [label "Reg"]
     [auto-resize #t])

(new message%
     [parent reg-dec-column]
     [label "Dec"]
     [auto-resize #t])

(new message%
     [parent reg-hex-column]
     [label "Hex"]
     [auto-resize #t])

(new message%
     [parent reg-ascii-column]
     [label "ASCII"]
     [auto-resize #t])

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


(define (ascii-display value) ; helper function for converting integer to ASCII
  (if (and (integer? value)
           (>= value 32)
           (<= value 126)) ; we only do 32-126 because that is the ASCII numbers for printable characters, rest may mess up things like int=10 is a new line
      (string (integer->char value))
      "-")) ; if it is not between 32-126, then we just print "-"


; making function to create labels for the reg column

(define (make-reg-name-labels i)
  (cond
    [(= i 16) '()]
    [else
     (cons (new message%
                [parent reg-name-column]
                [label (format "R~a" i)]
                [auto-resize #t])
           (make-reg-name-labels (+ i 1)))]))

; making function to create labels for the format columns

(define (make-reg-value-labels i parent-column starting-text)
  (cond
    [(= i 16) '()]
    [else
     (cons (new message%
                [parent parent-column]
                [label starting-text]
                [auto-resize #t])
           (make-reg-value-labels (+ i 1) parent-column starting-text))]))


; making the labels now using the 2 earlier helper functions

(define reg-name-labels
  (make-reg-name-labels 0))


(define reg-dec-labels
  (make-reg-value-labels 0 reg-dec-column "0"))


(define reg-hex-labels
  (make-reg-value-labels 0 reg-hex-column "0x0000"))


(define reg-ascii-labels
  (make-reg-value-labels 0 reg-ascii-column "-"))

; setting formatted value in reg panel

(define (set-register-value! reg-num value)
  (define dec-label (list-ref reg-dec-labels reg-num))
  (define hex-label (list-ref reg-hex-labels reg-num))
  (define ascii-label (list-ref reg-ascii-labels reg-num))

  (send dec-label set-label
        (format "~a" value))
  (send hex-label set-label
        (hex-display value))
  (send ascii-label set-label
        (ascii-display value)))

  

(define (reset-registers! i)
  (cond
    [(= i 16) (void)]
    [else
     (set-register-value! i 0)
     (reset-registers! (+ i 1))]))



(define memory-panel (new group-box-panel%
                      [parent content]
                      [label "Memory"]))


(new message%
     [parent memory-panel]
     [label "Address    Dec    Hex       ASCII"]
     [auto-resize #t])


(define (make-memory-labels i)
  [cond
    [(= i 8) '()]
    [else
     (cons (new message%
            [parent memory-panel]
            [label (format "0x000~a     Dec: -     Hex: -     ASCII: -" i)]
            [auto-resize #t])
           (make-memory-labels (+ i 1)))]])


(define memory-labels
  (make-memory-labels 0))


(define (set-memory-value! mem-num value)
  (define label-of-memory (list-ref memory-labels mem-num))
  (send label-of-memory set-label
        (format "0x000~a     Dec: ~a     Hex: ~a     ASCII: ~a" mem-num value (hex-display value) (ascii-display value))))


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
        (set-register-value! 0 65)
        (set-register-value! 1 123)
        (set-register-value! 2 10))])

(new button%
     [parent toolbar]
     [label "Reset"]
     [callback
      (lambda (button event) ; the button and event are two inputs the function takes, button: the button that was pressed, event: information about the click
        ;(displayln "Reset Clicked"))]) ; prints this string when the button is pressed. added this for now to test, can be removed later
        (reset-display!))])




(send frame show #t) ; this shows the window ; #t means true