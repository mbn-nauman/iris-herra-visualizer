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
                       [label "Registers"]
                       [alignment '(left top)] ; this forces the children of the panel to be placed on top left of this panel
                       [spacing 4])) ; this is amount of spacing between the children

;new horizontal panel for  the checkboxes to show and hide value format columns

(define register-options-panel
  (new horizontal-panel%
       [parent register-panel]
       [alignment '(left top)] ; this forces the children of the panel to be placed on top left of this panel
       [stretchable-height #f])) ; this forced racket to not auto strech the checkbox row and keep it the size just needed for the check boxes

(define register-table (new horizontal-panel% ; decided to make a new panel to make different types of formats as different panels inside this as that would make it easier to filter/show different types of formats (dec/hex/ascii/all)
       [parent register-panel]
       [alignment '(left top)] ; this forces the children of the panel to be placed on top left of this panel
       [spacing 20] ; this is amount of spacing between the columns...20pixels
       [stretchable-height #f])) ; this forced racket to not auto strech the checkbox row and keep it the size just needed for the check boxes

; now adding vertical panels for each format

(define reg-name-column (new vertical-panel%
       [parent register-table]
       [alignment '(left top)] ; this forces the children of the panel to be placed on top left of this panel
       [stretchable-width #f] ; this forces column to not strech more than it should so the column, when hidden, does it not take up extra space
       [stretchable-height #f])) ; this forced racket to not auto strech vertically

(define reg-dec-column (new vertical-panel%
       [parent register-table]
       [alignment '(left top)] ; this forces the children of the panel to be placed on top left of this panel
       [stretchable-width #f] ; this forces column to not strech more than it should so the column, when hidden, does it not take up extra space
       [stretchable-height #f])) ; this forced racket to not auto strech vertically

(define reg-hex-column (new vertical-panel%
       [parent register-table]
       [alignment '(left top)] ; this forces the children of the panel to be placed on top left of this panel
       [stretchable-width #f] ; this forces column to not strech more than it should so the column, when hidden, does it not take up extra space
       [stretchable-height #f])) ; this forced racket to not auto strech vertically

(define reg-ascii-column (new vertical-panel%
       [parent register-table]
       [alignment '(left top)] ; this forces the children of the panel to be placed on top left of this panel
       [stretchable-width #f] ; this forces column to not strech more than it should so the column, when hidden, does it not take up extra space
       [stretchable-height #f])) ; this forced racket to not auto strech vertically

; making the check boxes now -- as functions now so I can remove extra spaces between columns when a column is hidden

(define reg-dec-checkbox
  (new check-box%
       [parent register-options-panel]
       [label "Dec"]
       [value #t]
       [callback
        (lambda (checkbox event)
          (refresh-register-columns!))])) ; this function is going to decide which columns to show and will rebuild the register column each time we hide/show a column

(define reg-hex-checkbox
  (new check-box%
       [parent register-options-panel]
       [label "Hex"]
       [value #t]
       [callback
        (lambda (checkbox event)
          (refresh-register-columns!))])) ; this function is going to decide which columns to show and will rebuild the register column each time we hide/show a column

(define reg-ascii-checkbox
  (new check-box%
       [parent register-options-panel]
       [label "ASCII"]
       [value #t]
       [callback
        (lambda (checkbox event)
          (refresh-register-columns!))])) ; this function is going to decide which columns to show and will rebuild the register column each time we hide/show a column

; time to write the refresh register function :)

(define (refresh-register-columns!)
  (send register-table change-children ; changes children according to checked boxes
        (lambda (children)
                 (append (list reg-name-column) ; makes sure register column is always visible
                         (if (send reg-dec-checkbox get-value) (list reg-dec-column) '()) ; if dec is checked then show if not then dont add it to list to show
                         (if (send reg-hex-checkbox get-value) (list reg-hex-column) '())
                         (if (send reg-ascii-checkbox get-value) (list reg-ascii-column) '())))))

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


; learnt about this later --> (~r #:base 16 #:min-width 4 #:pad-string "0" 42) --> "002a"
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
                      [label "Memory"]
                      [alignment '(left top)] ; this forces the children of the panel to be placed on top left of this panel
                      [spacing 4])) ; this is amount of spacing between the children

; horizontal panel for checkboxes to show or hide value format columns

(define memory-options-panel
  (new horizontal-panel%
       [parent memory-panel]
       [alignment '(left top)] ; this forces the children of the panel to be placed on top left of this panel
       [stretchable-height #f])) ; this forced racket to not auto strech the checkbox row and keep it the size just needed for the check boxes


(define memory-table (new horizontal-panel%
                          [parent memory-panel]
                          [alignment '(left top)] ; this forces the children of the panel to be placed on top left of this panel
                          [stretchable-height #f])) ; this forced racket to not auto strech the checkbox row and keep it the size just needed for the check boxes


; now adding vertical panels for each format

(define mem-address-column
  (new vertical-panel%
       [parent memory-table]))

(define mem-dec-column
  (new vertical-panel%
       [parent memory-table]))

(define mem-hex-column
  (new vertical-panel%
       [parent memory-table]))

(define mem-ascii-column
  (new vertical-panel%
       [parent memory-table]))

; making the check boxes now -- as functions now so I can remove extra spaces between columns when a column is hidden

(define mem-dec-checkbox
  (new check-box%
       [parent memory-options-panel]
       [label "Dec"]
       [value #t]
       [callback
        (lambda (checkbox event)
          (refresh-memory-columns!))])) ; this function is going to decide which columns to show and will rebuild the register column each time we hide/show a column

(define mem-hex-checkbox
  (new check-box%
       [parent memory-options-panel]
       [label "Hex"]
       [value #t]
       [callback
        (lambda (checkbox event)
          (refresh-memory-columns!))])) ; this function is going to decide which columns to show and will rebuild the register column each time we hide/show a column

(define mem-ascii-checkbox
  (new check-box%
       [parent memory-options-panel]
       [label "ASCII"]
       [value #t]
       [callback
        (lambda (checkbox event)
          (refresh-memory-columns!))])) ; this function is going to decide which columns to show and will rebuild the register column each time we hide/show a column

; time to write the refresh memory function :)

(define (refresh-memory-columns!)
  (send memory-table change-children ; changes children according to checked boxes
        (lambda (children)
                 (append (list mem-address-column) ; makes sure register column is always visible
                         (if (send mem-dec-checkbox get-value) (list mem-dec-column) '()) ; if dec is checked then show if not then dont add it to list to show
                         (if (send mem-hex-checkbox get-value) (list mem-hex-column) '())
                         (if (send mem-ascii-checkbox get-value) (list mem-ascii-column) '())))))

; now adding headers for each value format and address

(new message%
     [parent mem-address-column]
     [label "Address"]
     [auto-resize #t])

(new message%
     [parent mem-dec-column]
     [label "Dec"]
     [auto-resize #t])

(new message%
     [parent mem-hex-column]
     [label "Hex"]
     [auto-resize #t])

(new message%
     [parent mem-ascii-column]
     [label "ASCII"]
     [auto-resize #t])

; making function to create labels for the memory column

(define (make-memory-address-labels i)
  (cond
    [(= i 8) '()]
    [else
     (cons (new message%
                [parent mem-address-column]
                [label (hex-display i)]
                [auto-resize #t])
           (make-memory-address-labels (+ i 1)))]))

; making function to create labels for the format columns

(define (make-memory-value-labels i parent-column starting-text)
  (cond
    [(= i 8) '()]
    [else
     (cons (new message%
                [parent parent-column]
                [label starting-text]
                [auto-resize #t])
           (make-memory-value-labels (+ i 1) parent-column starting-text))]))

; using ealier 2 functions to make the labels now

(define mem-address-labels
  (make-memory-address-labels 0))

(define mem-dec-labels
  (make-memory-value-labels 0 mem-dec-column "0"))

(define mem-hex-labels
  (make-memory-value-labels 0 mem-hex-column "0x0000"))

(define mem-ascii-labels
  (make-memory-value-labels 0 mem-ascii-column "-"))


; setting formatted value in memory panel

(define (set-memory-value! memory-num value)
  (define dec-label (list-ref mem-dec-labels memory-num))
  (define hex-label (list-ref mem-hex-labels memory-num))
  (define ascii-label (list-ref mem-ascii-labels memory-num))

  (send dec-label set-label
        (format "~a" value))

  (send hex-label set-label
        (hex-display value))

  (send ascii-label set-label
        (ascii-display value)))


(define (reset-memory! i)
  (cond
    [(= i 8) (void)]
    [else
     (set-memory-value! i 0)
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
        (set-memory-value! 2 72)
        (set-memory-value! 3 105))])

(new button%
     [parent toolbar]
     [label "Reset"]
     [callback
      (lambda (button event) ; the button and event are two inputs the function takes, button: the button that was pressed, event: information about the click
        ;(displayln "Reset Clicked"))]) ; prints this string when the button is pressed. added this for now to test, can be removed later
        (reset-display!))])




(send frame show #t) ; this shows the window ; #t means true