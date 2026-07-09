#lang racket
(require racket/gui/base racket/string)

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

; pad left and hex display for hex, moved up to accomodate the code panel editing we need and switch of format we need there

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



(define code-panel (new group-box-panel%
                      [parent content]
                      [label "Code / File"]))


(define code-row-count 8) ; number of code rows shown for now = 8

(define code-address-values ; this stores addresses for each row
  (make-vector code-row-count 0)) ; vector: fixed size list

(define code-command-values ; this stores the command values for each row as hex
  (make-vector code-row-count 0))

(define current-command-mode 0) ; 0 is for hex, 1 is for assembly


; this initializes each address line to its number and command to 0

(define (init-code-values! i)
  (cond
    [(= i code-row-count) (void)]
    [else
     (vector-set! code-address-values i i)
     (vector-set! code-command-values i 0)
     (init-code-values! (+ i 1))]))

(init-code-values! 0)

; going to start making the dropdown now for giving option of dec/hex for addresses and hex/asb for commands

(define code-options-panel
  (new horizontal-panel%
       [parent code-panel]
       [alignment '(left top)] ; this forces the children of the panel to be placed on top left of this panel
       [stretchable-height #f])) ; this forced racket to not auto strech the code option panel row and keep it the size just needed for dropdown menu

(define code-address-choice ; drop down for address column
  (new choice%
       [parent code-options-panel]
       [label "Address"]
       [choices '("Dec" "Hex")]
       [selection 1] ; auto choose hex initially
       [callback
        (lambda (choice event) ; for now we have just made a placeholder type thing here for event
          (refresh-code-display! 0))])) ; this means to rebuild all rows starting from row 0
                                        ; --> have to make this function, havent made it yet ;;;remove comment if have made it;;;


(define code-command-choice ; drop down for command column
  (new choice%
       [parent code-options-panel]
       [label "Command"]
       [choices '("Hex" "Assembly")]
       [selection 0] ; auto choose hex initially
       [callback
        (lambda (choice event)
          (save-code-fields! 0) ; it saves the rows of commands before making any changes
          (set! current-command-mode ; sets the current command mode to whatever is chosen now
                (send code-command-choice get-selection))
          (refresh-code-display! 0))])) ; then rebuilds the rows according to the new format (hex/asb)

; now going to make the actual table with the two columns

(define code-table
  (new horizontal-panel%
       [parent code-panel]
       [alignment '(left top)] ; this forces the children of the panel to be placed on top left of this panel
       [spacing 20] ; this is amount of spacing between the columns...20pixels
       [stretchable-height #f])) ; this forced racket to not auto strech the columns and keep them the size they should be

(define code-address-column ; address column
  (new vertical-panel%
       [parent code-table]
       [alignment '(left top)] ; this forces the children of the panel to be placed on top left of this panel
       [stretchable-width #f] ; this forced racket to not auto strech the columns and keep them the size they should be
       [stretchable-height #f])) ; this forced racket to not auto strech the columns and keep them the size they should be

(define code-command-column ; command column
  (new vertical-panel%
       [parent code-table]
       [alignment '(left top)] ; this forces the children of the panel to be placed on top left of this panel
       [stretchable-width #f] ; this forced racket to not auto strech the columns and keep them the size they should be
       [stretchable-height #f])) ; this forced racket to not auto strech the columns and keep them the size they should be

; adding title or headers for these columns now

(new message%
     [parent code-address-column]
     [label "Address"]
     [auto-resize #t])

(new message%
     [parent code-command-column]
     [label "Command"]
     [auto-resize #t])

; now going to make function to display addresses

(define (code-address-display address)
  (cond
    [(= (send code-address-choice get-selection) 0) ; seeing which option is chosen from the dropdown
     (format "~a" address)] ; if dec
    [else
     (hex-display address)])) ; if hex

; making some fake placeholder functions to convert hex-asb and asb-hex which we can later replace with actual functions

(define (hex->assembly command)
  (format "ASM(~a)" (hex-display command))) ; if input is 0x003 it should give ASM(0x003) --> fake as hell i know but still need it for testing my code display

(define (assembly->hex assembly-string) ; this returns hex for asb but for now it just gives 0 --> once again very fake but need it for testing
  0)

; now going to make function to display the commands

(define (code-command-display command)
  (cond
    [(= current-command-mode 0) ; checks if we need command in asb or hex
     (hex-display command)] ; if hex
    [else
     (hex->assembly command)])) ; if asb

; as the command column is editable, we need to pars our hex string values as numbers

(define (remove-hex-prefix s)
  (cond
    [(regexp-match? #rx"^(0x|0X)" s) ; this checks if the string starts from 0x or 0X -> learnt from AI
     (substring s 2)] ; if it does then remove first 2 characters
    [else s])) ; if not then keep same

(define (hex-string->number s) ; this converts the hex string to a number
  (string->number
   (remove-hex-prefix s) 16)) ; this removes the 0x/0X and then reads the string as base 16

; making address labels now

(define (make-code-address-labels i)
  (cond
    [(= i code-row-count) '()] ; stop once we reach max row count
    [else
     (cons (new message%
                [parent code-address-column]
                [label (code-address-display (vector-ref code-address-values i))] ; gets address i and displays it as needed (dec or hex)
                [auto-resize #t])
           (make-code-address-labels (+ i 1)))])) ; recursion

(define code-address-labels ; makes the labels
  (make-code-address-labels 0))

; now making the editable command fields

(define (make-code-command-fields i)
  (cond
    [(= i code-row-count) '()] ; stop once we reach max row count
    [else
     (cons (new text-field%
                [parent code-command-column]
                [label #f] ; no label
                [init-value (code-command-display (vector-ref code-command-values i))] ; this adds all the initial values by taking them from a list using recursion
                [min-width 160])
           (make-code-command-fields (+ i 1)))]))

(define code-command-fields
  (make-code-command-fields 0))

; function to update row

(define (set-code-row! row address command) ; row -> which row, address -> the adresss we need in that row, command -> the command we need in that row
  (vector-set! code-address-values row address) ; change the list internally with new address
  (vector-set! code-command-values row command) ; change the list internally with new command

  (define address-label
    (list-ref code-address-labels row)) ; getting current address row 

  (define command-field
    (list-ref code-command-fields row)) ; getting current command in that row

  (send address-label set-label
        (code-address-display address))

  (send command-field set-value
        (code-command-display command)))

; saving an edited command row --> reading what the user typed and saving it

(define (save-code-row! row)
  (define command-field
    (list-ref code-command-fields row)) ; getting the field where the text is written

  (define typed-text
    (send command-field get-value)) ; reads the text inside that field

  (define new-hex-value
    (cond
      [(= current-command-mode 0) ; if hex
       (hex-string->number typed-text)] ; parse text as hex
      [else
       (assembly->hex typed-text)])) ; if not, then convert it from assembly to hex
                                                                     ;--> '''the function for this right now is fake, will add proper function when we have it in backend'''
  (when new-hex-value ; learnt from ai, when is needed here so our GUI does not crash if the conversion to hex failed
    (vector-set! code-command-values row new-hex-value))) ; updates the list of commands

; now making a function which will save all command fields when we switch from hex to asb or asb to hex

(define (save-code-fields! i)
  (cond
    [(= i code-row-count) (void)]
    [else
     (save-code-row! i)
     (save-code-fields! (+ i 1))]))

; refreshing/re-building all code rows

(define (refresh-code-display! i)
  (cond
    [(= i code-row-count) (void)]
    [else
     (set-code-row! i (vector-ref code-address-values i) (vector-ref code-command-values i)) ; takes the row numbers using i, then using that it finds the address and command from value lists, then re-builds the row using this
     (refresh-code-display! (+ i 1))])) ; recursion




; register panel starts here
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


(define (ascii-display value) ; helper function for converting integer to ASCII
  (if (and (integer? value)
           (>= value 32)
           (<= value 126)) ; we only do 32-126 because that is the ASCII numbers for printable characters, rest may mess up things like int=10 is a new line
      (string (integer->char value))
      "-")) ; if it is not between 32-126, then we just print "-"


; making function to create labels for the reg column

(define (register-name-display i) ; makes sure that Reg14 and Reg15 are FP and SP respectively
  (cond
    [(= i 14) "R14/FP"]
    [(= i 15) "R15/SP"]
    [else (format "R~a" i)]))

(define (make-reg-name-labels i)
  (cond
    [(= i 16) '()]
    [else
     (cons (new message%
                [parent reg-name-column]
                [label (register-name-display i)]
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
        ;(set-code-display!
        (set-code-row! 0 0 #xE111)
        (set-code-row! 1 1 #xE219)
        (set-code-row! 2 2 #xA312)
        (set-code-row! 3 3 #xA111)
        (set-code-row! 4 4 #xB412)
        (set-code-row! 5 5 #xB521)
        (set-code-row! 6 6 #xB114)
        (set-code-row! 7 7 #x0002)
         "'the code in the file'")])


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