#lang racket
(require racket/gui/base racket/string "HERA-api.rkt")

 
(define frame (new frame% ; this creates the full window on which we will make the visualizer on
  [label "IRIS (by Muhammad Bin Nauman and David Wonnacott)"]
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


; Dave overlooked this and then wrote it too; later renamed  to hex-display and just commented this out
; learnt about this later --> (~r #:base 16 #:min-width 4 #:pad-string "0" 42) --> "002a"
;(define (hex-display value) ; this converts a number to hex
;  (string-append
;   "0x"
;   (pad-left (string-upcase (number->string value 16)) 4 #\0))) ; converting to hex here ; string-upcase is used to change letters to uppercase



(define code-panel (new group-box-panel%
                      [parent content]
                      [label "Code / File"]))


(define code-row-count 8) ; number of code rows shown for now = 8

(define code-address-values ; this stores addresses for each row
  (make-vector code-row-count 0)) ; vector: fixed size list

(define code-command-values ; this stores the command values for each row as hex
  (make-vector code-row-count 0))

(define current-command-mode 0) ; 0 is for hex, 1 is for assembly
(define current-address-mode 1) ; 0 is for dec, 1 is for hex


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
       [spacing 25]
       [stretchable-height #f])) ; this forced racket to not auto strech the code option panel row and keep it the size just needed for dropdown menu

(define code-address-choice ; drop down for address column
  (new choice%
       [parent code-options-panel]
       [label "Address"]
       [choices '("Dec" "Hex")]
       [selection 1] ; auto choose hex initially
       [callback
        (lambda (choice event)
          (save-code-fields! 0) ; first saves the fields
          (set! current-address-mode 
                (send code-address-choice get-selection)) ; sets the current address code to the new selections chosen
          (refresh-code-display! 0))])) ; rebuilds the rows according to the new selection


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

; now going to make the panel with the code lines

(define code-lines-panel
  (new vertical-panel%
       [parent code-panel]
       [alignment '(left top)]
       [stretchable-width #f]
       [stretchable-height #f]))

; now going to make function to display addresses

(define (code-address-display address)
  (cond
    [(= current-address-mode 0) ; checks if we need dec or hex
     (format "~a" address)] ; if dec
    [else
     (hex-display address)])) ; if hex

; now going to make a function to parse decimals like we did later in this file for hex so dont need to make it for hex

(define (dec-string->number s) ; changes "15" to 15 so string to int 
  (string->number s 10))

; making some fake placeholder functions to convert hex-asb and asb-hex which we can later replace with actual functions

(define (hex->assembly command)
  (format "ASM(~a)" (hex-display command))) ; if input is 0x003 it should give ASM(0x003) --> fake as hell i know but still need it for testing my code display

(define (assembly->hex assembly-string) ; this returns hex for asb but for now it just gives 0 --> once again very fake but need it for testing
  0)

; now going to make function to display the commands

(define (code-command-display address command)
  (cond
    [(= current-command-mode 0) ; checks if we need command in asb or hex
     (hex-display command)] ; if hex
    [else
     (get-code-asm address)])) ; if asb -- using function from api file now

; as the command column is editable, we need to pars our hex string values as numbers

(define (remove-hex-prefix s)
  (cond
    [(regexp-match? #rx"^(0x|0X)" s) ; this checks if the string starts from 0x or 0X -> learnt from AI
     (substring s 2)] ; if it does then remove first 2 characters
    [else s])) ; if not then keep same

(define (hex-string->number s) ; this converts the hex string to a number
  (string->number
   (remove-hex-prefix s) 16)) ; this removes the 0x/0X and then reads the string as base 16

; making a helper function to add a pointer on the address we are currently on

(define (pc-pointer-display address)
  (cond
    [(= address (get-PC)) "> "] ; if the current address is this then return "> "
    [else "  "])) ; else return a space

; making the line with the full address + command in it

(define (code-line-display address command)
  (string-append (pc-pointer-display address) (code-address-display address) " " (code-command-display address command)))


; now making the editable fields in the code box/panel

(define (make-code-line-fields i)
  (cond
    [(= i code-row-count) '()]
    [else
     (cons (new text-field%
                [parent code-lines-panel]
                [label #f]
                [init-value (code-line-display (vector-ref code-address-values i) (vector-ref code-command-values i))] ; initial value in the fields
                [min-width 300])
           (make-code-line-fields (+ i 1)))]))

(define code-line-fields ; initialization
  (make-code-line-fields 0))

; now making a function to set or update one line

(define (set-code-row! row address command) ; 3 inputs
  (vector-set! code-address-values row address) ; setting the new address in the address value list
  (vector-set! code-command-values row command) ; setting the new command in the command value list

  (define code-line-field
    (list-ref code-line-fields row)) ; new code line field

  (send code-line-field set-value
        (code-line-display address command))) ;setting value of the new code line field using code line display function

; function to split line into address and command using the space in between them

(define (split-code-line line)
  
  (define cleaned-line
    (string-trim line)) ; this works by removing extra spaces from the start and end of the line

  (define space-match
    (regexp-match-positions #rx" +" cleaned-line)) ; learnt this from ai, the #rx" +" means that one or more spaces 

  (cond ; seeing if a space was found or not
    [space-match ; if space was found
     
     (define space-position 
       (first space-match)) ; as space match is a list of the space postions we got, we see the first position in that list and save it in space-position, it would look something like '(6 . 7) which
     ;means it starts at 6 and ends at 7

     (define space-start 
       (car space-position)) ; car gives the first of the list so it will be 6 if we continue the last example

     (define space-end 
       (cdr space-position)) ; cdr gives the last of the list so it will be 7 if we continue the last example

     (values ; learnt from ai, usually functions return one value but this 'values' lets us return more than one value
      (substring cleaned-line 0 space-start) ; gives everything till the first space
      (string-trim
       (substring cleaned-line space-end)))] ; gives everything everything after the first space, and trims it so if there are extra spaces after first space, they are removed

    [else
     (values cleaned-line "")])) ; this happens if no space, helps the gui not crash


; now will make a function so save the edited code row by first splitting, then converting and then savind in the list/vector

(define (save-code-row! row)
  (define code-line-field
    (list-ref code-line-fields row)) ; getting the code row using row number

  (define typed-line
    (send code-line-field get-value)) ; getting the value in that code row

  (define-values (address-text command-text) ; splitting line and giving value to both the parameters
    (split-code-line typed-line))

  (define new-address 
    (cond
      [(= current-address-mode 0) ; checking current format mode
       (dec-string->number address-text)] ; if dec
      [else
       (hex-string->number address-text)])) ; if hex

  (define new-command
    (cond
      [(string=? command-text "") #f] ; if not command then we keep the old command
      [(= current-command-mode 0) ; checking current format mode
       (hex-string->number command-text)] ; if hex
      [else
       (assembly->hex command-text)])) ; if assembly

  (when new-address ; this is so gui doesnt fail
    (vector-set! code-address-values row new-address))

  (when new-command ; this is so gui doesnt fail
    (vector-set! code-command-values row new-command)))


; now making a function to save all the rows at once -- its going to be a basic recursive function

(define (save-code-fields! i) 
  (cond
    [(= i code-row-count) (void)]
    [else
     (save-code-row! i)
     (save-code-fields! (+ i 1))]))

; now making a function to rebuild all the rows or refresh them when format changes etc

(define (refresh-code-display! i)
  (cond
    [(= i code-row-count) (void)]
    [else
     (set-code-row! i (vector-ref code-address-values i) (vector-ref code-command-values i))
     (refresh-code-display! (+ i 1))]))

; making a function to get the address we will start with so can put a pointer on which address we are on in the code panel

(define (code-window-start-address)
  (max 0 (- (get-PC) 3))) ; using logic Dave told

; making a refresh function for the back end to test the api functions with the gui

(define (refresh-code-from-backend! row)
  (define start-address
           (code-window-start-address))
  
  (cond
    [(= row code-row-count) (void)]
    [else
     (define code-address
       (+ start-address row)) ; row is the visible row we can see in the gui and code-address is the actual address of that row in the backend...so like row 1 can have code-address 3
     ; addition is done so we are on the correct address, suppose we start from address 5 and are on row 2 in gui then the code-address we are actually going to execute now will be address 7
     
     (set-code-row! row code-address (get-code code-address)) ; uses get-code from api which takes the address number and gives the command on that address
     (refresh-code-from-backend! (+ row 1))]))




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

; making a function to be able to refresh the registers from the backend values usin step, instead of our hardcoded ones in step button

(define (refresh-registers-from-backend! i)
  (cond
    [(= i 16) (void)]
    [else
     (set-register-value! i (get-register i)) ; uses get-register from api
     (refresh-registers-from-backend! (+ i 1))]))
  

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

; making new panel for the size field

(define memory-size-panel
  (new horizontal-panel%
       [parent memory-panel]
       [alignment '(left top)]
       [stretchable-height #f])) ; forces racket to not autostrech and keep it the size it needs to be

; making the size field now which will be typeable AND the button to enter the size because if we do not make a button and we wanna enter 100 then it will first do 1 then 10 and then 100

(define memory-size-field
  (new text-field%
       [parent memory-size-panel]
       [label "Rows"]
       [init-value "8"] ; this needs to match the memory-row-count or else there will be a contradiction in when the app is run
       [min-width 80]
       [stretchable-width #f]))

(new button%
     [parent memory-size-panel]
     [label "Enter"]
     [callback
      (lambda (button event)
        (apply-memory-size!))]) ;will create this function later, adding it before making it is okay because it will only take place when button is pressed

; horizontal panel for checkboxes to show or hide value format columns

(define memory-options-panel
  (new horizontal-panel%
       [parent memory-panel]
       [alignment '(left top)] ; this forces the children of the panel to be placed on top left of this panel
       [stretchable-height #f])) ; this forced racket to not auto strech the checkbox row and keep it the size just needed for the check boxes

; adding scoll feature in memory panel

(define memory-scroll
  (new vertical-panel%
       [parent memory-panel]
       [style '(vscroll)] ; auto means it will show scroll whenever it is must needed -- removed auto scroll later cuz it was bugging and showing empty panel sometimes during testing :(
       [alignment '(left top)]))


(define memory-table (new horizontal-panel%
                          [parent memory-scroll]
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

(define mem-address-header
  (new message%
     [parent mem-address-column]
     [label "Address"]
     [auto-resize #t]))

(define mem-dec-header
  (new message%
     [parent mem-dec-column]
     [label "Dec"]
     [auto-resize #t]))

(define mem-hex-header
  (new message%
     [parent mem-hex-column]
     [label "Hex"]
     [auto-resize #t]))

(define mem-ascii-header
  (new message%
     [parent mem-ascii-column]
     [label "ASCII"]
     [auto-resize #t]))

; setting initial number of memory rows

(define memory-row-count 8)

; making function to create labels for the memory column

(define (make-memory-address-labels i)
  (cond
    [(= i memory-row-count) '()]
    [else
     (cons (new message%
                [parent mem-address-column]
                [label (hex-display i)]
                [auto-resize #t])
           (make-memory-address-labels (+ i 1)))]))

; making function to create labels for the format columns

(define (make-memory-value-labels i parent-column starting-text)
  (cond
    [(= i memory-row-count) '()]
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

; rebuilding rows of memory panel in one function

(define (rebuild-memory-rows!)


         ; clear all rows to just show the headers first "Address Dec Hex ASCII" <-- this way
         (send mem-address-column change-children
               (lambda (children)
                 (list mem-address-header)))
         (send mem-dec-column change-children
               (lambda (children)
                 (list mem-dec-header)))
         (send mem-hex-column change-children
               (lambda (children)
                 (list mem-hex-header)))
         (send mem-ascii-column change-children
               (lambda (children)
                 (list mem-ascii-header)))

         ; then adding the label values to each column according to the new value of number of rows (memory-row-count)
         (set! mem-address-labels
               (make-memory-address-labels 0))
         (set! mem-dec-labels
               (make-memory-value-labels 0 mem-dec-column "0"))
         (set! mem-hex-labels
               (make-memory-value-labels 0 mem-hex-column "0x0000"))
         (set! mem-ascii-labels
               (make-memory-value-labels 0 mem-ascii-column "-")))
  
         

; making a function to refresh mameory using backend function get-data

(define (refresh-memory-from-backend! i)
  (cond
    [(= i memory-row-count) (void)]
    [else
     (set-memory-value! i (get-data i)) ; get-data is from the api so we can actual step through code instead of fake and hardcoded values
     (refresh-memory-from-backend! (+ i 1))]))


(define (reset-memory! i)
  (cond
    [(= i memory-row-count) (void)]
    [else
     (set-memory-value! i 0)
     (reset-memory! (+ i 1))]))

(define (reset-display!)
  (reset-registers! 0)
  (reset-memory! 0))


; now making the apply size function

(define (apply-memory-size!) ; runs when enter button is pressed
         (define n (string->number (send memory-size-field get-value))) ; taking value added and turning it into a number

         (set! memory-row-count n) ; changing memory row count
         (rebuild-memory-rows!) ; rebuilding the rows
         (refresh-memory-from-backend! 0)) ; and then refreshing the memory columns according to the backend values


; adding a program counter near the buttons

(define pc-display
  (new message%
       [parent toolbar]
       [label "PC: 0x0000"]
       [auto-resize #t]))

(define (refresh-pc-from-backend!)
  (send pc-display set-label ; setting value to the pc-display label
        (string-append "PC: " (hex-display (get-PC))))) ; gets the value from the backend function get-PC

; making a default label for flags

(define flags-display
  (new message%
       [parent toolbar]
       [label "Flags: S=0 Z=0 V=0 C=0 CB=0"]
       [auto-resize #t]))

;making a boolean to bit function for flags

(define (bool->bit value)
  (if value "1" "0")) ; value will be #f or #t

; making a function to get the flags using get-flag and write them for gui in single line using string-append

(define (flags-display-string)
  
  (define flags
    (get-flags)) ; using this from api to get the flags from backend -- it returns a list/vector of the flags like (#f, #t...)

  (string-append ; writing all flags in one line
   "Flags: " "S="  (bool->bit (vector-ref flags 0)) " " "Z="  (bool->bit (vector-ref flags 1)) " " "V="  (bool->bit (vector-ref flags 2)) " "
   "C="  (bool->bit (vector-ref flags 3)) " " "CB=" (bool->bit (vector-ref flags 4))))

; refrshing the flags

(define (refresh-flags-from-backend!)
  (send flags-display set-label
        (flags-display-string)))

; making a single helper function which refreshes everything

(define (refresh-all-from-backend!)
  (refresh-pc-from-backend!)
  (refresh-registers-from-backend! 0)
  (refresh-memory-from-backend! 0)
  (refresh-code-from-backend! 0)
  (refresh-flags-from-backend!))

; buttons

(new button%
     [parent toolbar]
     [label "Run"]
     [callback
      (lambda (button event) ; the button and event are two inputs the function takes, button: the button that was pressed, event: information about the click
        (reset!)
        (load-code! "")
        (load-data! "")
        (refresh-all-from-backend!))])

(new button%
     [parent toolbar]
     [label "Step"]
     [callback
      (lambda (button event) ; the button and event are two inputs the function takes, button: the button that was pressed, event: information about the click
        (step!)
        (refresh-all-from-backend!))])

(new button%
     [parent toolbar]
     [label "Reset"]
     [callback
      (lambda (button event) ; the button and event are two inputs the function takes, button: the button that was pressed, event: information about the click
        ;(displayln "Reset Clicked"))]) ; prints this string when the button is pressed. added this for now to test, can be removed later
        ;(reset-display!))])
        (reset!)
        (refresh-all-from-backend!))])




(send frame show #t) ; this shows the window ; #t means true