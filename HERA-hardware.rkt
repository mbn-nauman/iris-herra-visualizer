#lang racket
(provide memsize wordlim PC flags registers memory-data memory-code reset! load-data! load-code! step! set-step-trace!)
(provide hex-str hera-op%-str)   ; hex-str prints hexidecimal 8-bit or 16-bit numbers 
(provide hera-val? hera-addr? hera-reg-num?)

(require "check.rkt")
(require racket/math) ;; for bitwise-and, etc.

(define debug-HERA-hw #t)
(define HERA-hw-step-trace #t)

(require racket/random) ; temporary, while testing

(define memsize 65536)  ; sometimes we set this to 64 to make testing less annoying
(define wordlim 65536)  ; registers and ram hold values modulo wordlim
(define PC 0)
(define flags       (make-vector 5       #f))
(define registers   (make-vector 16       0))
(define memory-data (make-vector memsize  0))
(define memory-code (make-vector memsize  0))
(define/contract (getf-s)    (-> boolean?) (vector-ref  flags 0))
(define/contract (getf-z)    (-> boolean?) (vector-ref  flags 1))
(define/contract (getf-v)    (-> boolean?) (vector-ref  flags 2))
(define/contract (get-c^!cb) (-> integer?) (if (and  (vector-ref  flags 3) (not (vector-ref  flags 4))) 1 0))  ; effective carry for ADD
(define/contract (get-cvcb)  (-> integer?) (if (or   (vector-ref  flags 3)      (vector-ref  flags 4))  1 0))  ; effective carry for SUB

; (define (setf-s v)  (vector-set! flags 0)) 
(define flag-s-ind  0) (define flag-s-mask #x01)
(define flag-z-ind  1) (define flag-z-mask #x02)
(define flag-v-ind  2) (define flag-v-mask #x04)
(define flag-c-ind  3) (define flag-c-mask #x08)
(define flag-cb-ind 4)

(eprintf " ==> HERA-hardware.rkt warning: overflow (V) flag not being set correctly, will always show as false (v not V) to let tests pass <==\n")
(eprintf " ==> HERA-hardware.rkt warning: INC and DEC only partly implemented (INC with offset 1 works...) <==\n")
(eprintf " ==> HERA-hardware.rkt warning: STORE and many other things totally untested, say UNTESTED in comment or OP name when printed <==\n")

(define/contract (set-step-trace! do-we-trace)
  (->                             boolean?    void?)
  (set! HERA-hw-step-trace do-we-trace))


(define (flags->string) ; for N=5, not _quite_ worth doing something cool with map
  (let ([sep " "])
    (string-append
     (if (vector-ref flags flag-cb-ind) "B " "b ")
     sep
     (if (vector-ref flags flag-c-ind) "C" "c")
     sep
     (if (vector-ref flags flag-v-ind) "V" "v")
     sep
     (if (vector-ref flags flag-z-ind) "Z" "z")
     sep
     (if (vector-ref flags flag-s-ind) "S" "s")
     )))

(define (hera-val? v)
  (and (integer? v) (<= 0 v) (< v wordlim)))
(define (hera-addr? a)
  (and (integer? a) (<= 0 a) (< a memsize)))
(define (hera-reg-num? r)
  (and (integer? r) (<= 0 r) (< r 16)))

;;
;; some bit operations making use of the string-input system that allows 0, 1, _, and letters to define ops
;;

; change non-"care-about-chars" into "miss", then care-about-chars into hit-is-or-nullstr (if != ""), in pattern
; cuts out spaces before and then after making the changes, so characters can be deleted by converting to " "
(define/contract (mask-for care-about-chars miss-is hit-is-or-nullstr pattern)
  (->                      string?          string? string?           string?   hera-val?)
  (let* ([dont-care-about (if (string=? (substring care-about-chars 0 1) "^")
                              (substring care-about-chars 1)
                              (string-append "^" care-about-chars))]
         [misses (list (regexp (string-append "[" dont-care-about "]")) miss-is)]
         [spaces '[#rx" *" ""]]
         [xforms (if (string=? hit-is-or-nullstr "")
                    (list spaces misses spaces)
                    (list spaces
                          misses
                          (list (regexp (string-append "[" care-about-chars "]")) hit-is-or-nullstr)
                          spaces))])
    (string->number
     (string-append "#b" (regexp-replaces pattern xforms)))))

;; select the care-about entries of pattern, from value
;;  e.g., (mask-with "a" "1100 dddd aaaa bbbb" #x1234) gives #x0030, using "b" or "d" gets #x0004 or #x0200, resp.
(define/contract (mask-with care-about pattern value)
  (->                       string?    string? hera-val?  hera-val?)
  (bitwise-and value (mask-for care-about "0" "1" pattern)))



(define (get-n3 i)    (/ (bitwise-and #xF000 i) #x1000))  ; nybble 3, i.e., usually the op-code, except for shifts, etc.
(define (get-n2 i)    (/ (bitwise-and #x0F00 i) #x0100))
(define (get-n1 i)    (/ (bitwise-and #x00F0 i) #x0010))
(define (get-n0 i)    (/ (bitwise-and #x000F i) #x0001))

(define (get-b0 i)       (bitwise-and #x00FF i))

(let ([example-mult "1100 Dddd Aaaa Bbbb"] [example-num #xbcde])
  (check-equal (mask-for  "1"  "0" ""  example-mult) #xC000) ; make all non-1's into 0's, so we can and with this
  (check-equal (mask-for  "0"  "1" ""  example-mult) #xCFFF) ;    all non-0's become 1's, so we can  or with this
  (check-equal (mask-for  "aA" "0" "1" example-mult) #x00F0) ; mask for parameter A
  (check-equal (mask-with "Dd" example-mult #xC532) #x0500)
  (check-equal (mask-with "Aa" example-mult #xC532) #x0030)
  (check-equal (mask-with "Bb" example-mult #xC532) #x0002)
  (check-equal (get-n3 example-num) #xb)
  (check-equal (get-n2 example-num) #xc)
  (check-equal (get-n1 example-num) #xd)
  (check-equal (get-n0 example-num) #xe)
)



; I've not done much with Racket classes; relying on https://docs.racket-lang.org/guide/classes.html and Claude
;    -Dave W
(define hera-op%
  (class object%
    (init pattern name action to-string)
    (define _p pattern)
    (define _n name)
    (define _a action)
    (define _s to-string)
    
    (super-new)
    
    (define and-mask  (mask-for "1"  "0"  "" pattern)) ; see examples in tests above
    (define  or-mask  (mask-for "0"  "1"  "" pattern))
    (define/public (match? me)   (and (= (bitwise-and me and-mask) and-mask)
                                      (= (bitwise-ior me  or-mask)  or-mask)))
    (define/public (doit!  instr) (if (match? instr) (_a _p instr)      (error (format "Instr #x~x doesn't match op ~s\n" instr _p))))
    (define/public (str    instr) (if (match? instr) (_s _p instr _n)   (error (format "Instr #x~x doesn't match op ~s for 'str'\n" instr _p))))
    
    (define/public (str-debug-verbose) (format "HERA op ~s: and-mask=#x~x or-mask=#x~x" _p and-mask or-mask))

    (let ([n3 (get-n3 and-mask)])  ; or-mask would also work; here, we use the leftmost 4 bits to start the dispatch...
      (vector-set! hera-op%-dispatch-table n3 (cons this (vector-ref hera-op%-dispatch-table n3))))
    ))

; Fake class-field
(define hera-op%-dispatch-table
  (make-vector 16 `()))    ; a vector of _lists_ of ops that can match this 4-bit prefix, e.g., element 0x0E has only SETLO, 0x03 has a lot of stuff

(define/contract (hera-op%-dispatch instr)
  (->                               hera-val? void?)
  (let* ([n3      (get-n3 instr)]
         [ops     (vector-ref hera-op%-dispatch-table n3)]
         [matches (filter (λ (op) (send op match? instr)) ops)])
    (if (not (empty? matches))
        (let ([op (first matches)])
          (when (> (length matches) 1)
            (eprintf " ==> HERA-hardware.rkt inconsistency: more than one match in group ~a for #x~x <==\n" n3 instr))
          (send op doit!  instr))
        (let ()
          (printf "Illegal instruction (no op implemented): #x~x\n" instr)
          (inc-PC!)))))
(define/contract (hex-str num         [hits 4]  [prefix "0x"])
  (->*                    (hera-val?) (integer? string?)       string?)
  (string-append prefix
                 (string-upcase (~r num #:base 16 #:min-width hits #:pad-string "0"))))  ; thanks, Claude, for reading all those manuals ... THIS IS COPY-PASTED CODE
  
(define/contract (hera-op%-str instr)
  (->                          hera-val? string?)
  (let* ([n3      (get-n3 instr)]
         [ops     (vector-ref hera-op%-dispatch-table n3)]
         [matches (filter (λ (op) (send op match? instr)) ops)])
    (match (length matches)
      [1    (send (first matches) str instr)]
      [0    (format "ASM(~a)  // no matching instruction found" (hex-str instr))]
      [else (format "ASM(~a)  // WARNING multiple matching operations found for this instruction" (hex-str instr))])))
        



(define (reset!)
  (set! PC 0)
  (set! flags       (make-vector 5       #f))
  (set! registers   (make-vector 16       0))
  (set! memory-data (make-vector memsize  0))
  (set! memory-code (make-vector memsize  0)))


(define/contract (get-reg  r)
  (->                      hera-reg-num? hera-val?)
  (vector-ref registers r))
(define/contract (set-reg! r             v)
  (->                      hera-reg-num? hera-val? void?)
  (when (> r 0)
    (vector-set! registers r v)))

(define/contract (get-flag  flag-ind)
  (->                       integer?   boolean?)
  (vector-ref  flags flag-ind))
(define/contract (set-flag! flag-ind  value)
  (->                       integer?   boolean?  void?)
  (vector-set! flags flag-ind value))

(define (inc-PC! [amount 1])
  (set! PC (modulo (+ PC amount) wordlim)))

;
; for now, also-set-flags is 0 for "no flags", #xff for all flags;
;          could extend to allow other bits patterns for which flags to set
; Note that value may be outside of HERA's signed or unsigned range, e.g., 17-25 can be 8, not #xFFF8
(define/contract (set-reg-inc-PC! reg             value       [also-set-flags #x0f])
  (->*                           (hera-reg-num?   integer?)   (integer?)            void?)
  (let ([hera-val (modulo value wordlim)])
    (when debug-HERA-hw
      (printf "set-reg-inc-PC for R~a <-- ~a/~a (PC ~a)\n" reg value hera-val PC))
    (set-reg! reg hera-val)
    (when (> also-set-flags 0)
      (when (bitwise-and also-set-flags flag-s-mask)
        (set-flag! flag-s-ind
                   (> (bitwise-and value (/ wordlim 2)) 0))
        (when (not (= (bitwise-and value (/ wordlim 2)) (bitwise-and hera-val (/ wordlim 2)))) (eprintf "Hmmm, questionable sign flag for ~a/~a" value hera-val)))
      (when (bitwise-and also-set-flags flag-z-mask)
        (set-flag! flag-z-ind
                   (= value 0)))
      (when (bitwise-and also-set-flags flag-v-mask)
        (set-flag! flag-v-ind
                   (not (= (bitwise-and value wordlim) (* 2 (bitwise-and value (/ wordlim 2)))))))
      (when (bitwise-and also-set-flags flag-c-mask)
        (set-flag! flag-c-ind
                   (> (bitwise-and value wordlim) 0))))
    (inc-PC!)
  ))


;;
;; Now, the HERA instructions themselves
;;   These auto-enroll in the dispatch table,
;;   so there's no need to capture them with ; (define ...
;;   and thus we don't want a lot of chatter about their un-captured values, turning off current-output
; Claude suggested parameterize would work with current-print, to prevent chatter while scanning this file
; https://docs.racket-lang.org/reference/parameters.html#%28form._%28%28lib._racket%2Fprivate%2Fmore-scheme..rkt%29._parameterize%29%29
; (parameterize ([current-print void])
;; parameterize no longer needed because I have a "let" inside it, and that returns void, which parameterize would also have done

(let ([hera-op%-arith-to-str    ; Use this for to-string for all arithmetic, but just write it once
       (λ (pattern instr name)
         (format "~a(R~x, R~x,R~x)" name (get-n2 instr) (get-n1 instr) (get-n0 instr)))]
      [hera-op%-as-ASM
       (λ (pattern instr name)
         (format "ASM(0x~x)  // Dave too lazy to make ~a pretty" (hex-str instr) name))]  ; for when I'm lazy
      [just-sz-flags (bitwise-ior flag-s-mask flag-z-mask)]
      [no-flags 0])
  
  ;;; 0xF***  SETHI     
  (new hera-op% [pattern "1111 dddd vvvvvvvv"] [name "SETLO"]
       [action (λ (pattern instr)
                 (let* ([v-shifted   (* (get-b0 instr) #x100)]
                        [d-reg       (get-n2 instr)])
                   (set-reg-inc-PC! d-reg
                                    (bitwise-ior (bitwise-and (get-reg d-reg) #x00ff) v-shifted)
                                    no-flags)))]
       [to-string (λ (pattern instr op)
                    (let ([vvvvvvvv (get-b0 instr)]
                          [d-reg    (get-n2 instr)])
                      (format "SETHI(R~x, ~a)  // UNTESTED" d-reg  (hex-str vvvvvvvv 2))))])

  ;;; 0xE***  SETLO
  (new hera-op% [pattern "1110 dddd vvvvvvvv"] [name "SETLO"]
       [action (λ (pattern instr)
                 (let* ([v_8bit      (get-b0 instr)]
                        [v_extended  (if (> v_8bit #x007f) (bitwise-ior v_8bit #xff00) v_8bit)])
                   (set-reg-inc-PC! (get-n2 instr) v_extended no-flags)))]
       [to-string (λ (pattern instr op)
                    (let ([vvvvvvvv (get-b0 instr)]
                          [d-reg    (get-n2 instr)])
                      (if (> (bitwise-and vvvvvvvv #x80) 0)
                          (format "SETLO(R~x, -~a)" d-reg (- #xff vvvvvvvv))
                          (format "SETLO(R~x, ~a)"  d-reg         vvvvvvvv ))))])

  ;;; 0xD*** XOR
  (new hera-op% [pattern "1101 dddd aaaa bbbb"] [name "XOR_UNTESTED"]
       [action (λ (pattern instr)
                 (set-reg-inc-PC! (get-n2 instr)
                                  (bitwise-xor (get-reg (get-n1 instr)) (get-reg (get-n0 instr)))
                                  just-sz-flags))]
       [to-string hera-op%-arith-to-str])
  
  ;;; 0xC*** MUL
  (new hera-op% [pattern "1100 dddd aaaa bbbb"] [name "MUL_UNTESTED"]
       [action (λ (pattern instr)
                 (set-reg-inc-PC! (get-n2 instr) (* (get-reg (get-n1 instr))
                                                    (get-reg (get-n0 instr)))))]
       [to-string hera-op%-arith-to-str])
  
  ;;; 0xB*** SUB
  (new hera-op% [pattern "1011 dddd aaaa bbbb"] [name "SUB"]
       [action (λ (pattern instr)
                 (set-reg-inc-PC! (get-n2 instr) (+ (get-reg (get-n1 instr))
                                                    (- (- wordlim 1) (get-reg (get-n0 instr))) ; n0 bit-flipped
                                                    (get-cvcb))))]
       [to-string hera-op%-arith-to-str])
  
  ;;; 0xA*** ADD
  (new hera-op% [pattern "1010 dddd aaaa bbbb"] [name "ADD"]
       [action (λ (pattern instr)
                 (set-reg-inc-PC! (get-n2 instr) (+ (get-reg (get-n1 instr))
                                                    (get-reg (get-n0 instr))
                                                    (get-c^!cb))))]
       [to-string hera-op%-arith-to-str])

  ;;; 0x9*** OR
  (new hera-op% [pattern "1001 dddd aaaa bbbb"] [name "OR_UNTESTED"]
       [action (λ (pattern instr)
                 (set-reg-inc-PC! (get-n2 instr)
                                  (bitwise-ior (get-reg (get-n1 instr)) (get-reg (get-n0 instr)))
                                  just-sz-flags))]
       [to-string hera-op%-arith-to-str])

  ;;; 0x8*** AND
  (new hera-op% [pattern "1000 dddd aaaa bbbb"] [name "AND_UNTESTED"]
       [action (λ (pattern instr)
                 (set-reg-inc-PC! (get-n2 instr)
                                  (bitwise-and (get-reg (get-n1 instr)) (get-reg (get-n0 instr)))
                                  just-sz-flags))]
       [to-string hera-op%-arith-to-str])

  ;;; 0x[7654]*** STORE/LOAD
  (let ([LOAD-doit
         (λ (pattern instr)
           (let ([offset   (+ (get-n1 instr) (/ (bitwise-and instr #x1000) #x1000))]
                 [reg      (get-n2 instr)])
             (set-reg-inc-PC!
              (get-n2 instr)
              (vector-ref memory-data (+ offset (get-reg (get-n0 instr))))
              (bitwise-ior flag-s-mask flag-z-mask))))]
        [LOAD-to-string
         (λ (pattern instr op)
           (let ([offset   (+ (get-n1 instr) (/ (bitwise-and instr #x1000) #x1000))]
                 [reg      (get-n2 instr)])
             (format "LOAD(R~x, ~a,R~x)" reg offset (get-n0 instr))))])
    (new hera-op% [pattern "0100 dddd oooo bbbb"] [name "LOAD"] [action LOAD-doit] [to-string LOAD-to-string])
    (new hera-op% [pattern "0101 dddd oooo bbbb"] [name "LOAD"] [action LOAD-doit] [to-string LOAD-to-string]) ; big-offset load
    )
  (let ([STORE-doit  ; WARNING: COMPLETELY UNTESTED  ToDo: test, duh
         (λ (pattern instr)
           (let ([offset   (+ (get-n1 instr) (/ (bitwise-and instr #x1000) #x1000))])
             (vector-set! memory-data (+ offset (get-reg (get-n0 instr))) (get-reg (get-n2 instr)))))]
        [STORE-to-string
         (λ (pattern instr op)
           (let ([offset   (+ (get-n1 instr) (/ (bitwise-and instr #x1000) #x1000))])
             (format "STORE(R~x, ~a,R~x)  // UNTESTED" (get-n2 instr) offset (get-n0 instr))))])
    (new hera-op% [pattern "0110 dddd oooo bbbb"] [name "STORE"] [action STORE-doit] [to-string STORE-to-string])
    (new hera-op% [pattern "0111 dddd oooo bbbb"] [name "STORE"] [action STORE-doit] [to-string STORE-to-string]) ; big-offset
    )

  ;;; 0x3***   A BUNCH OF THINGS GATHERED TO GETHER IN A CAVE AND GROOVING WITH A PICT
  (let  ; surely we'll need some variables?  In any case, this will indent to hightight grouping
      ([flag-blender (λ (blend-bits bit-vec)
                       (vector-set! flags flag-s-ind  (blend-bits (vector-ref flags flag-s-ind)  (> (bitwise-and bit-vec flag-s-mask) 0)))
                       (vector-set! flags flag-z-ind  (blend-bits (vector-ref flags flag-z-ind)  (> (bitwise-and bit-vec flag-z-mask) 0)))
                       (vector-set! flags flag-v-ind  (blend-bits (vector-ref flags flag-v-ind)  (> (bitwise-and bit-vec flag-v-mask) 0)))
                       (vector-set! flags flag-c-ind  (blend-bits (vector-ref flags flag-c-ind)  (> (bitwise-and bit-vec flag-c-mask) 0)))
                       (vector-set! flags flag-cb-ind (blend-bits (vector-ref flags flag-cb-ind) (> (bitwise-and bit-vec #x10) 0))))]
       [flag-grabber (λ (instr) (+ (/ (get-n3 instr) #x10) (get-n0 instr)))]
       )      
    ;;     0x3*[0-5]* SHIFTS

    
    ;;     0x3*[6]* FON/FOFF/FSET/FSET4
    (new hera-op% [pattern "0011 000V 0110 vvvv"] [name "FON_UNTESTED"]
         [action (λ (pattern instr)
                   (flag-blender bitwise-ior (flag-grabber instr)))]
         [to-string hera-op%-as-ASM])
    (new hera-op% [pattern "0011 100V 0110 vvvv"] [name "FOFF_UNTESTED"]
         [action (λ (pattern instr)
                   (flag-blender (λ (f v) (and f (not v))) (flag-grabber instr)))]
         [to-string hera-op%-as-ASM])
    (new hera-op% [pattern "0011 010V 0110 vvvv"] [name "FSET5_UNTESTED"]
         [action (λ (pattern instr)
                   (flag-blender (λ (f v) v) (flag-grabber instr)))]
         [to-string hera-op%-as-ASM])
    (new hera-op% [pattern "0011 110V 0110 vvvv"] [name "FSET4_UNTESTED"]
         [action (λ (pattern instr)
                   (flag-blender (λ (f v) v) (bitwise-and #x0f (flag-grabber instr))))]
         [to-string hera-op%-as-ASM])

    ;;     0x3*[7]* SAVEF and RSTRF  
    
    ;;     0x3*[8+]*  INC and DEC
    (new hera-op% [pattern "0011 dddd 11ee eeee"] [name "DEC"]
         [action (λ (pattern instr)
                   (let ([eeeeee (bitwise-and instr #x3f)]
                         [d-reg  (get-n2 instr)])
                     (set-reg-inc-PC! d-reg (+ (- (- wordlim 1) (+ eeeeee 1)) ; eeeeee+1 bit-flipped, i.e., just add 1 below to have -(eeeeee+1)
                                               (get-reg d-reg)
                                               1))))]
         [to-string (λ (pattern instr op)
                      (let ([eeeeee (bitwise-and instr #x3f)]
                            [reg      (get-n2 instr)])
                        (format "DEC(R~x, ~x)" reg (+ 1 eeeeee))))])
    (new hera-op% [pattern "0011 dddd 10ee eeee"] [name "INC"]
         [action (λ (pattern instr)
                   (let ([eeeeee (bitwise-and instr #x3f)]
                         [d-reg  (get-n2 instr)])
                     (set-reg-inc-PC! d-reg (+ eeeeee 1
                                               (get-reg d-reg)
                                               0))))]
         [to-string (λ (pattern instr op)
                      (let ([eeeeee (bitwise-and instr #x3f)]
                            [reg      (get-n2 instr)])
                        (format "INC(R~x, ~x)" reg (+ 1 eeeeee))))])
    )
  ;;; 0x3*** ends here

  ;;; 0x2***   CALL, RETURN, INTERRUPT HANDLING
  (let ()  ; surely we'll need some variables?  In any case, this will indent to hightight grouping
    (void)
    )
  ;;; 0x2*** ends here

  ;;; 0x[01]*** Branches
  ;;; table of names copied from the HERA2_4_0.pdf doc. then edited
  (let* ([hera-op%-branch-table  ; hobt is pronounced like "hobbit"
          (list->vector
           (list [list "BR"   (λ (c v z s)      #t)]         ; 0000 = unconditional branch
                 [list "ILLEGAL"  (λ (c v z s)  (not #t))]   ; 0001 = not defined; for symmetry, never
                 [list "BL"   (λ (c v z s)      (xor s v))]  ; 0010      (s ⊕ v)
                 [list "BGE"  (λ (c v z s) (not (xor s v)))] ; 0011      (s ⊕ v) ′
                 [list "BLE"  (λ (c v z s)      (or (xor s v) z))]   ;  ((s ⊕ v) ∨ z)
                 [list "BG"   (λ (c v z s) (not (or (xor s v) z)))]  ;  ((s ⊕ v) ∨ z) ′
                 [list "BULE" (λ (c v z s)      (or (not c) z))]     ;   (c′ ∨ z)
                 [list "BUG"  (λ (c v z s) (not (or (not c) z)))]    ;   (c′ ∨ z) ′
                 [list "BZ"   (λ (c v z s)       z)]    ;  z   branch condition 1000
                 [list "BNZ"  (λ (c v z s) (not  z))]   ;  z ′
                 [list "BC"   (λ (c v z s)       c)]    ;  c
                 [list "BNC"  (λ (c v z s) (not  c))]   ;  c ′
                 [list "BS"   (λ (c v z s)       s)]    ;  s
                 [list "BNS"  (λ (c v z s) (not  s))]   ;  s ′
                 [list "BV"   (λ (c v z s)       v)]    ;  v
                 [list "BNV"  (λ (c v z s) (not  v))]   ;  v ′
                 ))]
         [hera-op%-branch-to-string
          (λ (pattern instr op)
            (let* ([name (string-append (first (vector-ref hera-op%-branch-table (get-n2 instr))) "R")]
                   [oooooooo (bitwise-and instr #xff)])
              (if (> (bitwise-and oooooooo #x80) 0)
                  (format "~a(-~a) \t" name (- #xff oooooooo))
                  (format "~a(+~a) \t" name         oooooooo ))))]
         [hera-op%-branch-action
          (λ (pattern instr)
            (when debug-HERA-hw
              (printf "Considering branch ~a, flags are ~a\n"
                      (first (vector-ref hera-op%-branch-table (get-n2 instr)))
                      (flags->string)))
            (if (apply (second (vector-ref hera-op%-branch-table (get-n2 instr)))
                       (reverse (vector->list (vector-take flags 4)))) ; want s last, like printout
                (let* ([o_8bit      (get-b0 instr)]
                       [o_extended  (if (> o_8bit #x007f) (bitwise-ior o_8bit #xff00) o_8bit)]
                       [new_PC      (modulo (+ PC o_extended) memsize)])  ; note we assume a PC++ after the BRR
                  (set! PC new_PC)
                  (when debug-HERA-hw
                    (printf "   ... took the branch\n")))
                (set! PC (modulo (+ PC 1) memsize))))])
    
    (new hera-op% [pattern "0000 cccc oooooooo"] [name "B*R"]
         [action    hera-op%-branch-action]
         [to-string hera-op%-branch-to-string])
    )
  ;;; 0x[01]*** ends here

  (void)  ;; otherwise the result of the _parameterize_ will be printed with the outside-of-parameterize settings 
)

(define (step!)
  (let ([instr (vector-ref memory-code PC)])
    (when HERA-hw-step-trace
      (printf "~a\t~a\t" (hex-str PC) (hera-op%-str instr)))
    (hera-op%-dispatch instr)
    (when HERA-hw-step-trace
      (newline))   ))


;
;  File I?O
;

(define/contract (load-data!    [filename ""])  ; -->
  (->*                       () (string?)     void?)
  (if (string=? filename "")
      (let ()
        (vector-set! memory-code 0 #xE111)
        (vector-set! memory-data 0 #xdead)
        (vector-set! memory-data 1 5)
        (vector-set! memory-data 2 7)
        (vector-set! memory-data 3 11)
        (vector-set! memory-data 4 13)
        (vector-set! memory-data 5 17)
        (vector-set! memory-data 6 19)
        (vector-set! memory-data 7 23)
        (vector-set! memory-data 8 27))
      (error "Sorry, load-data! does not yet load from files, please omit name or use \"\" for sample test program"))
  )
      ;(set! memory-data 
      ;(list->vector (random-sample (list 0 1 2 3 4 7 12 17 65535 65534 (random -4 48) (random -4 48) (random -4 48) (random -4 48))
      ;                            memsize))))

(define/contract (load-code!    [filename ""])  ; -->
  (->*                       () (string?)     void?)
  (if (string=? filename "")
      (let ()
        (vector-set! memory-code 0 #xE111)
        (vector-set! memory-code 1 #xE219)
        (vector-set! memory-code 2 #xA312)
        (vector-set! memory-code 3 #xA111)
        (vector-set! memory-code 4 #xB412)
        (vector-set! memory-code 5 #xB521)
        (vector-set! memory-code 6 #xB114)
        (vector-set! memory-code 7 #x0302)  ; BGER, if result in R1 is already >=0, skip +=42 step
        (vector-set! memory-code 8 #xA113)  ; initially we skip this, then, later not
        (vector-set! memory-code 9 #x3780)
        (vector-set! memory-code 10 #x4807)
        (vector-set! memory-code 11 #x00FB)  ; branch back to  6
        )
      (error "Sorry, load-code! does not yet load from files, please omit name or use \"\" for sample test program"))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    TEST SUITE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(load-code! "")
(load-data! "")
(check-equal (vector-ref memory-code 0) #xE111)
(check-equal PC 0)

(check-equal (hera-op%-str (vector-ref memory-code 0))  "SETLO(R1, 17)")
(check-equal (hera-op%-str (vector-ref memory-code 2))  "ADD(R3, R1,R2)")
(check-equal (hera-op%-str (vector-ref memory-code 10)) "LOAD(R8, 0,R7)")

; (set-step-trace! #t)  ; shows stuff getting printed ... set it back again, or maybe make "trace" a parameter

(step!)  ; execute SETLO 1 0x11
(check-equal registers '#(0 17 00  0 0 0 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z s")
(check-equal PC 1)
(step!)  ; execute SETLO 2 0x19
(check-equal registers '#(0 17 25  0 0 0 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z s")
(check-equal PC 2)
(step!)  ; execute ADD R3 R1 R2
(check-equal registers '#(0 17 25 42 0 0 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z s")
(check-equal PC 3)
(step!)  ; execute ADD R1 R1 R1
(check-equal registers '#(0 34 25 42 0 0 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z s")
(check-equal PC 4)
(step!)  ; execute SUB R4 R1 R2, with no carry, that means borrow-in, so 34-25-1 --> 8
(check-equal registers '#(0  34 25 42 08  0 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  C v z s")  ; NOT-BORROW --> CARRY(F4) IS ON
(check-equal PC 5)
(step!)  ; execute SUB R5 R2 R1, with carry set from before, so no borrow-in, 25-34 --> -9 with a Borrow
(check-equal registers '#(0 34 25 42 08 65527 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z S")  ; BORROWED (so nn C), but got a NEGATIVE number (so S is on)
(check-equal PC 6)
(step!)                 ; SUB R1 R1 R3 (with borrow-in; no borrow-out = C out)
(check-equal registers '#(0 25 25 42 08 65527 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  C v z s")
(check-equal PC 7)
(step!)                 ; instr. 7, BRR +2
(check-equal registers '#(0 25 25 42 08 65527 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  C v z s")
(check-equal PC 9)      ; INC R7
(step!)
(check-equal registers '#(0 25 25 42 08 65527 0 1 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z s")
(check-equal PC 10)     ; LOAD R8 <-- M[R7]
(step!)
(check-equal registers '#(0 25 25 42 08 65527 0 1 5 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z s")
(check-equal PC 11)     ; BRR -5
(step!)                 ; instr. 9, BRR -3
(check-equal registers '#(0 25 25 42 08 65527 0 1 5 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z s")
(check-equal PC 6)
(step!)                 ; SUB R1 R1 R4 (25-8, no borrow-in, no borrow-out)
(check-equal registers '#(0 16 25 42 08 65527 0 1 5 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  C v z s")
(check-equal PC 7)      ; BRR +2
(step!)
(check-equal registers '#(0 16 25 42 08 65527 0 1 5 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  C v z s")
(check-equal PC 9)      ; INC R7
(step!)
(check-equal registers '#(0 16 25 42 08 65527 0 2 5 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z s")
(check-equal PC 10)     ; LOAD R8 <-- M[R7]
(step!)
(check-equal registers '#(0 16 25 42 08 65527 0 2 7 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z s")
(check-equal PC 11)     ; BRR -5
(step!)
(check-equal registers '#(0 16 25 42 08 65527 0 2 7 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z s")
(check-equal PC 6)
(step!)                 ; SUB R1 R1 R4 (17 - 8 with borrow)
(check-equal registers '#(0 07 25 42 08 65527 0 2 7 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  C v z s")
(check-equal PC 7)      ; BRR +2
(step!)
(check-equal registers '#(0 07 25 42 08 65527 0 2 7 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  C v z s")
(check-equal PC 9)      ; INC R7
(step!)
(check-equal registers '#(0 07 25 42 08 65527 0 3 7 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z s")
(check-equal PC 10)     ; LOAD R8 <-- M[R7]
(step!)
(check-equal registers '#(0 07 25 42 08 65527 0 3 11 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z s")
(check-equal PC 11)     ; BRR -5
(step!)
(check-equal registers '#(0 07 25 42 08 65527 0 3 11 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z s")
(check-equal PC 6)
(step!)                 ; SUB R1 R1 R3 (07 - 8, WITH borrow, and BORROW-OUT)
(check-equal registers '#(0 65534 25 42 08 65527 0 3 11 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z S")
(check-equal PC 7)      ; BRR +2
(step!)
(check-equal registers '#(0 65534 25 42 08 65527 0 3 11 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z S")
(check-equal PC 9)      ; INC R7
(step!)
(check-equal registers '#(0 65534 25 42 08 65527 0 4 11 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z s")
(check-equal PC 10)     ; LOAD R8 <-- M[R7]
(step!)
(check-equal registers '#(0 65534 25 42 08 65527 0 4 13 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z s")
(check-equal PC 11)     ; BRR -5
(step!)
(check-equal registers '#(0 65534 25 42 08 65527 0 4 13 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z s")
(check-equal PC 6)
(step!)                 ; SUB R1 R1 R3
(check-equal registers '#(0 65525 25 42 08 65527 0 4 13 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  C v z S")
(check-equal PC 7)      ; BRR +2
(step!)
(check-equal registers '#(0 65525 25 42 08 65527 0 4 13 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  C v z S")
(check-equal PC 9)      ; INC R7
(step!)
(check-equal registers '#(0 65525 25 42 08 65527 0 5 13 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z s")
(check-equal PC 10)     ; LOAD R8 <-- M[R7]
(step!)
(check-equal registers '#(0 65525 25 42 08 65527 0 5 17 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z s")
(check-equal PC 11)     ; BRR -5
(step!)
(check-equal registers '#(0 65525 25 42 08 65527 0 5 17 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z s")
(check-equal PC 6)
(step!)                 ; SUB R1 R1 R3
(check-equal registers '#(0 65516 25 42 08 65527 0 5 17 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  C v z S")
(check-equal PC 7)      ; BRR +2
(step!) 
(check-equal registers '#(0 65516 25 42 08 65527 0 5 17 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  C v z S")
(check-equal PC 9)      ; INC R7
(step!)
(check-equal registers '#(0 65516 25 42 08 65527 0 6 17 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z s")
(check-equal PC 10)     ; LOAD R8 <-- M[R7]
(step!)
(check-equal registers '#(0 65516 25 42 08 65527 0 6 19 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z s")
(check-equal PC 11)     ; BRR -5


; consider printing this:
; (vector-take (vector-map hera-op%-str memory-code) 16)
; or running after (set-step-trace! #t)

(reset!)
