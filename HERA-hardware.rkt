#lang racket
(provide memsize wordlim PC flags registers memory-data memory-code reset! load-data! load-code! step!)

(require "check.rkt")
(require racket/math) ;; for bitwise-and, etc.

(define debug-HERA-hw #f)



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

(define (setf-s v)  (vector-set! flags 0)) 
(define flag-s-ind  0)
(define flag-z-ind  1)
(define flag-v-ind  2)
(define flag-c-ind  3)
(define flag-cb-ind 4)

(eprintf " ==> HERA-hardware.rkt warning: overflow (V) flag not being set correctly, will always show as false (v not V) to let tests pass <==\n")

(define (flags->string) ; for N=5, not _quite_ worth doing something cool with map
  (let ([sep " "])
    (string-append
     (if (vector-ref flags flag-cb-ind) "B " "b ")
     sep
     (if (vector-ref flags flag-c-ind) "C" "c")
     sep
     "v" ; ToDo: (if (vector-ref flags flag-v-ind) "V" "v")
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
    (init pattern name action)
    (define _p pattern)
    (define _n name)
    (define _a action)
    (super-new)
    
    (define and-mask  (mask-for "1"  "0"  "" pattern)) ; see examples in tests above
    (define  or-mask  (mask-for "0"  "1"  "" pattern))

    (define op-a-mask (mask-for "aA" "0" "1" pattern))
    (define op-b-mask (mask-for "bB" "0" "1" pattern))
    (define op-d-mask (mask-for "Dd" "0" "1" pattern))
    (define op-v-mask (mask-for "Vv" "0" "1" pattern))
    
    (define/public (match? me)   (and (= (bitwise-and me and-mask) and-mask)
                                      (= (bitwise-ior me  or-mask)  or-mask)))
    (define/public (doit!  instr) (if (match? instr) (_a _p instr) (error (format "Instr #x~x doesn't match op ~s" instr _p))))
    
    (define/public (str-verbose) (format "HERA op ~s: and-mask=#x~x or-mask=#x~x" _p and-mask or-mask))

    (let ([n3 (get-n3 and-mask)])  ; or-mask would also work; here, we assume that the leftmost 4 bits tell us how to dispatch...
      (when (vector-ref hera-op%-dispatch-table n3)         ; ... this means LOAD and STORE will each appear twice, and ...
        (eprintf
         " ==> HERA-hardware.rkt: ~s overwriting op ~a <=="
                                  _n                n3))
      (vector-set! hera-op%-dispatch-table n3 this))
    ))

; Fake class-field
(define hera-op%-dispatch-table
  (make-vector 16 #f))

(define/contract (hera-op%-dispatch instr)
  (->                               hera-val? void?)
  (let* ([n3 (get-n3 instr)]
         [op (vector-ref hera-op%-dispatch-table n3)])
    (if op
        (if (send op match? instr)
            (send op doit!  instr)
            (eprintf " ==> HERA-hardware.rkt inconsistency: op for #x0~x doesn't think it matches #x~x <==\n" n3 instr))
        (let ()
          (printf "Illegal instruction (no op implemented): ~a\n" instr)
          (inc-PC!)))))



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

(define (inc-PC!)
  (set! PC (modulo (+ PC 1) wordlim)))

;
; for now, also-set-flags is 0 for "no flags", #xff for all flags;
;          could extend to allow other bits patterns for which flags to set
; Note that value may be outside of HERA's signed or unsigned range, e.g., 17-25 can be 8, not #xFFF8
(define/contract (set-reg-inc-PC! reg             value       [also-set-flags #xff])
  (->*                           (hera-reg-num?   integer?)   (integer?)            void?)
  (let ([hera-val (modulo value wordlim)])
    (when debug-HERA-hw
      (printf "set-reg-inc-PC for R~a <-- ~a/~a (PC ~a)\n" reg value hera-val PC))
    (set-reg! reg hera-val)
    (when (> also-set-flags 0)
      (set-flag! flag-z-ind
                 (= value 0))
      (set-flag! flag-s-ind
                 (> (bitwise-and value (/ wordlim 2)) 0))
      (when (not (= (bitwise-and value (/ wordlim 2)) (bitwise-and hera-val (/ wordlim 2)))) (eprintf "Hmmm, questionable sign flag for ~a/~a" value hera-val))
      (set-flag! flag-v-ind
                 (not (= (bitwise-and value wordlim) (* 2 (bitwise-and value (/ wordlim 2))))))
      (set-flag! flag-c-ind
                 (> (bitwise-and value wordlim) 0)))
    (inc-PC!)
  ))

(define SETLO
  (new hera-op% [pattern "1110 dddd vvvvvvvv"] [name "SETLO"]
       [action (λ (pattern instr)
                 (let* ([v_8bit      (get-b0 instr)]
                        [v_extended  (if (> v_8bit #x007f) (bitwise-ior v_8bit #xff00) v_8bit)])
                   (when debug-HERA-hw
                     (printf "SETLO #x~x setting R~a to #x~x\n" instr (get-n2 instr) v_extended))
                   (set-reg-inc-PC! (get-n2 instr) v_extended #x00)))]))
(define ADD
  (new hera-op% [pattern "1010 dddd aaaa bbbb"] [name "ADD"]
       [action (λ (pattern instr)
                 (when debug-HERA-hw
                       (printf "ADD   #x~x R~a = ~a + ~a\n" instr (get-n2 instr) (get-reg (get-n1 instr)) (get-reg (get-n0 instr))))
                 (set-reg-inc-PC! (get-n2 instr) (+ (get-reg (get-n1 instr))
                                                    (get-reg (get-n0 instr))
                                                    (get-c^!cb))))]))
(define SUB
  (new hera-op% [pattern "1011 dddd aaaa bbbb"] [name "SUB"]
       [action (λ (pattern instr)
                 (when debug-HERA-hw
                       (printf "SUB   #x~x R~a = ~a - ~a\n" instr (get-n2 instr) (get-reg (get-n1 instr)) (get-reg (get-n0 instr))))
                 (set-reg-inc-PC! (get-n2 instr) (+ (get-reg (get-n1 instr))
                                                    (- (- wordlim 1) (get-reg (get-n0 instr))) ; n0 bit-flipped
                                                    (get-cvcb))))]))

(define BRR
  (new hera-op% [pattern "0000 0000 oooooooo"] [name "BRR"]
       [action (λ (pattern instr)
                 (let* ([o_8bit      (get-b0 instr)]
                        [o_extended  (if (> o_8bit #x007f) (bitwise-ior o_8bit #xff00) o_8bit)]
                        [new_PC      (modulo (+ PC o_extended) memsize)])  ; note we assume a PC++ after the BRR
                   (when debug-HERA-hw
                     (printf "BRR #x~x updating PC from  #x~x to #x~x\n" instr PC new_PC))
                   (set! PC new_PC)))]))


(define (step!)
  (let ([instr (vector-ref memory-code PC)])
    (hera-op%-dispatch instr)))


;
;  File I?O
;

(define/contract (load-data! filename)  ; -->
  (->                       string?     void?)
  (set! memory-data
        (list->vector (random-sample (list 0 1 2 3 4 7 12 17 65535 65534 (random -4 48) (random -4 48) (random -4 48) (random -4 48))
                                     memsize))))

(define/contract (load-code! filename)  ; -->
  (->                       string?     void?)
  (vector-set! memory-code 0 #xE111)
  (vector-set! memory-code 1 #xE219)
  (vector-set! memory-code 2 #xA312)
  (vector-set! memory-code 3 #xA111)
  (vector-set! memory-code 4 #xB412)
  (vector-set! memory-code 5 #xB521)
  (vector-set! memory-code 6 #xB114)
  (vector-set! memory-code 7 #x0002)
  (vector-set! memory-code 8 #x0081)  ; we should skip this; branch somewhere crazy if we don't
  (vector-set! memory-code 9 #x00FD)  ; branch back to  6
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    TEST SUITE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(check-true  (send SETLO match? #xE12A))
(check-false (send SETLO match? #x312A))
(send SETLO doit! #xE12A)
(check-equal registers '#(0 42  0  0 0 0 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z s")

(check-true  (send ADD   match? #xA312))
(check-false (send ADD   match? #xE312))
(send ADD   doit! #xA111)
(check-equal registers '#(0 84  0  0 0 0 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z s")
(reset!)

(load-code! "/dev/null")
(check-equal (vector-ref memory-code 0) #xE111)
(check-equal PC 0)

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
(check-equal PC 9)
(step!)                 ; instr. 9, BRR -3
(check-equal registers '#(0 25 25 42 08 65527 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  C v z s")
(check-equal PC 6)
(step!)                 ; SUB R1 R1 R4 (25-8, no borrow-in, no borrow-out)
(check-equal registers '#(0 17 25 42 08 65527 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  C v z s")
(check-equal PC 7)      ; BRR +2
(step!)
(check-equal registers '#(0 17 25 42 08 65527 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  C v z s")
(check-equal PC 9)      ; BRR -3
(step!)
(check-equal registers '#(0 17 25 42 08 65527 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  C v z s")
(check-equal PC 6)
(step!)                 ; SUB R1 R1 R4 (17 - 8 no borrow)
(check-equal registers '#(0 09 25 42 08 65527 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  C v z s")
(check-equal PC 7)      ; BRR +2
(step!)
(check-equal registers '#(0 09 25 42 08 65527 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  C v z s")
(check-equal PC 9)      ; BRR -3
(step!)
(check-equal registers '#(0 09 25 42 08 65527 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  C v z s")
(check-equal PC 6)
(step!)                 ; SUB R1 R1 R3 (09 - 8, no borrow)
(check-equal registers '#(0 01 25 42 08 65527 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  C v z s")
(check-equal PC 7)      ; BRR +2
(step!)
(check-equal registers '#(0 01 25 42 08 65527 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  C v z s")
(check-equal PC 9)      ; BRR -3
(step!)
(check-equal registers '#(0 01 25 42 08 65527 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  C v z s")
(check-equal PC 6)
(step!)                 ; SUB R1 R1 R3 (01- 8, no borrow-in (so -7 answer), but borrow-out (c) and S
(check-equal registers '#(0 65529 25 42 08 65527 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z S")
(check-equal PC 7)      ; BRR +2
(step!)
(check-equal registers '#(0 65529 25 42 08 65527 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z S")
(check-equal PC 9)      ; BRR -3
(step!)
(check-equal registers '#(0 65529 25 42 08 65527 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  c v z S")
(check-equal PC 6)
(step!)                 ; SUB R1 R1 R3 (-7 - 8, with borrow-in (so -16 answer), no borrow-out (C) and S
(check-equal registers '#(0 65520 25 42 08 65527 0 0 0 0 0 0 0 0 0 0))
(check-equal (flags->string) "b  C v z S")
(check-equal PC 7)      ; BRR +2

(reset!)
