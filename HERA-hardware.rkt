#lang racket
(provide memsize wordlim PC flags registers memory-data memory-code reset! load-data! load-code!)

(require "check.rkt")
(require racket/math) ;; for bitwise-and



(require racket/random) ; temporary, while testing

(define memsize 65536)  ; sometimes we set this to 64 to make testing less annoying
(define wordlim 65536)  ; registers and ram hold values modulo wordlim
(define PC 0)
(define flags       (make-vector 5       #f))
(define registers   (make-vector 16       0))
(define memory-data (make-vector memsize  0))
(define memory-code (make-vector memsize  0))

(define debug-HERA-hw #t)

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


; I've not done much with Racket classes; relying on https://docs.racket-lang.org/guide/classes.html
;    -Dave W
(define hera-op%
  (class object%
    (init pattern action)
    (define _p pattern)
    (define _a action)
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
    (super-new)
    ;(field (andmask #x0000))    ; integer? ... 1's here mean 1's are needed in the op)
    ;(field ( ormask #xffff))    ; integer? ... 1's here mean 1's are needed in the op)
    ;(field (action! '()))       ; func to update state vars
    ; (field (op->string (λ () "Illegal instruction"))) ; func for output
    ))



(define (reset!)
  (set! PC 0)
  (set! flags       (make-vector 5       #f))
  (set! registers   (make-vector 16       0))
  (set! memory-data (make-vector memsize  0))
  (set! memory-code (make-vector memsize  0)))


(define/contract (set-reg! r             v)
  (->                      hera-reg-num? hera-val? void?)
  (when (> r 0)
    (vector-set! registers r v)))
(define/contract (get-reg  r)
  (->                      hera-reg-num? hera-val?)
  (vector-ref registers r))

(define (inc-PC!)
  (set! PC (modulo (+ PC 1) wordlim)))

;
; for now, also-set-flags is 0 for "no flags", #xff for all flags;
;          could extend to allow other bits patterns for which flags to set
; Note that value may be outside of HERA's signed or unsigned range, e.g., 17-25 can be 8, not #xFFF8
(define/contract (set-reg-inc-PC! reg             value       [also-set-flags #xff])
  (->*                           (hera-reg-num?   integer?)   (integer?)            void?)
  (let ([hera-val (modulo value wordlim)])
    (set-reg! reg hera-val)
    (when (> 0 also-set-flags)
      (eprintf "ToDo: set flags"))  ; ToDo
    (inc-PC!)
  ))

(define SETLO
  (new hera-op% [pattern "1110 dddd vvvvvvvv"]
       [action (λ (pattern instr)
                 (let* ([v_8bit      (get-b0 instr)]
                        [v_extended  (if (> v_8bit #x007f) (bitwise-ior v_8bit #xff00) v_8bit)])
                   (when debug-HERA-hw
                     (printf "SETLO #x~x setting R~a to #x~x\n" instr (get-n2 instr) v_extended))
                   (set-reg-inc-PC! (get-n2 instr) v_extended #x00)))]))
(define ADD
  (new hera-op% [pattern "1010 dddd aaaa bbbb"]
       [action (λ (pattern instr)
                 (when debug-HERA-hw
                       (printf "ADD   #x~x R~a = ~a + ~a\n" instr (get-n2 instr) (get-reg (get-n1 instr)) (get-reg (get-n0 instr))))
                 (set-reg-inc-PC! (get-n2 instr) (+ (get-reg (get-n1 instr)) (get-reg (get-n0 instr)))))]))
(define SUB
  (new hera-op% [pattern "1011 dddd aaaa bbbb"]
       [action (λ (pattern instr)
                 (when debug-HERA-hw
                       (printf "SUB   #x~x R~a = ~a - ~a\n" instr (get-n2 instr) (get-reg (get-n1 instr)) (get-reg (get-n0 instr))))
                 (set-reg-inc-PC! (get-n2 instr) (- (get-reg (get-n1 instr)) (get-reg (get-n0 instr)))))]))

(define (step!)
  (let ([op (vector-ref memory-code PC)])
    (when debug-HERA-hw
      (printf "Executing instruction #x~x at location #x~x\n" op PC))
    (cond  ; ToDo: replace with vector for get-n3, sub-hash/vector for cases 2 and 3
      [(send SETLO match? op)   (send SETLO doit! op)]
      [(send   ADD match? op)   (send   ADD doit! op)]
      [(send   SUB match? op)   (send   SUB doit! op)]
      [else  ; ToDo: consider throwing an error instead?
       (eprintf ; https://docs.racket-lang.org/reference/Writing.html#%28def._%28%28quote._~23~25kernel%29._eprintf%29%29
        "Illegal instruction: #x~x"  op)
       (inc-PC!)])))


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
  (set! memory-code
        (list->vector (random-sample (list #xA123 #xA121 #xA321 #xA221)
                                     memsize))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    TEST SUITE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(check-true  (send SETLO match? #xE12A))
(check-false (send SETLO match? #x312A))
(send SETLO doit! #xE12A)
(check-equal registers '#(0 42  0  0 0 0 0 0 0 0 0 0 0 0 0 0))
(check-equal flags     '#(#f #f #f #f #f))

(check-true  (send ADD   match? #xA312))
(check-false (send ADD   match? #xE312))
(send ADD   doit! #xA111)
(check-equal registers '#(0 84  0  0 0 0 0 0 0 0 0 0 0 0 0 0))
(check-equal flags     '#(#f #f #f #f #f))
(reset!)

(vector-set! memory-code 0 #xE111)
(vector-set! memory-code 1 #xE219)
(vector-set! memory-code 2 #xA312)
(vector-set! memory-code 3 #xA111)
(vector-set! memory-code 4 #xB412)
(vector-set! memory-code 5 #xB521)

(step!)  ; execute SETLO 1 0x11
(check-equal registers '#(0 17 00  0 0 0 0 0 0 0 0 0 0 0 0 0))
(check-equal flags     '#(#f #f #f #f #f))
(step!)  ; execute SETLO 2 0x19
(check-equal registers '#(0 17 25  0 0 0 0 0 0 0 0 0 0 0 0 0))
(check-equal flags     '#(#f #f #f #f #f))
(step!)  ; execute ADD R3 R1 R2
(check-equal registers '#(0 17 25 42 0 0 0 0 0 0 0 0 0 0 0 0))
(check-equal flags     '#(#f #f #f #f #f))
(step!)  ; execute ADD R1 R1 R1
(check-equal registers '#(0 34 25 42 0 0 0 0 0 0 0 0 0 0 0 0))
(check-equal flags     '#(#f #f #f #f #f))
(step!)  ; execute SUB R4 R1 R2
(check-equal registers '#(0 34 25 42 09  0 0 0 0 0 0 0 0 0 0 0))
(check-equal flags     '#(#f #f #f #f #f))
(step!)  ; execute SUB R5 R2 R1
(check-equal registers '#(0 34 25 42 09 65527 0 0 0 0 0 0 0 0 0 0))
(check-equal flags     '#(#f #f #f #f #f))
(reset!)
