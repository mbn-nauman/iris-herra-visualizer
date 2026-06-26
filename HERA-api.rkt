#lang racket
(require "check.rkt")
(provide get-PC step! get-flags get-register get-data get-code get-code-asm)
(provide load-data load-code)

(require racket/random) ; temporary, while testing

; Just mockup for now ... all provided functions can be called, but
;   for now, step! only simulates the exact instruction ADD(r1, r2,r3)

(define memsize 65536)  ; sometimes we set this to 64 to make testing less annoying
(define wordlim 65536)  ; registers and ram hold values modulo wordlim
(define verbose #f)
(define PC 0)
(define flags       (make-vector 5       #f))
(define registers   (make-vector 16       0))
(define memory-data (make-vector memsize  0))
(define memory-code (make-vector memsize  0))


(define (hera-val? v)
  (and (integer? v) (<= 0 v) (< v wordlim)))
(define (hera-addr? a)
  (and (integer? a) (<= 0 a) (< a memsize)))
(define (hera-reg-num? r)
  (and (integer? r) (<= 0 r) (< r 16)))


(define/contract (get-PC)  ; return the program counter
  (-> integer?) 
  PC)
(define          (step!)   ; this one will end up getting a lot more interesting
  (when verbose (printf "At ~a instr ~a\n" PC (vector-ref memory-code PC)))
  (when (= (vector-ref memory-code PC) #xA123)
    (vector-set! registers 1 (modulo (+ (vector-ref registers 2) (vector-ref registers 3)) wordlim)))
  (set! PC (modulo (+ PC 1) memsize)))

(define/contract (get-flags)      (-> vector?) flags)
(define/contract (get-register r)            ; -->
  (->                          hera-reg-num? hera-val?)
  (vector-ref registers   r))

(define/contract (get-data dadr)      ; -->
  (->                      hera-addr? hera-val?)
  (vector-ref memory-data dadr))

(define/contract (get-code iadr)      ; -->
  (->                      hera-addr? hera-val?)
  (vector-ref memory-code iadr))
(define/contract (get-code-asm iadr)      ; -->
  (->                          hera-addr? string?)
  "ADD(R3, R5,R2) // e.g.")
  

; (set! registers (list->vector (random-sample (list 0 1 2 7 12 65535) 16)))
;  (vector-set! registers 0 0)

(define/contract (load-data filename)  ; -->
  (->                       string?     void?)
  (set! memory-data
        (list->vector (random-sample (list 0 1 2 3 4 7 12 17 65535 65534 (random -4 48) (random -4 48) (random -4 48) (random -4 48))
                                     memsize))))

(define/contract (load-code filename)  ; -->
  (->                       string?     void?)
  (set! memory-code
        (list->vector (random-sample (list #xA123 #xA121 #xA321 #xA221)
                                     memsize))))


;;
;;  ---- UNIT TEST BELOW ----
;;

(check-equal (get-PC) 0)
(check-equal (get-register 0) 0)
(check-equal (get-data 0) 0)
(check-equal (get-code 0) 0)

(step!)
(check-equal (get-PC) 1)  ; this should actually stay 0 if code[0] is 0, but, that's not implemented yet ... ToDo!

