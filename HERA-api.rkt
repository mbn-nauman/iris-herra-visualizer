#lang racket
(require racket/random)
(provide get-PC step! get-flags get-registers get-data get-code)


; Just mockup for now ... all provided functions can be called, but
;   for now, step! only simulates the exact instruction ADD(r1, r2,r3)

(define memsize 65536)  ; sometimes we set this to 64 to make testing less annoying
(define wordlim 65536)  ; registers and ram hold values modulo wordlim
(define verbose #f)
(define PC 0)
(define flags       (make-vector 5       #f))
(define registers   (list->vector (random-sample (list 0 1 2 7 12 65535) 16)))
                    (vector-set! registers 0 0)
(define memory-data (list->vector (random-sample (list 0 1 2 3 4 7 12 17 65535 65534 (random -4 48) (random -4 48) (random -4 48) (random -4 48))
                                                 memsize)))
(define memory-code (list->vector (random-sample (list #xA123 #xA121 #xA321 #xA221)
                                                 memsize)))



(define/contract (get-PC)  ; return the program counter
  (-> integer?) 
  PC)
(define          (step!)   ; this one will end up getting a lot more interesting
  (when verbose (printf "At ~a instr ~a\n" PC (vector-ref memory-code PC)))
  (when (= (vector-ref memory-code PC) #xA123)
    (vector-set! registers 1 (modulo (+ (vector-ref registers 2) (vector-ref registers 3)) wordlim)))
  (set! PC (modulo (+ PC 1) memsize)))

(define/contract (get-flags)     (-> vector?) flags)
(define/contract (get-registers) (-> vector?) registers)
(define/contract (get-data)      (-> vector?) memory-data)
(define/contract (get-code)      (-> vector?) memory-code)
  
