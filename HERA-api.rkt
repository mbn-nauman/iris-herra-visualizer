#lang racket
(require "check.rkt")
(require "HERA-hardware.rkt")

(provide reset! step! get-PC get-flags get-register get-data get-code get-code-asm)  ; reset! right outta HERA-hardware
(provide load-data! load-code!)

(define verbose #f)


(define/contract (get-PC)  ; return the program counter
  (-> integer?) 
  PC)

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
 


;;
;;  ---- UNIT TEST BELOW ----
;;

(check-equal (get-PC) 0)
(check-equal (get-register 0) 0)
(check-equal (get-data 0) 0)
(check-equal (get-code 0) 0)


