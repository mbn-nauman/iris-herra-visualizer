#lang racket
(require "check.rkt")
(require "HERA-hardware.rkt")

(provide reset! step! get-PC get-flags get-register get-data get-code get-code-asm)  ; reset! right outta HERA-hardware
(provide load-data! load-code!)

(define verbose #f)
(define show-demo #t)  ; change to #t to do the demo

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
  (hera-op%-str (get-code iadr)))
 


;;
;;  ---- UNIT TEST BELOW ----
;;

(check-equal (get-PC) 0)
(check-equal (get-register 0) 0)
(check-equal (get-data 0) 0)
(check-equal (get-code 0) 0)

; try out this demo, if you like
(when show-demo
  (printf   " ====  Welcome to the HERA-api demo   ====\n\n")
  (load-code!)
  (printf   " ==== Here's our example HERA program ====\n")
  (map (λ (addr) (printf "~a\t~a\n" (hex-str addr) (get-code-asm addr)))
       (range 13))
  (printf "\n ==== Now, lets run some steps ====\n")
  (load-data!)  ;  get the data memory set up so the LOAD's work
  (set-step-trace! #t)
  (map (λ (step) (step!) (printf "   Reg:\t~a\n" (vector-take registers 11)))
       (range 42))
  (set-step-trace! #f)
  (void)
  )

