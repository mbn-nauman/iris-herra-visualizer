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

(define (hera-val? v)
  (and (integer? v) (<= 0 v) (< v wordlim)))
(define (hera-addr? a)
  (and (integer? a) (<= 0 a) (< a memsize)))
(define (hera-reg-num? r)
  (and (integer? r) (<= 0 r) (< r 16)))

;;
;; some bit operations making use of the string-input system that allows 0, 1, _, and letters to define ops
;;

; cut spaces, change non-"care-about-chars" into "miss", then care-about-chars into hit-is-or-nullstr (if != ""), in pattern
(define/contract (maskfor care-about-chars miss-is hit-is-or-nullstr pattern)
  (->                     string?          string? string?           string?   hera-val?)
  (let* ([misses (list (regexp (string-append "[^" care-about-chars "]")) miss-is)]
         [spaces '[#rx" *" ""]]
         [xforms (if (string=? hit-is-or-nullstr "")
                    (list spaces misses)
                    (list spaces misses
                          (list (regexp (string-append "[" care-about-chars "]")) hit-is-or-nullstr)))])
    (string->number
     (string-append "#b" (regexp-replaces pattern xforms)))))

(define (get-n3 i)    (/ (bitwise-and #xF000 i) #x1000))  ; nybble 3, i.e., usually the op-code, except for shifts, etc.
(define (get-n2 i)    (/ (bitwise-and #x0F00 i) #x0100))
(define (get-n1 i)    (/ (bitwise-and #x00F0 i) #x0010))
(define (get-n0 i)    (/ (bitwise-and #x000F i) #x0001))

(define (get-b0 i)       (bitwise-and #x00FF i))

(let ([example-mult "1100 Dddd Aaaa Bbbb"] [example-num #xbcde])
  (check-equal (maskfor  "1"  "0" ""  example-mult) #xC000) ; make all non-1's into 0's, so we can and with this
  (check-equal (maskfor  "0"  "1" ""  example-mult) #xCFFF) ;    all non-0's become 1's, so we can  or with this
  (check-equal (maskfor  "aA" "0" "1" example-mult) #x00F0) ; mask for parameter A
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
    (define and-mask  (maskfor "1"  "0"  "" pattern)) ; see examples in tests above
    (define  or-mask  (maskfor "0"  "1"  "" pattern))

    (define op-a-mask (maskfor "aA" "0" "1" pattern))
    (define op-b-mask (maskfor "bB" "0" "1" pattern))
    (define op-d-mask (maskfor "Dd" "0" "1" pattern))
    (define op-v-mask (maskfor "Vv" "0" "1" pattern))
    
    (define/public (match? me)   (or (= (bitwise-and me and-mask) and-mask)
                                     (= (bitwise-ior me  or-mask)  or-mask)))
    (define/public (doit!  instr) (if (match? instr) (_a instr) (error (format "Instr #x~x doesn't match op ~s" instr _p))))
    
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
  (->                      hera-reg-num? hera-val? void)
  (when (> r 0)
    (vector-set! registers r v)))


(define SETLO
  (new hera-op% [pattern "1110 cccc vvvvvvvv"]
       [action (λ (instr)
                 (set-reg! (get-n2 instr) (get-b0 instr)))]))
(define ADD
  (new hera-op% [pattern "1010 cccc vvvvvvvv"]
       [action (λ (instr)
                 (set-reg! (get-n2 instr) (get-b0 instr)))]))

(define (step!)
  (let ([op (vector-ref memory-code PC)])
    (when (send SETLO match? op)
      (send SETLO doit! op))
    ))

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

(vector-set! memory-data 0 #xE12A)
(check-true  (send SETLO match? #xE12A))
(send SETLO doit! #xE12A)
(check-equal registers '#(0 42  0 0 0 0 0 0 0 0 0 0 0 0 0 0))
(vector-set! memory-code 0 #xE211)
(step!)  ; execute SETLO 2 0x11
(check-equal registers '#(0 42 17 0 0 0 0 0 0 0 0 0 0 0 0 0))
(reset!)
