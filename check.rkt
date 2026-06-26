#lang racket
;
; this defines a special form "check-equal" to help me check things
; unlike the standard rack-unit, this prints the _expressions_, if there is an error, and has other options
;
; examples:
; (check-equal (cdr (list 1 2 3)) (list 2 3))
; (check-true?  (= 1 1))
; (check-true   (= 1 1))  ; synonym for the above, but it's a predicate, so "?" is preferred, I think... ; IPSB: no, it's not a predicate! predicates return booleans, not <#void>!
; DEPRECATED (check-error? (cdr 5))
; (check-equal (cdr (list)) (list 2 3))  ; this will complain that an error was thrown
;
; set-check-output can be set to the following ... all levels at/above the next multiple of 10 are performed:
;  3: state all tests before they are run
;  2: print answers for incorrect tests  (this is the default)
;  0: don't even execute either parameter ... implies check-result is 0 also
; set-check-result can be set to
;  1: return #t for success, #f for failure
;  0: return (void), so as not to cause printing (this is the default)
;
; at then end, call (check-whinge) to produce a string that's empty (if no errors) or has the form "check module found n errors"


; (require (for-syntax racket/base))  ; tried this for "line" thingy below, no dice so far...
; (require syntax/srcloc) ; To-Do: Update to Racket 8.15, then try this!

(provide check-equal error? ; check-error?
         check-true?  check-false?  check-true  check-false
         set-check-output! set-check-result! check-whinge
         ;implies  ; not sure if "implies" is somewhere in standard racket or not, but I might write one here if not...
         )

(define check-output 2)
(define check-result 0)
(define check-err-count 0)

(define (set-check-output! level)
  (set! check-output level))
(define (set-check-result! level)
  (set! check-result level))
(define (check-record-error!)  ; private
  (set! check-err-count (+ 1 check-err-count)))

(define check-equal-examples #<<end-check-equal-examples
> (set-check-result! 0)
> (check-equal (+ 1 1) (* 1 1))
'()
> (set-check-result! 1)
> (check-equal (+ 1 1) (* 1 1))
#f
> (set-check-level! 2)
> (check-equal (+ 1 1) (* 1 1))
     UH-OH! got result   2
            rather than  (* 1 1)
            for          (+ 1 1)
#f
> (set-check-level! 3)
> (check-equal (+ 1 1) (* 1 1))
Trying (+ 1 1) and hoping for (* 1 1).
     UH-OH! got result   2
            rather than (* 1 1)
            for         (+ 1 1)
#f
end-check-equal-examples
)

(define-syntax check-equal ; (check-equal stx)  ; stx as in https://www.greghendershott.com/2014/06/file-and-line-in-racket.html  ... but, no luck, so far
  ; @ToDo: some day, maybe? Dave figures out how to take stuff like __LINE__ and __FILE__ and integrates it here
;  (let ([da-line (with-syntax ([line (syntax-line stx)])
;                   (syntax-case stx ()
;                     [_ #'line]))]) ; https://www.greghendershott.com/2014/06/file-and-line-in-racket.html
;    (display da-line)
;    da-line
;  ))

  (syntax-rules ()
    ((_ ques ans)
     (if (>= check-output 1)
         (begin
           (when (>= check-output 3)
             (printf "Trying ~s and hoping for ~s.\n" 'ques ans))
           (with-handlers ([exn:fail?                              ;; Sort of like Java/Python "try", but the "catch" comes before the thing to try
                           (λ (e) (begin                           ;; This is like a "catch" or "except" part, with e being the exception
                                    (check-record-error!)
                                    (when (>= check-output 2)
                                      (printf "\n  UH-OH!!!!!         threw an exception\n     when hoping for ~s\n         for         ~s\n"
                                              ans 'ques))

                                    ; (printf "\n  UH-OH!!!!!         threw an exception\n     when hoping for ~s\n         for         ~s\n" ans 'ques))
                                    ; So sad; Always get _this_ line number in check.rkt when I try this the easy way
                                    ; (printf "\n  ~s:~s UH-OH!!!!!         threw an exception\n     when hoping for ~s\n         for         ~s\n"
                                    ;         (syntax-source #'here) (syntax-line #'here) ; https://www.greghendershott.com/2014/06/file-and-line-in-racket.html
                                    ;         ans 'ques))

                                    (if (>= check-result 1) #f (void))))])
             (let* ([got ques]                                     ;; This is the "try" part: we actually evaluate the question, and report if we got ans
                    [ok  (equal? got ans)])
               (when (not ok)
                 (begin
                   (check-record-error!)
                   (when (>= check-output 2)
                     (printf "\n  UH-OH! got result  ~s\n         rather than ~s\n         for         ~s\n"
                             ; (quote-srcloc-string) ; NEEDS RACKET 8.15 ...  now trying srcloc, from https://www.reddit.com/r/Racket/comments/q6cnym/how_to_retrieve_line_column_of_current_location/
                             got ans
                             'ques))))
               (if (>= check-result 1) ok (void)))))
         (void)))))

(check-equal 1 1)

(define-syntax check-true?
  (syntax-rules ()
    ((_ expr)
     (check-equal (not (not expr)) #t))))  ; not-not lets, e.g., the result of "member" count as true
(define-syntax check-true
  (syntax-rules ()
    ((_ expr)
     (check-equal (not (not expr)) #t))))
(define-syntax check-false?
  (syntax-rules ()
    ((_ expr)
     (check-equal (not (not expr)) #f))))
(define-syntax check-false
  (syntax-rules ()
    ((_ expr)
     (check-equal (not (not expr)) #f))))

;; Examples:
(check-true (> 42 17))
;; Note the following gives a "check" report rather than an exception, because check-true and check-equal are syntax, not functions
;; (check-true (> 1 (/ 5 0)))

; deprecated: may not work now thath check-equal notices errors :-(
;(define-syntax check-error?
;  (syntax-rules ()
;    ((_ expr)
;     (if (with-handlers ; https://docs.racket-lang.org/reference/exns.html
;             ([exn:fail?
;               (λ (x) #t)])
;           (begin
;             expr
;             #f))
;         (void)
;         (check-equal (format " ~a" `expr) "should produce an error" )))))

; we can at least have this syntax, which returns #t if the expression raises an error
; optionally, also accepts a type for the error (or, really, any predicate or function you want)
(define-syntax error?
  (syntax-rules ()
    [(error? type expression) (with-handlers ([exn:fail? type]) (begin expression #f))]
    [(error? expression) (with-handlers ([exn:fail? (const #t)]) (begin expression #f))]))
(check-true (error? (/ 1 0)))
(check-false (error? 5))
(check-true (error? exn:fail:contract:divide-by-zero? (/ 1 0)))
(check-false (error? exn:fail:contract:arity? (/ 1 0)))


(define (check-whinge)
  (if (> check-err-count 0)
      (format "check module found ~a errors")
      ""))


