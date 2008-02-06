;; Error handling

#fload "sfc.sf"
#fload "print.ss"

;; Reader errors
(define (r-error* msg arg*)
  (newline)
  (q-display "QA0: Reader error: ")
  (q-display msg)
  (for-each (lambda (arg) 
	      (write-char #\space)
	      (if (string? arg) (q-display arg) (q-write arg))) 
	    arg*)
  (newline)
  (reset))

(define-syntax r-error
  (syntax-rules ()
    [(_ msg arg ...) (r-error* msg (list arg ...))]))

;; Syntax errors
(define (s-error* msg arg*)
  (newline)
  (q-display "Syntax error: ")
  (q-fprint* (current-output-port) msg arg*)
  (newline)
  (reset))

(define-syntax s-error
  (syntax-rules ()
    [(_ msg arg ...) (s-error* msg (list arg ...))]))

;; Internal compiler errors, aka ICE
(define (ic-error* loc msg arg*)
  (newline)
  (q-display "QA0 ICE: ")
  (q-display loc)
  (q-display " ")
  (q-fprint* (current-output-port) msg arg*)
  (newline)
  (reset))

(define-syntax ic-error
  (syntax-rules ()
    [(_ loc msg arg ...) (ic-error* loc msg (list arg ...))]))

