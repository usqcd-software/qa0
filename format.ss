;; Limited formatter
#fload "sfc.sf"
#fload "error.ss"
;;
;; (provide q-fmt*)
;; (provide q-fmt)
;;

(define (q-fmt* fmt arg*)
  (define (fmt/1 obj r*)
    (cond
     [(boolean? obj) (append (list (if obj #\t #\f) #\#) r*)]
     [(char? obj) (cons obj r*)]
     [(number? obj) (append (reverse (string->list (number->string obj))) r*)]
     [(symbol? obj) (append (reverse (string->list (symbol->string obj))) r*)]
     [(string? obj) (append (reverse (string->list obj)) r*)]
     [else (ic-error "q-fmt: unknown object" obj)]))
  (let loop ([flst (string->list fmt)] [arg* arg*] [r* '()])
    (cond
     [(null? flst) (list->string (reverse r*))]
     [(char=? (car flst) #\~)
      (and (pair? (cdr flst))
	   (let ([c (cadr flst)])
	     (cond
	      [(char=? c #\a) (loop (cddr flst) (cdr arg*)
				    (fmt/1 (car arg*) r*))]
	      [(char=? c #\~) (loop (cddr flst) arg* (cons #\~ r*))]
	      [(char=? c #\%) (loop (cddr flst) arg* (cons #\newline r*))]
	      [else (ic-error "q-fmt: unrecognized format" (cadr flst))])))]
     [else (loop (cdr flst) arg* (cons (car flst) r*))])))

(define-syntax q-fmt
  (syntax-rules ()
    [(_ fmt arg ...) (q-fmt* fmt (list arg ...))]))