;; Limited formatter
#fload "sfc.sf"
#fload "error.ss"
;;
;; (provide q-fmt*)
;; (provide q-fmt)
;;

;; a copy of the printer...
(define (q-fmt* fmt arg*)
  (define (out-char c d? r*)
    (cond
     [d? (cons c r*)]
     [(char=? c #\newline) (out-string "#\\newline" #t r*)]
     [(char=? c #\space) (out-string "#\\space" #t r*)]
     [else (cons c (out-string "#\\" #t r*))]))
  (define (out-string s d? r*)
    (let ([l (string-length s)])
      (let loop ([r* (if d? r* (out-char #\" #t r*))] [i 0])
      (cond
       [(= i l) (if d? r* (out-char #\" #t r*))]
       [else (let ([c (string-ref s i)])
	       (cond
		[d? (loop (cons c r*) (+ i 1))]
		[(or (char=? c #\\) (char=? c #\"))
		 (loop (out-char c #t (out-char #\\ #t r*)) (+ i 1))]
		[else (loop (out-char c #t r*) (+ i 1))]))]))))
  (define (out-box x d? r*)
    (fmt/1 (unbox x) d? (out-string "#&" r*)))
  (define (out-vector x d? r*)
    (let ([n (vector-length x)])
      (let loop ([r* (out-string "#(" #t r*)] [i 0])
	(cond
	 [(= i n) (out-char #\) #t r*)]
	 [(zero? i) (loop (fmt/1 (vector-ref x i) d? r*) (+ i 1))]
	 [else (loop (fmt/1 (vector-ref x i) d? (out-string " " #t r*))
		     (+ i 1))]))))
  (define (out-pair x d? r*)
    (let loop ([r* (out-char #\( #t r*)] [x x])
      (let ([r* (fmt/1 (car x) d? r*)])
	(cond
	 [(pair? (cdr x)) (loop (out-char #\space #t r*) (cdr x))]
	 [(null? (cdr x)) (out-char #\) #t r*)]
	 [else (out-char #\) (fmt/1 (cdr x d? (out-string " . " #t r*))))]))))
  (define (fmt/1 x d? r*)
    (cond
     [(eof-object? x) (out-string "#<eof>" #t r*)]
     [(input-port? x) (out-string "#<iport>" #t r*)]
     [(output-port? x) (out-string "#<oport>" #t r*)]
     [(symbol? x) (out-string (symbol->string x) #t r*)]
     [(pair? x) (out-pair x d? r*)]
     [(fixnum? x) (out-string (number->string x) #t r*)]
     [(flonum? x) (out-string (number->string x) #t r*)]
     [(null? x) (out-string "()" #t r*)]
     [(boolean? x) (out-string (if x "#t" "#f") #t r*)]
     [(char? x) (out-char x d? r*)]
     [(string? x) (out-string x d? r*)]
     [(vector? x) (out-vector x d? r*)]
     [(box? x) (out-box x d? r*)]
     [(procedure? x) (out-string "#<procedure>" #t r*)]
     [else (out-string "#<unknown>" #t r*)]))

  (let loop ([flst (string->list fmt)] [arg* arg*] [r* '()])
    (cond
     [(null? flst) (list->string (reverse r*))]
     [(char=? (car flst) #\~)
      (and (pair? (cdr flst))
	   (let ([c (cadr flst)])
	     (cond
	      [(char=? c #\a) (loop (cddr flst) (cdr arg*)
				    (fmt/1 (car arg*) #t r*))]
	      [(char=? c #\s) (loop (cddr flst) (cdr arg*)
				    (fmt/1 (car arg*) #f r*))]
	      [(char=? c #\~) (loop (cddr flst) arg* (cons #\~ r*))]
	      [(char=? c #\%) (loop (cddr flst) arg* (cons #\newline r*))]
	      [else (ic-error 'q-fmt "unrecognized format ~a" (cadr flst))])))]
     [else (loop (cdr flst) arg* (cons (car flst) r*))])))

(define-syntax q-fmt
  (syntax-rules ()
    [(_ fmt arg ...) (q-fmt* fmt (list arg ...))]))