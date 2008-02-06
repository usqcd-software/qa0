;; Complex to Double Hummer converter
#fload "sfc.sf"
#fload "common.sf"
#fload "ast.ss"
;;
;; (provide complex->double-hummer)
;;
(define complex->double-hummer
  (let ()
    (define fresh-reg
      (let ([*reg-count* 0])
	(lambda ()
	  (let ([x (gen-reg 'b *reg-count*)])
	    (set! *reg-count* (+ *reg-count* 1))
	    (make-reg x)))))
    (define (cx->dh-decl decl)
      (variant-case decl
	[qa0-proc (attr* name arg-name* arg-type* arg-c-name* arg-c-type* code*)
	  (cx->dh-proc attr* name arg-name* arg-type* arg-c-name* arg-c-type* 
		       code*)]
	[else decl]))
    (define (cx->dh-proc attr* name
			 arg-name* arg-type*
			 arg-c-name* arg-c-type*
			 code*)
      (make-qa0-proc attr* name
		     arg-name* arg-type*
		     arg-c-name* arg-c-type*
		     (cx->dh-code* code*)))
    (define (cx->dh-code* code*)
      (let loop ([code* code*] [r* '()])
	(cond
	 [(null? code*) (reverse r*)]
	 [else (loop (cdr code*) (cx->dh-code (car code*) r*))])))
    (define (cx->dh-code code r*)
      (variant-case code
	[qa0-operation (attr* name output* input*)
	  (cx->dh-operation code attr* name output* input* r*)]
	[qa0-load (attr* type output addr*)
	  (cx->dh-load attr* type output addr* r*)]
	[qa0-store (attr* type addr* value)
	  (cx->dh-store attr* type addr* value r*)]
	[qa0-loop (attr* var low high code*)
	  (cx->dh-loop attr* var low high code* r*)]
	[qa0-if (var true-code* false-code*)
	  (cx->dh-if var true-code* false-code* r*)]))
    (define (dho-generic attr* dh-op output* input* r*)
      (cons (make-qa0-operation attr* dh-op output* input*) r*))
    (define (check-output* name out* len)
      (check-io "output" name out* len))
    (define (check-input* name in* len)
      (check-io "input" name in* len))
    (define (check-io cl name r* len)
      (if (not (= (length r*) len))
	  (error 'cx2dh "op ~a: ~a should be of ~a elements, found ~a"
		 name cl len r*)))
    (define (dho-mul attr* output* input* r*)
      (check-output* 'complex-mul output* 1)
      (check-input* 'complex-mul input* 2)
      (let ([x (fresh-reg)])
	(cons* (make-qa0-operation attr* 'dh-mul-b output* (cons x input*))
	       (make-qa0-operation attr* 'dh-mul-a (list x) input*)
	       r*)))
    (define (dho-madd attr* output* input* r*)
      (check-output* 'complex-mul output* 1)
      (check-input* 'complex-mul input* 3)
      (let ([x (fresh-reg)])
	(cons* (make-qa0-operation attr* 'dh-madd-b output*
				   (cons x (cdr input*)))
	       (make-qa0-operation attr* 'dh-madd-a (list x) input*)
	       r*)))
    (define (dho-cmul attr* output* input* r*)
      (check-output* 'complex-mul output* 1)
      (check-input* 'complex-mul input* 2)
      (let ([x (fresh-reg)])
	(cons* (make-qa0-operation attr* 'dh-cmul-b output* (cons x input*))
	       (make-qa0-operation attr* 'dh-cmul-a (list x) input*)
	       r*)))
    (define (dho-cmadd attr* output* input* r*)
      (check-output* 'complex-mul output* 1)
      (check-input* 'complex-mul input* 3)
      (let ([x (fresh-reg)])
	(cons* (make-qa0-operation attr* 'dh-cmadd-b output*
				   (cons x (cdr input*)))
	       (make-qa0-operation attr* 'dh-cmadd-a (list x) input*)
	       r*)))
    (define (dho-dot-add attr* output* input* r*)
      (check-output* 'complex-mul output* 1)
      (check-input* 'complex-mul input* 3)
      (let ([x (fresh-reg)])
	(cons* (make-qa0-operation attr* 'dh-dot-b output*
				   (cons x (cdr input*)))
	       (make-qa0-operation attr* 'dh-dot-a (list x) input*)
	       r*)))
    (define (dho-store-float attr* addr* value r*)
      (let ([x (fresh-reg)])
	(cons* (make-qa0-store attr* 'dh-float addr* x)
	       (make-qa0-operation '() 'dh->float (list x) (list value))
	       r*)))
    (define dh-optable
      (list
       (cons* 'complex-move            'dh-move            #f            )
       (cons* 'complex                 'dh-make            #f            )
       (cons* 'complex-real            'dh-real            #f            )
       (cons* 'complex-imag            'dh-imag            #f            )
       (cons* 'complex-neg             'dh-neg             #f            )
       (cons* 'complex-times-plus-i    'dh-times-plus-i    #f            )
       (cons* 'complex-times-minus-i   'dh-times-minus-i   #f            )
       (cons* 'complex-add             'dh-add             #f            )
       (cons* 'complex-sub             'dh-sub             #f            )
       (cons* 'complex-rmul            'dh-rmul            #f            )
       (cons* 'complex-mul             #f                  dho-mul       )
       (cons* 'complex-madd            #f                  dho-madd      )
       (cons* 'complex-rmadd           'dh-rmadd           #f            )
       (cons* 'complex-rmsub           'dh-rmsub           #f            )
       (cons* 'complex-cmul            #f                  dho-cmul      )
       (cons* 'complex-cmadd           #f                  dho-cmadd     )
       (cons* 'complex-add-i           'dh-add-i           #f            )
       (cons* 'complex-sub-i           'dh-sub-i           #f            )
       (cons* 'complex-norm-init       'dh-norm-init       #f            )
       (cons* 'complex-norm-add        'dh-norm-add        #f            )
       (cons* 'complex-norm-fini       'dh-norm-fini       #f            )
       (cons* 'complex-dot-init        'dh-dot-init        #f            )
       (cons* 'complex-dot-add         #f                  dho-dot-add   )
       (cons* 'complex-dot-fini        'dh-dot-fini        #f            )))
    (define (cx->dh-operation code attr* name output* input* r*)
      (let ([trans (assq name dh-optable)])
	(cond
	 [(not trans) (cons code r*)]
	 [(cadr trans) (dho-generic attr* (cadr trans) output* input* r*)]
	 [else ((cddr trans) attr* output* input* r*)])))
    (define (cx->dh-load attr* type output addr* r*)
      (cons (make-qa0-load attr* (convert-type type) output addr*)
	    r*))
    (define (cx->dh-store attr* type addr* value r*)
      (if (eq? type 'complex-float)
	  (dho-store-float attr* addr* value r*)
	  (cons (make-qa0-store attr* (convert-type type) addr* value)
		r*)))
    (define (cx->dh-loop attr* var low high code* r*)
      (cons (make-qa0-loop attr* var low high (cx->dh-code* code*))
	    r*))
    (define (cx->dh-if var true-code* false-code* r*)
      (cons (make-qa0-if var (cx->dh-code* true-code*)
			 (cx->dh-code* false-code*))
	    r*))
    (define (convert-type type)
      (case type
	[(complex-float) 'dh-float]
	[(complex-double) 'dh-double]
	[else type]))

    (lambda (qa0)
      (variant-case qa0
	[qa0-top (decl*)
	  (let loop ([decl* decl*] [r* '()])
	    (cond
	     [(null? decl*) (make-qa0-top (reverse r*))]
	     [else (loop (cdr decl*)
			 (cons (cx->dh-decl (car decl*)) r*))]))]))))
