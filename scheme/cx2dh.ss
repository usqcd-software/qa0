;; Complex to Double Hummer converter
#fload "sfc.sf"
#fload "common.sf"
#fload "error.ss"
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
          (s-error "op ~a: ~a should be of ~a elements, found ~a"
                   name cl len r*)))
    (define (dho-mul attr* output* input* r*)
      (check-output* 'complex-mul output* 1)
      (check-input* 'complex-mul input* 2)
      (let ([x (fresh-reg)])
        (list* (make-qa0-operation attr* 'dh-mul-b output* (cons x input*))
               (make-qa0-operation attr* 'dh-mul-a (list x) input*)
               r*)))
    (define (dho-madd attr* output* input* r*)
      (check-output* 'complex-mul output* 1)
      (check-input* 'complex-mul input* 3)
      (let ([x (fresh-reg)])
        (list* (make-qa0-operation attr* 'dh-madd-b output*
                                   (cons x (cdr input*)))
               (make-qa0-operation attr* 'dh-madd-a (list x) input*)
               r*)))
    (define (dho-cmul attr* output* input* r*)
      (check-output* 'complex-cmul output* 1)
      (check-input* 'complex-cmul input* 2)
      (let ([x (fresh-reg)])
        (list* (make-qa0-operation attr* 'dh-cmul-b output* (cons x input*))
               (make-qa0-operation attr* 'dh-cmul-a (list x) input*)
               r*)))
    (define (dho-cmadd attr* output* input* r*)
      (check-output* 'complex-cmadd output* 1)
      (check-input* 'complex-cmadd input* 3)
      (let ([x (fresh-reg)])
        (list* (make-qa0-operation attr* 'dh-cmadd-b output*
                                   (cons x (cdr input*)))
               (make-qa0-operation attr* 'dh-cmadd-a (list x) input*)
               r*)))
    (define (dho-cmsub attr* output* input* r*)
      (check-output* 'complex-cmsub output* 1)
      (check-input* 'complex-cmsub input* 3)
      (let ([x (fresh-reg)])
        (list* (make-qa0-operation attr* 'dh-cmsub-b output*
                                   (cons x (cdr input*)))
               (make-qa0-operation attr* 'dh-cmsub-a (list x) input*)
               r*)))
    (define (dho-dot-add attr* output* input* r*)
      (check-output* 'complex-dot-add output* 1)
      (check-input* 'complex-dot-add input* 3)
      (let ([x (fresh-reg)])
        (list* (make-qa0-operation attr* 'dh-dot-add-b output*
                                   (cons x (cdr input*)))
               (make-qa0-operation attr* 'dh-dot-add-a (list x) input*)
               r*)))
    (define (dho-dot-add-i attr* output* input* r*)
      (check-output* 'complex-dot-add-i output* 1)
      (check-input* 'complex-dot-add-i input* 3)
      (let ([x (fresh-reg)])
        (list* (make-qa0-operation attr* 'dh-dot-add-i-b output*
                                   (cons x (cdr input*)))
               (make-qa0-operation attr* 'dh-dot-add-i-a (list x) input*)
               r*)))
    (define (dho-dot-sub attr* output* input* r*)
      (check-output* 'complex-dot-sub output* 1)
      (check-input* 'complex-dot-sub input* 3)
      (let ([x (fresh-reg)])
        (list* (make-qa0-operation attr* 'dh-dot-sub-b output*
                                   (cons x (cdr input*)))
               (make-qa0-operation attr* 'dh-dot-sub-a (list x) input*)
               r*)))
    (define (dho-dot-sub-i attr* output* input* r*)
      (check-output* 'complex-dot-sub-i output* 1)
      (check-input* 'complex-dot-sub-i input* 3)
      (let ([x (fresh-reg)])
        (list* (make-qa0-operation attr* 'dh-dot-sub-i-b output*
                                   (cons x (cdr input*)))
               (make-qa0-operation attr* 'dh-dot-sub-i-a (list x) input*)
               r*)))
    (define dh-optable
      (list
       (list* 'complex-zero                 'dh-zero        #f             )
       (list* 'complex-move                 'dh-move        #f             )
       (list* 'complex                      'dh-make        #f             )
       (list* 'complex-real                 'dh-real        #f             )
       (list* 'complex-imag                 'dh-imag        #f             )
       (list* 'complex-neg                  'dh-neg         #f             )
       (list* 'complex-times-plus-i         'dh-times-plus-i    #f         )
       (list* 'complex-times-minus-i        'dh-times-minus-i   #f         )
       (list* 'complex-add                  'dh-add         #f             )
       (list* 'complex-sub                  'dh-sub         #f             )
       (list* 'complex-rmul                 'dh-rmul        #f             )
       (list* 'complex-mul                  #f              dho-mul        )
       (list* 'complex-madd                 #f              dho-madd       )
       (list* 'complex-rmadd                'dh-rmadd       #f             )
       (list* 'complex-rmsub                'dh-rmsub       #f             )
       (list* 'complex-cmul                 #f              dho-cmul       )
       (list* 'complex-cmadd                #f              dho-cmadd      )
       (list* 'complex-cmsub                #f              dho-cmsub      )
       (list* 'complex-add-i                'dh-add-i       #f             )
       (list* 'complex-sub-i                'dh-sub-i       #f             )
       (list* 'complex-real-cmul-conj-init  'dh-real-cmul-conj-init   #f   )
       (list* 'complex-real-cmul-conj-add   'dh-real-cmul-conj-add    #f   )
       (list* 'complex-real-cmul-conj-fini  'dh-real-cmul-conj-fini   #f   )
       (list* 'complex-norm-init            'dh-norm-init   #f             )
       (list* 'complex-norm-add             'dh-norm-add    #f             )
       (list* 'complex-norm-fini            'dh-norm-fini   #f             )
       (list* 'complex-dot-init             'dh-dot-init    #f             )
       (list* 'complex-dot-add              #f              dho-dot-add    )
       (list* 'complex-dot-add-i            #f              dho-dot-add-i  )
       (list* 'complex-dot-sub              #f              dho-dot-sub    )
       (list* 'complex-dot-sub-i            #f              dho-dot-sub-i  )
       (list* 'complex-dot-fini             'dh-dot-fini    #f             )))
    (define (cx->dh-operation code attr* name output* input* r*)
      (let ([trans (assq name dh-optable)])
        (cond
         [(not trans) (cons code r*)]
         [(cadr trans) (dho-generic attr* (cadr trans) output* input* r*)]
         [else ((cddr trans) attr* output* input* r*)])))
    (define (cx->dh-load attr* type output addr* r*)
      (cons (make-qa0-load attr* (convert-type type) output addr*)
            r*))
    (define (dho-store-float attr* addr* value r*)
      (let ([x (fresh-reg)])
        (list* (make-qa0-store attr* 'dh-float addr* x)
               (make-qa0-operation '() 'dh->float (list x) (list value))
               r*)))
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
