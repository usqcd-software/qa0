;; Complex to Quad Hummer converter
#fload "sfc.sf"
#fload "common.sf"
#fload "error.ss"
#fload "ast.ss"
;;
;; (provide complex->quad-hummer)
;;
(define complex->quad-hummer
  (let ()
    (define fresh-reg
      (let ([*reg-count* 0])
        (lambda ()
          (let ([x (gen-reg 'b *reg-count*)])
            (set! *reg-count* (+ *reg-count* 1))
            (make-reg x)))))
    (define (cx->qh-decl decl)
      (variant-case decl
        [qa0-proc (attr* name arg-name* arg-type* arg-c-name* arg-c-type* code*)
          (cx->qh-proc attr* name arg-name* arg-type* arg-c-name* arg-c-type* 
                       code*)]
        [else decl]))
    (define (cx->qh-proc attr* name
                         arg-name* arg-type*
                         arg-c-name* arg-c-type*
                         code*)
      (make-qa0-proc attr* name
                     arg-name* arg-type*
                     arg-c-name* arg-c-type*
                     (cx->qh-code* code*)))
    (define (cx->qh-code* code*)
      (let loop ([code* code*] [r* '()])
        (cond
         [(null? code*) (reverse r*)]
         [else (loop (cdr code*) (cx->qh-code (car code*) r*))])))
    (define (cx->qh-code code r*)
      (variant-case code
        [qa0-operation (attr* name output* input*)
          (cx->qh-operation code attr* name output* input* r*)]
        [qa0-load (attr* type output addr*)
          (cx->qh-load attr* type output addr* r*)]
        [qa0-store (attr* type addr* value)
          (cx->qh-store attr* type addr* value r*)]
        [qa0-loop (attr* var low high code*)
          (cx->qh-loop attr* var low high code* r*)]
        [qa0-if (var true-code* false-code*)
          (cx->qh-if var true-code* false-code* r*)]))
    (define (qho-generic attr* qh-op output* input* r*)
      (cons (make-qa0-operation attr* qh-op output* input*) r*))
    (define (check-output* name out* len)
      (check-io "output" name out* len))
    (define (check-input* name in* len)
      (check-io "input" name in* len))
    (define (check-io cl name r* len)
      (if (not (= (length r*) len))
          (s-error "op ~a: ~a should be of ~a elements, found ~a"
                   name cl len r*)))
    (define (qho-mul attr* output* input* r*)
      (check-output* 'complex-mul output* 1)
      (check-input* 'complex-mul input* 2)
      (let ([x (fresh-reg)])
        (list* (make-qa0-operation attr* 'qh-mul-b output* (cons x input*))
               (make-qa0-operation attr* 'qh-mul-a (list x) input*)
               r*)))
    (define (qho-madd attr* output* input* r*)
      (check-output* 'complex-mul output* 1)
      (check-input* 'complex-mul input* 3)
      (let ([x (fresh-reg)])
        (list* (make-qa0-operation attr* 'qh-madd-b output*
                                   (cons x (cdr input*)))
               (make-qa0-operation attr* 'qh-madd-a (list x) input*)
               r*)))
    (define (qho-msub attr* output* input* r*)
      (check-output* 'complex-mul output* 1)
      (check-input* 'complex-mul input* 3)
      (let ([x (fresh-reg)])
        (list* (make-qa0-operation attr* 'qh-msub-b output*
                                   (cons x (cdr input*)))
               (make-qa0-operation attr* 'qh-msub-a (list x) input*)
               r*)))
    (define (qho-cmul attr* output* input* r*)
      (check-output* 'complex-cmul output* 1)
      (check-input* 'complex-cmul input* 2)
      (let ([x (fresh-reg)])
        (list* (make-qa0-operation attr* 'qh-cmul-b output* (cons x input*))
               (make-qa0-operation attr* 'qh-cmul-a (list x) input*)
               r*)))
    (define (qho-cmadd attr* output* input* r*)
      (check-output* 'complex-cmadd output* 1)
      (check-input* 'complex-cmadd input* 3)
      (let ([x (fresh-reg)])
        (list* (make-qa0-operation attr* 'qh-cmadd-b output*
                                   (cons x (cdr input*)))
               (make-qa0-operation attr* 'qh-cmadd-a (list x) input*)
               r*)))
    (define (qho-cmsub attr* output* input* r*)
      (check-output* 'complex-cmsub output* 1)
      (check-input* 'complex-cmsub input* 3)
      (let ([x (fresh-reg)])
        (list* (make-qa0-operation attr* 'qh-cmsub-b output*
                                   (cons x (cdr input*)))
               (make-qa0-operation attr* 'qh-cmsub-a (list x) input*)
               r*)))
    (define (qho-dot-add attr* output* input* r*)
      (check-output* 'complex-dot-add output* 1)
      (check-input* 'complex-dot-add input* 3)
      (let ([x (fresh-reg)])
        (list* (make-qa0-operation attr* 'qh-dot-add-b output*
                                   (cons x (cdr input*)))
               (make-qa0-operation attr* 'qh-dot-add-a (list x) input*)
               r*)))
    (define (qho-dot-add-i attr* output* input* r*)
      (check-output* 'complex-dot-add-i output* 1)
      (check-input* 'complex-dot-add-i input* 3)
      (let ([x (fresh-reg)])
        (list* (make-qa0-operation attr* 'qh-dot-add-i-b output*
                                   (cons x (cdr input*)))
               (make-qa0-operation attr* 'qh-dot-add-i-a (list x) input*)
               r*)))
    (define (qho-dot-sub attr* output* input* r*)
      (check-output* 'complex-dot-sub output* 1)
      (check-input* 'complex-dot-sub input* 3)
      (let ([x (fresh-reg)])
        (list* (make-qa0-operation attr* 'qh-dot-sub-b output*
                                   (cons x (cdr input*)))
               (make-qa0-operation attr* 'qh-dot-sub-a (list x) input*)
               r*)))
    (define (qho-dot-sub-i attr* output* input* r*)
      (check-output* 'complex-dot-sub-i output* 1)
      (check-input* 'complex-dot-sub-i input* 3)
      (let ([x (fresh-reg)])
        (list* (make-qa0-operation attr* 'qh-dot-sub-i-b output*
                                   (cons x (cdr input*)))
               (make-qa0-operation attr* 'qh-dot-sub-i-a (list x) input*)
               r*)))
    (define qh-optable
      (list
       (list* 'complex-zero                 'qh-zero        #f             )
       (list* 'complex-move                 'qh-move        #f             )
       (list* 'complex                      'qh-make        #f             )
       (list* 'complex-real                 'qh-real        #f             )
       (list* 'complex-imag                 'qh-imag        #f             )
       (list* 'complex-neg                  'qh-neg         #f             )
       (list* 'complex-times-plus-i         'qh-times-plus-i    #f         )
       (list* 'complex-times-minus-i        'qh-times-minus-i   #f         )
       (list* 'complex-add                  'qh-add         #f             )
       (list* 'complex-sub                  'qh-sub         #f             )
       (list* 'complex-rmul                 'qh-rmul        #f             )
       (list* 'complex-mul                  #f              qho-mul        )
       (list* 'complex-madd                 #f              qho-madd       )
       (list* 'complex-msub                 #f              qho-msub       )
       (list* 'complex-rmadd                'qh-rmadd       #f             )
       (list* 'complex-rmsub                'qh-rmsub       #f             )
       (list* 'complex-cmul                 #f              qho-cmul       )
       (list* 'complex-cmadd                #f              qho-cmadd      )
       (list* 'complex-cmsub                #f              qho-cmsub      )
       (list* 'complex-add-i                'qh-add-i       #f             )
       (list* 'complex-sub-i                'qh-sub-i       #f             )
       (list* 'complex-real-cmul-conj-init  'qh-real-cmul-conj-init   #f   )
       (list* 'complex-real-cmul-conj-add   'qh-real-cmul-conj-add    #f   )
       (list* 'complex-real-cmul-conj-fini  'qh-real-cmul-conj-fini   #f   )
       (list* 'complex-norm-init            'qh-norm-init   #f             )
       (list* 'complex-norm-add             'qh-norm-add    #f             )
       (list* 'complex-norm-fini            'qh-norm-fini   #f             )
       (list* 'complex-dot-init             'qh-dot-init    #f             )
       (list* 'complex-dot-add              #f              qho-dot-add    )
       (list* 'complex-dot-add-i            #f              qho-dot-add-i  )
       (list* 'complex-dot-sub              #f              qho-dot-sub    )
       (list* 'complex-dot-sub-i            #f              qho-dot-sub-i  )
       (list* 'complex-dot-fini             'qh-dot-fini    #f             )))
    (define (cx->qh-operation code attr* name output* input* r*)
      (let ([trans (assq name qh-optable)])
        (cond
         [(not trans) (cons code r*)]
         [(cadr trans) (qho-generic attr* (cadr trans) output* input* r*)]
         [else ((cddr trans) attr* output* input* r*)])))
    (define (cx->qh-load attr* type output addr* r*)
      (cons (make-qa0-load attr* (convert-type type) output addr*)
            r*))
    (define (qho-store-float attr* addr* value r*)
      (let ([x (fresh-reg)])
        (list* (make-qa0-store attr* 'qh-float addr* x)
               (make-qa0-operation '() 'qh->float (list x) (list value))
               r*)))
    (define (cx->qh-store attr* type addr* value r*)
      (if (eq? type 'complex-float)
          (qho-store-float attr* addr* value r*)
          (cons (make-qa0-store attr* (convert-type type) addr* value)
                r*)))
    (define (cx->qh-loop attr* var low high code* r*)
      (cons (make-qa0-loop attr* var low high (cx->qh-code* code*))
            r*))
    (define (cx->qh-if var true-code* false-code* r*)
      (cons (make-qa0-if var (cx->qh-code* true-code*)
                         (cx->qh-code* false-code*))
            r*))
    (define (convert-type type)
      (case type
        [(complex-float) 'qh-float]
        [(complex-double) 'qh-double]
        [else type]))

    (lambda (qa0)
      (variant-case qa0
        [qa0-top (decl*)
          (let loop ([decl* decl*] [r* '()])
            (cond
             [(null? decl*) (make-qa0-top (reverse r*))]
             [else (loop (cdr decl*)
                         (cons (cx->qh-decl (car decl*)) r*))]))]))))
