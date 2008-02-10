;; emit routine for verbose AST
#fload "sfc.sf"
#fload "print.ss"
;;
;; (provide emit-verbose)
;;
(define (emit-verbose key target* data*)
  (let loop ([target* target*] [data* data*])
    (cond
     [(null? target*) (q-print "~%")]
     [(or (eq? key (car target*))
	  (and (list? (car target*))
	       (memq key (car target*))))
      (q-print "~a" (car data*))
      (loop (cdr target*) (cdr data*))]
     [else (loop (cdr target*) (cdr data*))])))

