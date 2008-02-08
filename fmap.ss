;; Functional map.
;;
#fload "sfc.sf"
#fload "common.sf"
#fload "error.ss"

;; CPO
(define (fmap-compare a b) ; => -1,0,1; transitive and antisymmetric
  (define (cmp-vector a b)
    (let ([n-a (vector-length a)]
	  [n-b (vector-length b)])
      (cond
       [(= n-a n-b) (let loop ([i 0])
		      (cond
		       [(= i n-a) 0]
		       [else (let ([x (cmp (vector-ref a i) (vector-ref b i))])
			       (cond
				[(zero? x) (loop (+ i 1))]
				[else x]))]))]
       [else (normalize (- n-a n-b))])))
  (define (cmp-string a b)
    (cond
     [(string<? a b) -1]
     [(string>? a b) +1]
     [else 0]))
  (define (compare-vector a b)
    (cond
     [(null? b) +1]
     [(boolean? b) +1]
     [(char? b) +1]
     [(number? b) +1]
     [(symbol? b) +1]
     [(string? b) +1]
     [(pair? b) +1]
     [(vector? b) (cmp-vector a b)]
     [else (ic-error 'fmap-compare "Unsupported argument ~a" b)]))
  (define (compare-pair a b)
    (cond
     [(null? b) +1]
     [(boolean? b) +1]
     [(char? b) +1]
     [(number? b) +1]
     [(symbol? b) +1]
     [(string? b) +1]
     [(pair? b) (let ([h (cmp (car a) (car b))])
		  (cond
		   [(zero? h) (cmp-x (cdr a) (cdr b))]
		   [else h]))]
     [else -1]))
  (define (compare-string a b)
    (cond
     [(null? b) +1]
     [(boolean? b) +1]
     [(char? b) +1]
     [(number? b) +1]
     [(symbol? b) +1]
     [(string? b) (cmp-string a b)]
     [else -1]))
  (define (compare-symbol a b)
    (cond
     [(null? b) +1]
     [(boolean? b) +1]
     [(char? b) +1]
     [(number? b) +1]
     [(symbol? b) (normalize (cmp-string (symbol->string a)
					 (symbol->string b)))]
     [else -1]))
  (define (compare-number a b)
    (cond
     [(null? b) +1]
     [(boolean? b) +1]
     [(char? b) +1]
     [(number? b) (normalize (- a b))]
     [else -1]))
  (define (compare-char a b)
    (cond
     [(null? b) +1]
     [(boolean? b) +1]
     [(char? b) (normalize (- (char->integer a) (char->integer b)))]
     [else -1]))
  (define (compare-boolean a b)
    (cond
     [(null? b) +1]
     [(boolean? b) (if a +1 -1)]
     [else -1]))
  (define (normalize v)
    (cond
     [(negative? v) -1]
     [(positive? v) +1]
     [else 0]))
  (define (cmp a b)
    (cond
     [(equal? a b) 0]
     [else (cmp-x a b)]))
  (define (cmp-x a b)
    (cond
     [(null? a) -1]
     [(boolean? a) (compare-boolean a b)]
     [(char? a) (compare-char a b)]
     [(number? a) (compare-number a b)]
     [(symbol? a) (compare-symbol a b)]
     [(string? a) (compare-string a b)]
     [(pair? a) (compare-pair a b)]
     [(vector? a) (compare-vector a b)]
     [else (ic-error 'fmap-compare "Unsupported argument ~a" a)]))

  (cmp a b))

;; treap variants
(define-variant treap-empty ())
(define-variant treap-node (key value hash left right))

;;;;;
(define (dump-fmap fm)
  (fprintf error-port "~%FMAP DUMP~%")
  (let loop ([fm fm] [p ""] [r ">"])
    (variant-case fm
      [treap-empty () #f]
      [treap-node (key value left right)
	(loop left (string-append p " ") " ")
	(fprintf error-port "~a~a~s ...~%" r p key)
	(loop right (string-append p " ") " ")]))
  (fprintf error-port "FMAP END~%~%"))

;; fmap operations
(define empty-fmap
  (let ([e (make-treap-empty)])
    (lambda () e)))

(define (lookup-fmap fm k success failure)
(if (or (equal? k '(type g-op))
	(equal? k '(type elem-op)))
    (begin
      (fprintf error-port "lookup-fmap ~s~%ENV:~%" k)
;      (dump-fmap fm)
      ))
  (let loop ([fm fm])
    (variant-case fm
      [treap-empty () (failure)]
      [treap-node (key value left right)
	(let ([z (fmap-compare k key)])
(if (or (equal? k '(type g-op))
	(equal? k '(type elem-op)))
    (fprintf error-port "    loop ~s ~s~%" key value))
	  (case z
	    [(-1) (loop left)]
	    [(+1) (loop right)]
	    [else (success value)]))])))

(define extend-fmap
  (let ()
    (define fmap-random!
      (let ([rstep 0.6180339887498948] [x 0.0])
	(lambda ()
	  (let ([y x]
		[z (fl+ x rstep)])
	    (set! x (if (fl<? z 1.0) z (- z 1.0)))
	    y))))
    (define (ref-ptr p) ((car p)))
    (define (set-ptr! p v) ((cdr p) v))
    (define (mk-left p)
      (cons (lambda () (treap-node->left p))
	    (lambda (v) (set-treap-node-left! p v))))
    (define (mk-right p)
      (cons (lambda () (treap-node->right p))
	    (lambda (v) (set-treap-node-right! p v))))
    (lambda (fm k v)
      (define (mk-addr) (cons (lambda () fm) (lambda (v) (set! fm v))))
      (define (update ignore)
	(let loop ([p (mk-addr)])
	  (let ([ref (ref-ptr p)])
	    (variant-case ref
	      [treap-node (key hash left right)
		(let ([z (fmap-compare k key)])
		  (case z
		    [(0)
(fprintf error-port "fmap:: Update empty~%")
		     (set-ptr! p (make-treap-node k v hash left right)) fm]
		    [(-1) (let ([x (copy ref)])
			    (set-ptr! p x)
(fprintf error-port "fmap:: Update move left~%")
			    (loop (mk-left x)))]
		    [(+1) (let ([x (copy ref)])
			    (set-ptr! p x)
(fprintf error-port "fmap:: Update move right~%")
			    (loop (mk-right x)))]
		    [else (ic-error 'extend-fmap
				    "Unexpected fmap-compare value ~a"
				    z)]))]))))
      (define (copy t)
	(variant-case t
	  [treap-empty () t]
	  [treap-node (key value hash left right)
            (make-treap-node key value hash left right)]))
      (define (insert)
	(let* ([h (fmap-random!)]
	       [kv (make-treap-node k v h (empty-fmap) (empty-fmap))])
	  (let loop ([p (mk-addr)])
	    (let ([ref (ref-ptr p)])
	      (variant-case ref
		[treap-empty ()
(fprintf error-port "fmap:: Insert on empty (loop)~%")
 (set-ptr! p kv) fm]
		[treap-node (key hash)
	          (cond
		   [(fl<? h hash)
		    (let reorder ([lf (mk-left kv)] [rt (mk-right kv)]
				  [q (begin (set-ptr! p kv) (copy ref))])
		      (variant-case q
			[treap-empty () (set-ptr! lf (empty-fmap))
				        (set-ptr! rt (empty-fmap))
(fprintf error-port "fmap:: Insert on empty (reorder)~%")
					fm]
			[treap-node (key left right)
		          (let ([z (fmap-compare k key)])
			    (case z
			      [(-1) (set-ptr! rt q)
(fprintf error-port "fmap:: Insert left move~%")
			            (reorder lf (mk-left q) (copy left))]
			      [(+1) (set-ptr! lf q)
(fprintf error-port "fmap:: Insert right move~%")
			            (reorder (mk-right q) rt (copy right))]
			      [else (ic-error 'extend-fmap
					      "can't happen (reorder)")]))]))]
		   [else (let ([z (fmap-compare k key)]
			       [x (copy ref)])
			   (set-ptr! p x)
			   (case z
			     [(-1)
(fprintf error-port "fmap:: Insert shift left~%")
(loop (mk-left x))]
			     [(+1)
(fprintf error-port "fmap:: Insert shift right~%")
 (loop (mk-right x))]
			     [else (ic-error 'extend-fmap
					     "can't happen (loop)")]))])])))))
(fprintf error-port "extend-fmap k=~s, v=~s~%" k v)
(if (equal? k '(param elem-op))
    (begin 
;      (dump-fmap fm)
      #f))
  (lookup-fmap fm k update insert))))

(define (fmap->alist fm)
  (let loop ([r* '()] [fm fm])
    (variant-case fm
      [treap-empty () r*]
      [treap-node (key value left right)
	(loop (cons (cons key value) (loop r* left)) right)])))

(define (fmap-for-each fm filter proc)
  (let loop ([fm fm])
    (variant-case fm
      [treap-empty () #f]
      [treap-node (key value left right)
	(loop left)
	(if (filter key value) (proc key value))
	(loop right)])))
