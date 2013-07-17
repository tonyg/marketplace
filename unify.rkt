#lang racket/base

(require racket/set)
(require racket/match)
(require (only-in racket/class object?))
(require "struct-map.rkt")

(provide (struct-out variable)
	 (struct-out canonical-variable)
	 wild
	 wild?
	 non-wild?
	 ground?
	 variables-in
	 unify
	 unify/env
	 unify/vars
	 unify-match/vars
	 freshen
	 canonicalize
	 mgu-freshen
	 mgu-canonical
	 apply-subst
	 specialization?
	 upper-case-symbols->variables
	 upper-case-symbols->canonical)

;; A Subst is a Maybe<AList<Variable,Any>>.
;; TODO: semantics

;; Compared by eq?, not equal?. In particular, info is not involved in
;; equivalence.
(struct variable (info)
	#:property prop:custom-write
	(lambda (v port mode)
	  (display "?" port)
	  (write (variable-info v) port)))

;; Compared by equal?, not eq?. The number is a part of the
;; appropriate equivalence relation for canonical-variables.
(struct canonical-variable (index) #:transparent
	#:property prop:custom-write
	(lambda (v port mode)
	  (display "?!" port)
	  (write (canonical-variable-index v) port)))

;; -> Variable
;; Create a fresh (and hence unconstrained) variable.
(define (wild [base-name '_])
  (variable (gensym base-name)))

;; Any -> Boolean
;; True iff the argument is a variable or canonical-variable.
(define (wild? x)
  (or (variable? x) (canonical-variable? x)))

;; Any -> Boolean
;; True iff the argument is neither a variable nor a canonical-variable.
(define (non-wild? x)
  (not (wild? x)))

;; Any -> Boolean
;; Racket objects are structures, so we reject them explicitly for
;; now, leaving them opaque to unification.
(define (non-object-struct? x)
  (and (struct? x)
       (not (object? x))))

;; Any -> Set<Variable>
(define (variables-in x)
  (let walk ((x x) (acc (set)))
    (cond
     [(wild? x) (set-add acc x)]
     [(pair? x) (walk (car x) (walk (cdr x) acc))]
     [(vector? x) (foldl walk acc (vector->list x))]
     [(non-object-struct? x) (walk (struct->vector x #f) acc)]
     [else acc])))

;; Any -> Boolean
;; True iff the term is completely ground, that is has no variables or
;; canonical-variables in it.
(define (ground? x)
  (let walk ((x x))
    (cond
     [(wild? x) #f]
     [(pair? x) (and (walk (car x)) (walk (cdr x)))]
     [(vector? x) (andmap walk (vector->list x))]
     [(non-object-struct? x) (walk (struct->vector x #f))]
     [else #t])))

;; Variable Any -> Boolean
(define (occurs? var val)
  (let walk ((x val))
    (cond
     [(eq? var x) #t]
     [(pair? x) (or (walk (car x)) (walk (cdr x)))]
     [(vector? x) (ormap walk (vector->list x))]
     [(non-object-struct? x) (walk (struct->vector x #f))]
     [else #f])))

;; Variable Any Subst -> Subst
(define (extend-subst var val env)
  (cond
   [(eq? var val)
    ;; Avoid trivial tautologies. Less trivial ones are not detected,
    ;; but are harmless.
    env]
   [(occurs? var val)
    ;; Occurs check.
    #f]
   [else
    (cons (cons var val) env)]))

;; Any Subst Set<Variable> -> Any
(define (chase x env seen)
  (if (variable? x)
      (cond [(set-member? seen x) x]
	    [(assq x env) => (lambda (cell) (chase (cdr cell) env (set-add seen x)))]
	    [else x])
      x))

;; Any Any -> Subst
(define (unify a b)
  (unify/env a b '()))

;; Any Any Subst -> Subst
(define (unify/env a0 b0 env)
  (let walk ((a0 a0) (b0 b0) (env env))
    (and env
	 (let ((a (chase a0 env (set)))
	       (b (chase b0 env (set))))
	   (cond
	    [(variable? a) (extend-subst a b env)]
	    [(variable? b) (extend-subst b a env)]
	    [(and (pair? a) (pair? b))
	     (walk (car a) (car b) (walk (cdr a) (cdr b) env))]
	    [(and (vector? a) (vector? b) (= (vector-length a) (vector-length b)))
	     (for/fold ([env env]) ([ea a] [eb b]) (walk ea eb env))]
	    [(and (non-object-struct? a) (non-object-struct? b))
	     (walk (struct->vector a #f) (struct->vector b #f) env)]
	    [else (and (equal? a b) env)])))))

;; Any -> (values Any AList<Symbol,Variable>)
;; Converts upper-case symbols to variables, making sure that
;; eq? symbols map to eq? variables.
(define (upper-case-symbols->variables x)
  (let walk ((x x) (env '()))
    (cond
     [(upper-case-symbol? x)
      (cond [(assq x env) => (lambda (cell) (values (cdr cell) env))]
	    [else (let ((v (variable x))) (values v (cons (cons x v) env)))])]
     [(pair? x)
      (define-values (a env1) (walk (car x) env))
      (define-values (d env2) (walk (cdr x) env1))
      (values (cons a d) env2)]
     [(vector? x)
      (define result (make-vector (vector-length x)))
      (values result (for/fold ([env env]) ([i (vector-length x)])
		       (define-values (val env1) (walk (vector-ref x i) env))
		       (vector-set! result i val)
		       env1))]
     [(non-object-struct? x) (struct-map/accumulator walk env x)]
     [else (values x env)])))

;; Any -> Any
(define (upper-case-symbols->canonical t)
  (define env (make-hash)) ;; cheeky use of mutation
  (let walk ((t t))
    (cond
     [(or (upper-case-symbol? t) (wild? t))
      (cond [(hash-ref env t #f)]
	    [else (define v (canonical-variable (hash-count env))) (hash-set! env t v) v])]
     [(pair? t) (cons (walk (car t)) (walk (cdr t)))]
     [(vector? t) (list->vector (map walk (vector->list t)))]
     [(non-object-struct? t) (struct-map walk t)]
     [else t])))

;; Any -> Boolean
(define (upper-case-symbol? x)
  (and (symbol? x)
       (let ((name (symbol->string x)))
	 (and (positive? (string-length name))
	      (char-upper-case? (string-ref name 0))))))

;; AList<A,B> -> AList<B,A>
(define (flip-env env)
  (map (lambda (x) (cons (cdr x) (car x))) env))

;; Any Any -> Subst
;; Like unify after upper-case-symbols->variables on both arguments.
(define (unify/vars a b)
  (define-values (processed env) (upper-case-symbols->variables (cons a b)))
  (define s (unify (car processed) (cdr processed)))
  (and s (apply-subst s env)))

;; Any Any -> Subst
;; Like unify-match after upper-case-symbols->variables on both
;; arguments, extracting bindings only from the first argument.
(define (unify-match/vars a b)
  (define-values (pa a-env) (upper-case-symbols->variables a))
  (define-values (pb b-env) (upper-case-symbols->variables b))
  (define s (unify pa pb))
  (and s (apply-subst s a-env)))

;; Utility used by freshen and canonicalize below.
;; Must visit the term in the order specified by canonicalize
;; below. Here we rely both upon Racket's left-to-right evaluation
;; order and upon defined struct-mappers traversing their arguments in
;; some deterministic order.
(define (freshen* t var-handler canon-handler)
  (define env (make-hash)) ;; cheeky use of mutation
  (let walk ((t t))
    (cond
     [(wild? t)
      (cond [(hash-ref env t #f)]
	    [else (define v ((if (canonical-variable? t) canon-handler var-handler) t env))
		  (hash-set! env t v)
		  v])]
     [(pair? t) (cons (walk (car t)) (walk (cdr t)))]
     [(vector? t) (list->vector (map walk (vector->list t)))]
     [(non-object-struct? t) (struct-map walk t)]
     [else t])))

;; Any -> Any
;;
;; Freshens a term by substituting out variables in the term with
;; fresh variables to produce an arbitrary member of the term's
;; alpha-equivalence-class that shares no variables with the original.
;;
;; Treats canonical-variables just like regular ones, freshening them
;; with new ordinary (non-canonical) variables.
(define (freshen t)
  (freshen* t
	    (lambda (var env) (variable (variable-info var)))
	    (lambda (var env) (variable (canonical-variable-index var)))))

;; Any -> Any
;;
;; Canonicalizes a term by substituting out variables in the term with
;; canonical-variables to produce a canonical member of the term's
;; alpha-equivalence-class.
;;
;; Canonical variables are used in a structurally-determined order
;; related to print order: generally, all unseen variables to the left
;; of a term's print representation are given canonical equivalents
;; before those to the right.
;;
;; Canonical-variables may not appear in the input term.
(define (canonicalize t)
  (freshen* t
	    (lambda (var env) (canonical-variable (hash-count env)))
	    (lambda (var env) (canonical-variable (hash-count env)))))

;; Any Any -> Any
;; If the arguments unify, applies the substitution to one of them,
;; yielding a most general unifier, and then freshens the result.
(define (mgu-freshen a b)
  (define sub (unify a b))
  (and sub (freshen (apply-subst sub a))))

;; Any Any -> Any
;; If the arguments unify, applies the substitution to one of them,
;; yielding a most general unifier, and then canonicalizes the result.
(define (mgu-canonical a b)
  (define sub (unify a b))
  (and sub (canonicalize (apply-subst sub a))))

;; Subst Any -> Any
(define (apply-subst env x)
  (let walk ((x0 x))
    (define x (chase x0 env (set)))
    (cond
     [(pair? x) (cons (walk (car x)) (walk (cdr x)))]
     [(vector? x) (list->vector (map walk (vector->list x)))]
     [(non-object-struct? x) (struct-map walk x)]
     [else x])))

;; True iff a is a specialization (or instance) of b.
(define (specialization? a b)
  (let walk ((a a) (b b))
    (cond
     [(wild? b) #t]
     [(wild? a) #f]
     [(and (pair? a) (pair? b))
      (and (walk (car a) (car b)) (walk (cdr a) (cdr b)))]
     [(and (vector? a) (vector? b) (= (vector-length a) (vector-length b)))
      (for/and ([aa a] [bb b]) (walk aa bb))]
     [(and (non-object-struct? a) (non-object-struct? b))
      (walk (struct->vector a #f) (struct->vector b #f))]
     [else (equal? a b)])))

(require racket/trace)
(trace ;;unify/env
       ;;upper-case-symbols->variables
       ;;apply-subst
       ;;specialization?
       )
