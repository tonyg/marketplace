#lang typed/racket/base
;; Limited support for reasoning about subtyped of a polymorphic base struct type in TR.

(require (for-syntax racket/base))
(require racket/match)

(provide pseudo-substruct:)

(define-syntax-rule (pseudo-substruct: (super-type TypeParam ...) SubType sub-type sub-type?)
  (begin (define-type SubType (super-type TypeParam ...))
	 (define-predicate sub-type? SubType)
	 (define-match-expander sub-type
	   (lambda (stx)
	     (syntax-case stx () [(_ f (... ...)) #'(? sub-type? (super-type f (... ...)))]))
	   (lambda (stx)
	     (syntax-case stx ()
	       [x (identifier? #'x) #'(inst super-type TypeParam ...)]
	       [(_ f (... ...)) #'((inst super-type TypeParam ...) f (... ...))])))))
