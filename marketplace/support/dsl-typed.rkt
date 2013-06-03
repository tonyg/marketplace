#lang typed/racket/base

(require (for-syntax typed/racket/base))
(provide define&provide-dsl-helper-syntaxes)
(provide literal-identifier=?)

(define-syntax-rule (define&provide-dsl-helper-syntaxes context (identifier ...))
  (begin (provide identifier ...)
	 (define-syntax identifier
	   (lambda (stx)
	     (raise-syntax-error #f
				 (format "Illegal use of ~a outside ~a"
					 'identifier
					 context)
				 stx)))
	 ...))

;; Typed racket wraps literal identifiers during provide. Here we dig
;; through the renamings to see if they're the same thing. Gross!
;; Fragile?
(: literal-identifier=? : Syntax Identifier -> Boolean)
(define (literal-identifier=? actual expected)
  (and (identifier? actual)
       (identifier-binding actual)
       (eq? (syntax-local-value actual)
	    (syntax-local-value expected))))
