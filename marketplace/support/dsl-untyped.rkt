#lang racket/base

(require (for-syntax racket/base))
(provide define&provide-dsl-helper-syntaxes)

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
