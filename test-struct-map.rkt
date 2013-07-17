#lang racket/base

(require "struct-map.rkt")
(require rackunit)
(provide (struct-out foo))

(struct foo (bar zot)
	#:transparent
	#:property prop:struct-map (lambda (f seed x)
				     (define-values (bar* seed*) (f (foo-bar x) seed))
				     (values (foo bar* (foo-zot x)) seed)))

(check-equal? (struct-map (lambda (x) (list '! x)) (foo 123 234))
	      (foo (list '! 123) 234))
