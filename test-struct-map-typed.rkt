#lang typed/racket/base

(require typed/rackunit)

(require/typed "struct-map.rkt"
	       [struct-map ((Any -> Any) Any -> Any)])
(require/typed "test-struct-map.rkt"
	       [#:struct foo ([bar : Integer]
			      [zot : Integer])])

(check-equal? (struct-map (lambda (x) (if (equal? x 123) 999 888)) (foo 123 234))
	      (foo 999 234))
