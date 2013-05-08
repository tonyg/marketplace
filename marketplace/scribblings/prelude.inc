;; -*- scheme -*-
(require scribble/racket
	 scribble/eval
	 scriblib/footnote
	 (for-syntax racket)
	 (for-label typed/racket/base))

;; TODO: make it display "=" instead of ":" connecting the defined
;; type to the definition.
(define-syntax deftype
  (lambda (stx)
    (syntax-case stx ()
      [(_ (t a ...) d desc ...)
       #`(defthing #:kind "type" t
;; This is in column zero because scribble is overscrupulous about location preservation.
#,(quasisyntax/loc #'d (All (a ...) d))
	   desc ...)]
      [(_ t d desc ...)
       #`(defthing #:kind "type" t d desc ...)])))
