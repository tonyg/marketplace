;; -*- scheme -*-
(require scribble/racket
	 scriblib/footnote
	 (for-syntax racket)
	 (for-label typed/racket/base))

;; TODO: make it display "=" instead of ":" connecting the defined
;; type to the definition.
(define-syntax deftype
  (lambda (stx)
    (syntax-case stx ()
      [(_ (t a ...) d desc ...)
       #`(defthing #:kind "type" t #,(syntax/loc #'d (All (a ...) d)) desc ...)]
      [(_ t d desc ...)
       #`(defthing #:kind "type" t d desc ...)])))
