;; -*- scheme -*-
(require scribble/racket
	 scribble/eval
	 scriblib/footnote

	 slideshow/pict
	 "vm-pictures.rkt"

	 "from.rkt"

	 (for-syntax racket)
	 (for-label typed/racket/base))

(require (for-label (only-in marketplace/drivers/tcp-bare tcp)
		    marketplace/sugar))

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

(define (vm-figure p)
  (centered (to-element (render (scale p 1.2) #:target 'pict))))
