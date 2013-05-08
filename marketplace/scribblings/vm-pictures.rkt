#lang racket/base

(require (only-in racket/math pi))
(require slideshow/pict)
(require file/convertible)

(provide (all-defined-out))

(define default-process-height 98)

(define default-font (make-parameter 'roman))

(define final-border (make-parameter 1))

(define process-angle (make-parameter 90))
(define process-width (make-parameter 24))
(define process-corner (make-parameter 6))
(define process-height (make-parameter default-process-height))
(define process-gap (make-parameter 6))
(define process-fill-color (make-parameter "white"))

(define PROCESS-SHADED (list #xdd #xdd #xdd))

(define leg-offset (make-parameter 4))
(define leg-width (make-parameter 2))
(define leg-spot-width (make-parameter 4))

(define (default-leg-height)
  (/ (network-height) 2))

(define (meta-leg-height)
  (+ (default-leg-height) (vm-height) (network-height)))

(define network-height (make-parameter 12))
(define vm-height (make-parameter 27))
(define process-label-size (make-parameter 12))
(define network-label-size (make-parameter 9))
(define vm-label-size (make-parameter 12))
(define ellipsis-size (make-parameter 24))

(define (process label)
  (cb-superimpose
   (colorize (filled-rounded-rectangle (process-width) (process-height) (process-corner))
	     (process-fill-color))
   (rounded-rectangle (process-width) (process-height) (process-corner))
   (vc-append (text label (default-font) (process-label-size) (d2r (process-angle)))
	      (blank (/ (- (process-width) (process-label-size)) 2)))))

(define (process-group . ps)
  (apply hb-append (process-gap) ps))

(define (process-space [n 1])
  (blank (+ (* (- n 1) (/ (process-width) 2))
	    (- (/ (process-width) 2) (* 2 (process-gap)))) (process-height)))

(define (vm-label str)
  (text str (default-font) (vm-label-size) (d2r 0)))

(define (network-label str)
  (text str (default-font) (network-label-size) (d2r 0)))

(define (vm label net-label . ps)
  (define ps-pict (apply process-group ps))
  (define label-width (max (pict-width label) (pict-width net-label)))
  (define width (max (+ label-width (* 2 (max (vm-label-size) (network-label-size))))
		     (pict-width ps-pict)))
  (vl-append ps-pict
	     (cb-superimpose (colorize (filled-rectangle width (network-height))
				       (process-fill-color))
			     (rectangle width (network-height))
			     net-label)
	     (cc-superimpose (colorize (filled-rectangle width (vm-height))
				       (process-fill-color))
			     (rectangle width (vm-height))
			     label)))

(define (process-ellipsis)
  (cc-superimpose (text ". . ." (default-font) (ellipsis-size) 0)
		  (blank 0 (process-height))))

(define (d2r d)
  (* pi (/ d 180.0)))

(define (leg p offset height)
  (define x (+ (/ (pict-width p) 2) (* offset (leg-offset))))
  (define y (pict-height p))
  (define leg-pict (vc-append (vline (leg-width) (- height (/ (leg-spot-width) 2)))
			      (disk (leg-spot-width) #:draw-border? #f)))
  (pin-over p (- x (/ (pict-width leg-pict) 2)) y leg-pict))

(define (relay-legs p)
  (leg (leg p 1 (default-leg-height)) -1 (meta-leg-height)))

(define (local-leg p)
  (leg p 0 (default-leg-height)))

(define (render p #:target [target (string->symbol (or (getenv "VM_PICTURES_TARGET") "eps"))])
  (define final-pict (cc-superimpose (blank (+ (pict-width p) (* 2 (final-border)))
					    (+ (pict-height p) (* 2 (final-border))))
				     (panorama p)))
  (case target
    [(pict)
     final-pict]
    [(screen)
     ;; FFS. This connects to the display even if you don't use it.
     ;; (local-require racket/gui/base)
     ;; (show-pict final-pict 800 600)
     (log-error "You need to uncomment a couple of lines in vm-pictures.rkt")
     (void)]
    [(png)
     (display (convert final-pict 'png-bytes))]
    [(eps)
     (display (convert final-pict 'eps-bytes))]))
