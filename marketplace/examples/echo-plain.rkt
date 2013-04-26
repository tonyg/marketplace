#lang racket/base
;; Plain Racket version, using (require) instead of #lang marketplace.

(require marketplace/sugar-untyped)
(require marketplace/drivers/tcp-bare)

(define (echoer from to)
  (transition/no-state
    (endpoint
      #:subscriber (tcp-channel from to ?)
      #:on-absence (quit)
      [(tcp-channel _ _ data)
       (send-message (tcp-channel to from data))])))

(ground-vm tcp
	   (endpoint #:subscriber (tcp-channel ? (tcp-listener 5999) ?)
		     #:conversation (tcp-channel from to _)
		     #:on-presence (spawn #:child (echoer from to))))
