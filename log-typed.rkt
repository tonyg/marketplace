#lang typed/racket/base

(require/typed "log-untyped.rkt"
	       [marketplace-root-logger Logger])

;; WARNING: duplicated in log-untyped.rkt
(define-syntax marketplace-log
  (syntax-rules ()
    [(_ level-exp message)
     (let ((level level-exp))
       (when (log-level? marketplace-root-logger level)
	 (log-message marketplace-root-logger level message #f)))]
    [(_ level format-string exp ...)
     (marketplace-log level (format format-string exp ...))]))

(provide marketplace-root-logger
	 marketplace-log)
