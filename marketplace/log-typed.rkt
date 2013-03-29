#lang typed/racket/base

(require/typed "log-untyped.rkt"
	       [matrix-root-logger Logger])

;; WARNING: duplicated in log-untyped.rkt
(define-syntax matrix-log
  (syntax-rules ()
    [(_ level-exp message)
     (let ((level level-exp))
       (when (log-level? matrix-root-logger level)
	 (log-message matrix-root-logger level message #f)))]
    [(_ level format-string exp ...)
     (matrix-log level (format format-string exp ...))]))

(provide matrix-root-logger
	 matrix-log)
