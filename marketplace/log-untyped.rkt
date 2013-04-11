#lang racket/base

(require racket/match)

(provide matrix-root-logger
	 matrix-log)

(define matrix-root-logger (make-logger 'typed-matrix #f))

;; WARNING: duplicated in log-typed.rkt
(define-syntax matrix-log
  (syntax-rules ()
    [(_ level-exp message)
     (let ((level level-exp))
       (when (log-level? matrix-root-logger level)
	 (log-message matrix-root-logger level message #f)))]
    [(_ level format-string exp ...)
     (matrix-log level (format format-string exp ...))]))

(define (level-code level)
  (match level
    ['debug "D"]
    ['info "I"]
    ['warning "W"]
    ['error "E"]
    ['fatal "F"]
    [other (symbol->string other)]))

(match (getenv "MATRIX_LOG")
  [#f (void)]
  [str (let ((level (string->symbol str)))
	 (define receiver (make-log-receiver matrix-root-logger level))
	 (thread
	  (lambda ()
	    (let loop ()
	      (match (sync receiver)
		[(vector level message data event-name)
		 (fprintf (current-error-port) "~a/~a\n" (level-code level) message)])
	      (loop)))))])
