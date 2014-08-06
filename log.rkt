#lang racket/base

(require racket/match)

(provide marketplace-root-logger
	 marketplace-log)

(define marketplace-root-logger (make-logger 'marketplace #f))

(define-syntax marketplace-log
  (syntax-rules ()
    [(_ level-exp message)
     (let ((level level-exp))
       (when (log-level? marketplace-root-logger level)
	 (log-message marketplace-root-logger level message #f)))]
    [(_ level format-string exp ...)
     (marketplace-log level (format format-string exp ...))]))

(define (level-code level)
  (match level
    ['debug "D"]
    ['info "I"]
    ['warning "W"]
    ['error "E"]
    ['fatal "F"]
    [other (symbol->string other)]))

(match (getenv "MARKETPLACE_LOG")
  [#f (void)]
  [str (let ((level (string->symbol str)))
	 (define receiver (make-log-receiver marketplace-root-logger level))
	 (thread
	  (lambda ()
	    (let loop ()
	      (match (sync receiver)
		[(vector level message data event-name)
		 (fprintf (current-error-port) "~a/~a\n" (level-code level) message)])
	      (loop)))))])
