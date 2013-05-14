#lang marketplace

(require racket/port)

;; Usually it's OK to just use display and friends directly.
;; Here we have a console output driver just to show how it's done.
(spawn #:debug-name 'console-output-driver
       #:child
       (transition/no-state
	(endpoint #:subscriber (list 'console-output ?)
		  [(list 'console-output item)
		   (begin (printf "~a" item)
			  (void))])))

(spawn #:debug-name 'console-input-driver
       #:child
       (transition/no-state
	(endpoint #:publisher (list 'console-input ?)
		  #:name 'input-relay
		  #:on-absence
		  (list (send-message (list 'console-output "Connection terminated.\n"))
			(quit)))
	(endpoint #:subscriber (cons (read-line-evt (current-input-port) 'any) ?)
		  [(cons _ (? eof-object?))
		   (list (send-message (list 'console-output "Terminating on local EOF.\n"))
			 (delete-endpoint 'input-relay))]
		  [(cons _ (? string? line))
		   (send-message (list 'console-input line))])))

(spawn #:debug-name 'outbound-connection
       #:child
       (let ((local (tcp-handle 'outbound))
	     (remote (tcp-address "localhost" 5999)))
	 (transition/no-state
	  (endpoint #:subscriber (list 'console-input ?)
		    #:on-absence (quit)
		    [(list 'console-input line)
		     (list (send-message (list 'console-output (format "> ~a \n" line)))
			   (send-message (tcp-channel local remote (string-append line "\n"))))])
	  (endpoint #:publisher (tcp-channel local remote ?))
	  (endpoint #:subscriber (tcp-channel remote local ?)
		    #:on-absence (quit)
		    [(tcp-channel _ _ (? eof-object?))
		     (quit)]
		    [(tcp-channel _ _ data)
		     (list (send-message (list 'console-output (format "< ~a" data)))
			   (void))]))))
