#lang racket/base

(require racket/set)
(require racket/match)
(require racket/class)
(require racket/async-channel)
(require racket/gui/base)
(require racket/date)
(require racket/format)
(require racket/math)

(require images/icons/control)
(require images/icons/arrow)
(require images/icons/symbol)
(require images/icons/misc)
(require images/icons/style)

(require racket/pretty)

(require (prefix-in core: "../types.rkt"))

(provide any-state?
	 open-debugger)

;; Frame
;;   Toolbar
;;     Rewind one step - moves without executing
;;     Fast-forward one step - moves without executing
;;     --
;;     Stop
;;     Run one step
;;     Play
;;     --
;;     Select history depth
;;     --
;;     Kill this process
;;   State display
;;     If the state is (vm?), special display; permits spawning debuggers on nested processes
;;   Endpoints display
;;     Shows ID and role 
;;     Permits deletion of endpoint
;;     Permits interaction with endpoint??
;;   Trace display
;;     Selection of a row rewinds to that point

(define (any-state? x) #t)

(define (open-debugger name)
  (define to-debugger (make-channel))
  (define from-debugger (make-semaphore 0))
  (parameterize ((current-eventspace (make-eventspace)))
    (new debugger%
	 [name name]
	 [from-vm to-debugger]
	 [to-vm from-debugger]))
  (values (lambda (t) (channel-put to-debugger t))
	  (lambda () (semaphore-wait from-debugger))))

(define debugger%
  (class object%

    (init-field name)
    (init-field from-vm)
    (init-field to-vm)

    (define mutex (make-semaphore 1))
    (define stepping? #t)
    (define waiting? #f)

    (define frame (new frame%
		       [label (format "~a" name)]
		       [width 480]
		       [height 700]))

    (define menu-bar (new menu-bar% [parent frame]))
    (define edit-menu (new menu% [label "Edit"] [parent menu-bar]))
    (append-editor-operation-menu-items edit-menu #f)

    (define toolbar (new horizontal-pane%
			 [parent frame]
			 [stretchable-height #f]
			 [alignment '(right center)]))

    (define state-panel (new horizontal-panel%
			     [parent frame]))

    (define endpoints (new list-box%
			   [style '(single column-headers)]
			   [label #f]
			   [choices '()]
			   [parent frame]
			   [columns '("ID" "Orientation" "Topic" "Type")]))

    (define events (new list-box%
			[style '(single column-headers)]
			[label #f]
			[choices '()]
			[parent frame]
			[columns '("Time" "Dir" "Type" "Detail")]))

    (send endpoints set-column-width 1 32 32 32)
    (send endpoints set-column-width 3 32 32 32)

    (send events set-column-width 1 32 32 32)

    (define (toolbar-button label [handler void])
      (new button%
	   [label label]
	   [min-width 32]
	   [parent toolbar]
	   [callback handler]))

    (define (toolbar-spacer)
      (new pane% [parent toolbar] [min-width 16] [stretchable-width #f]))

    ;; (toolbar-button (left-over-arrow-icon #:color syntax-icon-color))
    ;; (toolbar-button (right-over-arrow-icon #:color syntax-icon-color))
    ;; (toolbar-spacer)

    (define pause-button
      (toolbar-button (pause-icon #:color halt-icon-color)
		      (lambda (b e)
			(send pause-button enable #f)
			(send play-button enable #t)
			(send step-button enable #t)
			(call-with-semaphore
			 mutex
			 (lambda ()
			   (set! stepping? #f))))))
    (define play-button
      (toolbar-button (play-icon #:color run-icon-color)
		      (lambda (b e)
			(send pause-button enable #t)
			(send play-button enable #f)
			(send step-button enable #f)
			(call-with-semaphore
			 mutex
			 (lambda ()
			   (set! stepping? #t)
			   (when waiting?
			     (set! waiting? #f)
			     (semaphore-post to-vm)))))))
    (toolbar-spacer)
    (define step-button
      (toolbar-button (step-icon #:color run-icon-color)
		      (lambda (b e)
			(call-with-semaphore
			 mutex
			 (lambda ()
			   (when waiting?
			     (set! waiting? #f)
			     (semaphore-post to-vm)))))))

    (send play-button enable #f)
    (send step-button enable #f)

    ;; (toolbar-spacer)
    ;; (toolbar-button "Settings...")
    ;; (toolbar-spacer)
    ;; (toolbar-button (stop-sign-icon))

    (define state-text (new text%))
    (define state-canvas (new editor-canvas%
			      [parent state-panel]
			      [editor state-text]
			      [label "State"]))

    (define controller-thread
      (thread
       (lambda ()
	 (controller-thread-loop))))

    (define (current-timestamp)
      (define (p n w) (~a (exact-truncate n) #:width w #:align 'right #:pad-string "0"))
      (define d (current-date))
      (format "~a:~a:~a.~a ~a-~a-~a"
	      (p (date-hour d) 2)
	      (p (date-minute d) 2)
	      (p (date-second d) 2)
	      (p (/ (date*-nanosecond d) 1000000.0) 3)
	      (date-year d)
	      (p (date-month d) 2)
	      (p (date-day d) 2)))

    (define (record-event! stamp dir type detail)
      (define n (send events get-number))
      (send events append stamp)
      (send events set-string n dir 1)
      (send events set-string n type 2)
      (send events set-string n (~a detail) 3)
      (send events set-first-visible-item
	    (max 0 (- n (- (send events number-of-visible-items) 1)))))

    (define (format-action action)
      (cond
       [(core:yield? action)
	(values "Yield" "")]
       [(core:at-meta-level? action)
	(format-preaction "Meta" (core:at-meta-level-preaction action))]
       [else
	(format-preaction "" action)]))

    (define (format-preaction layer preaction)
      (define-values (type detail)
	(match preaction
	  [(core:add-endpoint pre-eid role handler)
	   (values "Sub" (string-append (format-role role) " " (~a pre-eid)))]
	  [(core:delete-endpoint pre-eid reason)
	   (values "Unsub" (format "~a ~v" pre-eid reason))]
	  [(core:send-message body 'publisher)
	   (values "Send" (~v body))]
	  [(core:send-message body 'subscriber)
	   (values "Feedback" (~v body))]
	  [(core:spawn spec maybe-k child-debug-name)
	   (values "Spawn" (~v child-debug-name))]
	  [(core:quit #f reason)
	   (values "Exit" (~v reason))]
	  [(core:quit pid reason)
	   (values "Kill" (format "~a ~v" pid reason))]))
      (values (string-append layer type) detail))

    (define (format-role r)
      (match-define (core:role orientation topic interest-type) r)
      (format "~a/~a ~v"
	      (string-ref (symbol->string orientation) 0)
	      (string-ref (symbol->string interest-type) 0)
	      topic))

    (define (handle-from-vm x)
      (define now (current-timestamp))
      (match x
	[(core:transition state actions)
	 (send state-text erase)
	 (send state-text insert (pretty-format state))
	 (let loop ((a actions))
	   (cond
	    [(pair? a) (loop (car a)) (loop (cdr a))]
	    [(or (null? a) (eq? a #f) (void? a)) (void)]
	    [else (define-values (type detail) (format-action a))
		  (record-event! now "Act" type detail)]))
	 (call-with-semaphore
	  mutex
	  (lambda ()
	    (if stepping?
		(semaphore-post to-vm)
		(set! waiting? #t))))]
	[(cons meta? e)
	 (define prefix (if meta? "Meta" ""))
	 (match e
	   [(core:presence-event role)
	    (record-event! now "Evt" (string-append prefix "Presence") (format-role role))]
	   [(core:absence-event role reason)
	    (record-event! now "Evt" (string-append prefix "Absence")
			   (string-append (format-role role) " " (~v reason)))]
	   [(core:message-event _ message)
	    (record-event! now "Evt" (string-append prefix "Recv") (~v message))])]))

    (define (controller-thread-loop)
      (sync (handle-evt from-vm
			(lambda (x)
			  (handle-from-vm x)
			  (controller-thread-loop)))))

    (super-new)

    (send frame show #t)
    ))
