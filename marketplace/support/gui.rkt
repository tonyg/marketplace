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

(struct historical-moment (alive? state endpoints) #:transparent)

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

    (define current-historical-moment (historical-moment #t (void) '()))
    (define displayed-endpoints '())
    (define booted? #f)

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
			   [columns '("ID" "" "" "" "Topic")]))

    (define events (new list-box%
			[style '(single column-headers)]
			[callback (lambda (lb e)
				    (define sel (or (send lb get-selection)
						    (- (send lb get-number) 1)))
				    (define m (and sel (send lb get-data sel)))
				    (when m (select-historical-moment m)))]
			[label #f]
			[choices '()]
			[parent frame]
			[columns '("Time" "Dir" "Type" "Detail")]))

    (define FIXED-COLUMN-WIDTH 40)
    (send endpoints set-column-width 1 FIXED-COLUMN-WIDTH FIXED-COLUMN-WIDTH FIXED-COLUMN-WIDTH)
    (send endpoints set-column-width 2 FIXED-COLUMN-WIDTH FIXED-COLUMN-WIDTH FIXED-COLUMN-WIDTH)
    (send endpoints set-column-width 3 FIXED-COLUMN-WIDTH FIXED-COLUMN-WIDTH FIXED-COLUMN-WIDTH)

    (send events set-column-width 1 FIXED-COLUMN-WIDTH FIXED-COLUMN-WIDTH FIXED-COLUMN-WIDTH)

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

    (define status-indicator
      (new canvas%
	   [parent toolbar]
	   [min-width 32]
	   [min-height 32]
	   [stretchable-width #f]
	   [style '(transparent no-focus)]
	   [paint-callback (lambda (c dc)
			     (define mx (/ (send c get-width) 2))
			     (define my (/ (send c get-height) 2))
			     (define r (/ (min mx my) 2))
			     (send dc set-brush
				   (if (historical-moment-alive? current-historical-moment)
				       "green"
				       "red")
				   'solid)
			     (send dc draw-ellipse (- mx r) (- my r) (* r 2) (* r 2)))]))

    (define state-text (new text%))
    (define state-canvas (new editor-canvas%
			      [parent state-panel]
			      [editor state-text]
			      [label "State"]))

    (define (select-historical-moment m)
      (match-define (historical-moment alive? state new-endpoints) m)

      (when (not (equal? displayed-endpoints new-endpoints))
	(send endpoints clear)
	(for [(ep (in-list new-endpoints))]
	  (match-define (list pre-eid meta? (core:role orientation topic interest-type)) ep)
	  (define n (send endpoints get-number))
	  (send endpoints append (~v pre-eid))
	  (send endpoints set-string n (if meta? "Meta" "") 1)
	  (send endpoints set-string n (case orientation
					 [(publisher) "Pub"]
					 [(subscriber) "Sub"]) 2)
	  (send endpoints set-string n (case interest-type
					 [(participant) ""]
					 [(observer) "Obs"]
					 [(everything) "***"]) 3)
	  (send endpoints set-string n (~v topic) 4))
	(set! displayed-endpoints new-endpoints))

      (send state-canvas set-canvas-background
	    (if alive?
		(make-color #xff #xff #xff)
		(make-color #xff #xd0 #xd0)))

      (send status-indicator refresh)

      (send state-text erase)
      (send state-text insert (pretty-format state)))

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
      (send events set-data n current-historical-moment)
      (send events set-string n dir 1)
      (send events set-string n type 2)
      (send events set-string n (~a detail) 3)
      (define current-selection (send events get-selection))
      (when (or (not current-selection) (= current-selection (- n 1)))
	(send events set-first-visible-item n)
	(send events set-selection n)
	(select-historical-moment current-historical-moment)))

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

    (define (apply-action! a)
      (cond
       [(core:yield? a) (void)]
       [(core:at-meta-level? a) (apply-preaction! #t (core:at-meta-level-preaction a))]
       [else (apply-preaction! #f a)]))

    (define (apply-preaction! meta? p)
      (match p
	[(core:quit #f reason)
	 (set! current-historical-moment
	       (struct-copy historical-moment current-historical-moment
		 [alive? #f]))]
	[(core:add-endpoint pre-eid role handler)
	 (set! current-historical-moment
	       (struct-copy historical-moment current-historical-moment
		 [endpoints (append (filter (lambda (e) (not (equal? (car e) pre-eid)))
					    (historical-moment-endpoints
					     current-historical-moment))
				    (list (list pre-eid meta? role)))]))]
	[(core:delete-endpoint pre-eid reason)
	 (set! current-historical-moment
	       (struct-copy historical-moment current-historical-moment
		 [endpoints (filter (lambda (e) (not (equal? (car e) pre-eid)))
				    (historical-moment-endpoints
				     current-historical-moment))]))]
	[_ (void)]))

    (define (handle-from-vm x)
      (define now (current-timestamp))
      (match x
	[(core:transition state actions)
	 (when (or (not booted?)
		   (not (equal? state (historical-moment-state current-historical-moment))))
	   (set! booted? #t)
	   (set! current-historical-moment
		 (struct-copy historical-moment current-historical-moment [state state]))
	   (record-event! now "Txn" "State" (~v state)))
	 (let loop ((a actions))
	   (cond
	    [(pair? a) (loop (car a)) (loop (cdr a))]
	    [(or (null? a) (eq? a #f) (void? a)) (void)]
	    [else (define-values (type detail) (format-action a))
		  (apply-action! a)
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
			  (queue-callback (lambda () (handle-from-vm x)))
			  (controller-thread-loop)))))

    (super-new)

    (select-historical-moment current-historical-moment)

    (send frame show #t)
    ))
