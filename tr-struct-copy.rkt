#lang racket/base
;; Revolting hacked-on struct-copy using unhygienic-identifier=?
;; instead of free-identifier=? to compare accessor names, to get
;; around the contracting of accessors exported from TR modules.
;;
;; Workaround for PR13149.

(require (for-syntax racket/base racket/private/struct-info))

(provide tr-struct-copy)

(define-for-syntax (unhygienic-identifier=? a b)
  (eq? (syntax->datum a)
       (syntax->datum b)))

(define-syntax (tr-struct-copy stx)
  (if (not (eq? (syntax-local-context) 'expression))
      (quasisyntax/loc stx (#%expression #,stx))
      (syntax-case stx ()
	[(form-name info struct-expr field+val ...)
	 (let ([ans (syntax->list  #'(field+val ...))])
	   ;; Check syntax:
	   (unless (identifier? #'info)
	     (raise-syntax-error #f "not an identifier for structure type" stx #'info))
	   (for-each (lambda (an)
		       (syntax-case an ()
			 [(field val)
			  (unless (identifier? #'field)
			    (raise-syntax-error #f 
						"not an identifier for field name" 
						stx
						#'field))]
			 [(field #:parent p val)
			  (unless (identifier? #'field)
			    (raise-syntax-error #f 
						"not an identifier for field name" 
						stx
						#'field))
			  (unless (identifier? #'p)
			    (raise-syntax-error #f 
						"not an identifier for parent struct name" 
						stx
						#'field))]
			 [_
			  (raise-syntax-error #f
					      (string-append
					       "bad syntax;\n"
					       " expected a field update of the form (<field-id> <expr>)\n"
					       " or (<field-id> #:parent <parent-id> <expr>)")
					      stx
					      an)]))
		     ans)
	   (let-values ([(construct pred accessors parent)
			 (let ([v (syntax-local-value #'info (lambda () #f))])
			   (unless (struct-info? v)
			     (raise-syntax-error #f "identifier is not bound to a structure type" stx #'info))
			   (let ([v (extract-struct-info v)])
			     (values (cadr v)
				     (caddr v)
				     (cadddr v)
				     (list-ref v 5))))])
               
	     (let* ([ensure-really-parent
		     (λ (id)
			(let loop ([parent parent])
			  (cond
			   [(eq? parent #t)
			    (raise-syntax-error #f "identifier not bound to a parent struct" stx id)]
			   [(not parent)
			    (raise-syntax-error #f "parent struct information not known" stx id)]
			   [(free-identifier=? id parent) (void)]
			   [else
			    (let ([v (syntax-local-value parent (lambda () #f))])
			      (unless (struct-info? v)
				(raise-syntax-error #f "unknown parent struct" stx id)) ;; probably won't happen(?)
			      (let ([v (extract-struct-info v)])
				(loop (list-ref v 5))))])))]
		    [new-fields
		     (map (lambda (an)
			    (syntax-case an ()
			      [(field expr)
			       (list (datum->syntax #'field
						    (string->symbol
						     (format "~a-~a"
							     (syntax-e #'info)
							     (syntax-e #'field)))
						    #'field)
				     #'expr
				     (car (generate-temporaries (list #'field))))]
			      [(field #:parent id expr)
			       (begin
				 (ensure-really-parent #'id)
				 (list (datum->syntax #'field
						      (string->symbol
						       (format "~a-~a"
							       (syntax-e #'id)
							       (syntax-e #'field)))
						      #'field)
				       #'expr
				       (car (generate-temporaries (list #'field)))))]))
			  ans)]
                      
		    ;; new-binding-for : syntax[field-name] -> (union syntax[expression] #f)
		    [new-binding-for 
		     (lambda (f)
		       (ormap (lambda (new-field)
				(and (unhygienic-identifier=? (car new-field) f)
				     (caddr new-field)))
			      new-fields))])
                 
	       (unless construct
		 (raise-syntax-error #f
				     "constructor not statically known for structure type"
				     stx
				     #'info))
	       (unless pred
		 (raise-syntax-error #f
				     "predicate not statically known for structure type"
				     stx
				     #'info))
	       (unless (andmap values accessors)
		 (raise-syntax-error #f
				     "not all accessors are statically known for structure type"
				     stx
				     #'info))
                 
                 
	       (let ([dests
		      (map (lambda (new-field)
			     (or (ormap (lambda (f2)
					  (and f2
					       (unhygienic-identifier=? (car new-field) f2)
					       f2))
					accessors)
				 (raise-syntax-error #f 
						     "accessor name not associated with the given structure type" 
						     stx
						     (car new-field))))
			   new-fields)])
		 ;; Check for duplicates using dests, not as, because mod=? as might not be id=?
		 (let ((dupe (check-duplicate-identifier dests)))
		   (when dupe 
		     (raise-syntax-error #f 
					 "duplicate field assignment" 
					 stx 
					 ;; Map back to an original field:
					 (ormap (lambda (nf)
						  (and nf
						       (unhygienic-identifier=? dupe (car nf))
						       (car nf)))
						(reverse new-fields)))))
                   
		 ;; the actual result
		 #`(let ((the-struct struct-expr))
		     (if (#,pred the-struct)
			 (let #,(map (lambda (new-field)
				       #`[#,(caddr new-field) #,(cadr new-field)])
				     new-fields)
			   (#,construct
			    #,@(map 
				(lambda (field) (or (new-binding-for field) 
						    #`(#,field the-struct)))
				(reverse accessors))))
			 (raise-argument-error 'form-name 
					       #,(format "~a?" (syntax-e #'info))
					       the-struct)))))))])))
