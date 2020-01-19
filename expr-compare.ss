;Matthew Dalton
;mattdalton@ucla.edu
;304964539

#lang racket

; symbol lambda
(define LAMBDA (string->symbol "\u03BB"))

; returns true if a and b are lists and are the same length
(define (sameLengthLists? a b)
	(if (or (not (list? a)) (not (list? b)))
		#f
		(= (length a) (length b))))

; checks to see if the inputs are booleans -- if so, give the desired output
(define (boolCheck a b)
	(if (and (equal? a #t) (equal? b #f))
		'%
		(if (and (equal? a #f) (equal? b #t))
			'(not %)
			`(if % ,a ,b))))

; checks if the parameters (arglists) are of the same types
(define (sameTypes? a b)
	(cond
		[(list? a) (list? b)] ; check if both lists
		[(and (not (list? a)) (pair? a)) (and (not (list? b)) (pair? b))] ; check if both improper lists
		[(not (pair? a)) (not (pair? b))]
		[else #f]))

; calculates the length of the input pair
; DOES NOT WORK IF THE LAST ELEMENT OF IMPROPER LIST IS A LIST ITSELF -- FIX THIS
(define (myLength p)
	(let myLength-app ((p p) (len 0))
		(cond
			[(pair? p) (myLength-app (cdr p) (+ 1 len))] ; nonempty list or pair
			[(list? p) len] ; empty list
			[else (+ 1 len)] ; singleton
		)))

; checks to see if the parameters are of the same length
; registers '(a) and 'a as same length, but this is OK because types were checked before this is called
(define (sameLength? a b) (= (myLength a) (myLength b)))

; checks to see if the inputs are the heads of a lambda expression
(define (lambdas? a b)
	(and 
		(or (equal? a 'lambda) (equal? a LAMBDA))
		(or (equal? b 'lambda) (equal? b LAMBDA))))

; determines if the first is a member of the second
(define (pairMember x l)
	(cond [(pair? l) (if (equal? x (car l)) #t (pairMember x (cdr l)))] [else (equal? x l)]))

; determines if the first is a member of the second
(define (element? x l)
	(cond
		[(list? l) (member x l)] ; we have a list
		[(pair? l) (pairMember x l)] ; we have an improper list
		[else (equal? x l)] ; we have a singleton
	)
)

; updates the old bindings in the provided list to the combo
; does not account for a pair
(define (changeBindings l old combo)
	(cond
		[(not (or (list? l) (pair? l)))
			(if (equal? l old)
				combo
				l)]
		[(empty? l) '()] ; we are at the end of the list
		[(equal? (car l) old) (cons combo (changeBindings (cdr l) old combo))] ; we need to switch
		[(and (lambdas? (car l) (car l)) (element? old (cadr l))) l] ; stop if old in a lambda parameter list
		[(or (list? (car l)) (pair? (car l))) (cons (changeBindings (car l) old combo) (changeBindings (cdr l) old combo))]
		[else (cons (car l) (changeBindings (cdr l) old combo))] ; do not switch
	)
)

; returns a pair (updatedA . updatedB)
(define (changeAllList a b aargs bargs)
	(cond
		[(empty? aargs) `(,a . ,b)]
		[(equal? (car aargs) (car bargs))
			(changeAllList a b (cdr aargs) (cdr bargs))]
		[else (let ((new (string->symbol (string-append (symbol->string (car aargs)) "!"
													  	(symbol->string (car bargs))))))
				(changeAllList (changeBindings a (car aargs) new)
							   (changeBindings b (car bargs) new)
							   (cdr aargs) (cdr bargs)))]
	)
)


; returns a pair (updatedA . updatedB)
(define (changeAllPair a b aargs bargs)
	(cond
		[(not (pair? aargs))
			(let ((new (string->symbol (string-append (symbol->string aargs) "!" (symbol->string bargs)))))
				`(,(changeBindings a aargs new) . ,(changeBindings b bargs new)))] ; singleton
		[(equal? (car aargs) (car bargs))
			(changeAllPair a b (cdr aargs) (cdr bargs))] ; in common
		[else (let ((new (string->symbol (string-append (symbol->string (car aargs)) "!"
													  	(symbol->string (car bargs))))))
				(changeAllPair (changeBindings a (car aargs) new)
							   (changeBindings b (car bargs) new)
							   (cdr aargs) (cdr bargs)))] ; different
	)
)

; converts the old bindings in a list to their combo
; a and b represent lambda expression from arg lists on (exclude preceding lambda)
(define (updateBindings a b aargs bargs)
	(cond
		[(list? aargs) (changeAllList a b aargs bargs)] ; update bindings for lists of references
		[(pair? aargs) (changeAllPair a b aargs bargs)] ; update bindings for the improper lists of references
		[else ; we know they must be different so update accordingly
			(let ((new (string->symbol (string-append (symbol->string aargs) "!" (symbol->string bargs)))))
				`(,(changeBindings a aargs new) . ,(changeBindings b bargs new)))]
	)
)

; decides how to handle lambda expressions
(define (handleLambda a b)
	(let ([aargs (cadr a)] [bargs (cadr b)])
		(cond
			[(equal? aargs bargs) ; arg lists are the same -- handle normally
				(expr-compare-app a b)]
			[(not (sameTypes? aargs bargs)) ; arg lists are different types -- difference the whole thing
				`(if % ,a ,b)]
			[(not (sameLength? aargs bargs)) ; arg lists are different lengths -- difference the whole thing
				`(if % ,a ,b)]
			[else ; arg lists same type and length but different names -- change bindings then handle normally
				(let ((newProgs (updateBindings (cdr a) (cdr b) aargs bargs)))
					(expr-compare-app (cons (car a) (car newProgs))
									  (cons (car b) (cdr newProgs))))]
		)
	)
)

; determine if the input is a special form and act accordingly
; DETERMINE A WAY TO RECURSE BACK TO EXPR-COMPARE-APP -- maybe need to pull that out into its own function
(define (specialCheck a b)
	(let ([ahead (car a)] [bhead (car b)] [atail (cdr a)] [btail (cdr b)])
		(cond
			[(and (equal? ahead 'lambda) (equal? bhead 'lambda))
				(handleLambda a b)] ; handle lambdas
			[(lambdas? ahead bhead)
				(handleLambda (cons LAMBDA atail) (cons LAMBDA btail))] ; handle lambdas (replace with symbol)
			[(and (equal? ahead 'if) (equal? bhead 'if))
				(expr-compare-app a b)] ; we have two if statements -- call expr-compare-app recursively
			[(or (equal? ahead 'if) (equal? bhead 'if))
				`(if % ,a ,b)] ; we have one if statement
			[(or (equal? ahead 'quote) (equal? bhead 'quote))
				`(if % ,a ,b)] ; we have at least one quoted expression
			[else
				(expr-compare-app a b)] ; we have the normal case -- call expr-compare-app recursively
		)
	)
)

; inside function for expr-compare
(define (expr-compare x y)
	(let ([xx (list x)] [yy (list y)])
		(car (expr-compare-app xx yy))
	)
)


; outputs a diff between the two input programs x and y
(define (expr-compare-app xx yy)
	(if (and (empty? xx) (empty? yy))
		'() ; base case
		(let ([xhead (car xx)] [yhead (car yy)] [xtail (cdr xx)] [ytail (cdr yy)]) ; split up lists
			(cond
				[(equal? xhead yhead) (cons xhead (expr-compare-app xtail ytail))] ; term in common
				[(not (sameLengthLists? xhead yhead)) ; checks if they're the same length
					(cons (boolCheck xhead yhead) (expr-compare-app xtail ytail))] ; not same or not lists
				[else (cons (specialCheck xhead yhead) (expr-compare-app xtail ytail))] ; recurse on heads
				; LINE ABOVE THIS: replace the first term in cons with the specialCheck function
			)
		)
	)
)

(define my-eval
  (let ((ns (make-base-namespace)))
    (lambda (expr) (eval expr ns))))

(define (test-expr-compare x y)
	(let ((result (expr-compare x y)))
		(and (equal? (my-eval x) (my-eval `(let ((% #t)) ,result)))
			 (equal? (my-eval y) (my-eval `(let ((% #f)) ,result)))
		)
	)
)


(define test-expr-x `((lambda (a b) (if (> b a)
										(lambda () (- b a))
										((,LAMBDA a (+ (g (> a 1) b a) (if (pair? cddr a)
																	(cadr a)
																	(lambda () (if (> 2 1) #f '(lambda () (+ 1 2))))))) (cons 1 (cons 2 '())))))
						1 #t))
(define test-expr-y `((,LAMBDA (a b) (if (< b a)
										((lambda (a) (- b a) 2))
										((,LAMBDA a (+ (if (> a 1) b a) (if (pair? cdr a)
																	(cadr a)
																	(,LAMBDA () (if (> 2 1) #t '(,LAMBDA () (+ 1 2))))))) (cons 2 (cons 1 '())))))
						1 #f))
