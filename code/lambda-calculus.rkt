#lang racket
(require pict
         pict/tree-layout)

(provide draw-expr
         save-pict
         check-expr?
         substitute
         reduce
         format-expr
         display-expr
         eval)
         
;;; Utilities displaying λ-expressions
(define (my-node str color)
  (cc-superimpose
   (disk 40 #:color color)
   (text str null 20)))

(define (draw atom->pict a)
  (cond ((null? a) #f)                                 
        ((list? a) (match a
                     [(list 'λ var ': body) (tree-layout #:pict
                                                       (atom->pict (string-append "λ"
                                                                                  (symbol->string var)) "white")
                                                       (draw atom->pict body))]
                     [(list (list 'λ var ': body) arg) (tree-layout #:pict
                                                                    (atom->pict "@" "red")
                                                                    (draw atom->pict `(λ ,var : ,body))
                                                                    (draw atom->pict arg))]
                     [(list fn arg) (tree-layout #:pict
                                                 (atom->pict "@" "white")
                                                 (draw atom->pict fn)
                                                 (draw atom->pict arg))]
                     [_ #f]))
        (else (tree-layout #:pict (atom->pict (symbol->string a) "white")))))   

; draw a λ-expression as a tree
(define (draw-expr expr)
  (naive-layered (draw my-node expr)))

(define (save-pict the-pict name kind)
  (define bm (pict->bitmap the-pict))
  (send bm save-file name kind))


;;; Interpreter of λ-calculus with the normal order evaluation strategy
; expr -> var | (λ <var> . <expr>) | (<expr> <expr>)
(define (make-lambda var body) `(λ ,var : ,body))
(define get-var cadr)
(define get-body cadddr)
(define get-fn car)
(define get-arg cadr)

(define var? symbol?)
(define (lambda? expr)
  (and (pair? expr) (eqv? (car expr) 'λ)))
(define application? pair?)

; symbols for renaming variables
(define alphabet (map integer->char
                     (range (char->integer #\a) (add1 (char->integer #\z)))))

; infinite stream of symbols for renaming variables
(define symbols
  (for*/stream ([n (in-naturals)]
                [ch alphabet])
    (string->symbol (format "~a~a" ch n))))

(define fresh-symbol
  (let ([n -1])
    (lambda args
      (if (null? args)
          (begin
            (set! n (add1 n))
            (stream-ref symbols n))
          (set! n (car args)))
      )
    )
  )

; check if the expression is syntactivally correct
(define (check-expr? expr)
  (cond
    ([var? expr] #t)
    ([lambda? expr] (if (and (= (length expr) 4)
                             (eqv? ': (caddr expr)))
                        (check-expr? (get-body expr))
                        (error "Wrong lambda abstraction: " expr)))
    ([application? expr] (if (= (length expr) 2)
                             (and (check-expr? (get-fn expr))
                                  (check-expr? (get-arg expr)))
                             (error "Wrong application: " expr)))
    (else (error "Unknown expression"))))

; get the set of bounded variables
(define (get-bounded-vars expr)
  (match expr
    [(list 'λ var ': body) (set-add (get-bounded-vars body) var)]
    [(list fn arg) (set-union (get-bounded-vars fn) (get-bounded-vars arg))]
    [var (set)]
    ))

; get the set of free variables
(define (get-free-vars expr)
  (match expr
    [(list 'λ var ': body) (set-remove (get-free-vars body) var)]
    [(list fn arg) (set-union (get-free-vars fn) (get-free-vars arg))]
    [var (set var)]
    ))

; checks if var is among the free variables from val 
; if yes, it renames var in expr by a fresh symbol
(define (check-var expr var val)
  (let ([free (get-free-vars val)])
    (if (set-member? free (get-var expr))
        (let ([new-var (fresh-symbol)])
          (make-lambda new-var
                       (substitute
                        (substitute (get-body expr) (get-var expr) new-var)
                        var
                        val))) 
        (make-lambda (get-var expr) (substitute (get-body expr) var val))
        )
    ))
        
  
; substitute val for var in expr
(define (substitute expr var val)
  ;(printf "expr: ~a, var: ~a, val: ~a~n" expr var val)
  (cond
    ([var? expr] (if (eqv? expr var)
                     val
                     expr))
    ([lambda? expr] (if (eqv? var (get-var expr)) ; if var is not free in expr
                        expr                      ; then no substitution in body
                        (check-var expr var val)))
                        ;(let ([new-var (fresh-symbol)])
                        ;  (make-lambda (get-var expr) ;new-var
                        ;               (substitute
                        ;                (get-body expr) ;(substitute (get-body expr) (get-var expr) new-var)
                        ;                var
                        ;                val)))))
    ([application? expr] (list (substitute (get-fn expr) var val)
                               (substitute (get-arg expr) var val)))
    (else (error "Unknown expression")))
  )

; reduce outer-leftmost redex
(define (reduce expr)
  (cond
    ([var? expr] expr)
    ([lambda? expr] (make-lambda (get-var expr) (reduce (get-body expr))))
    ([application? expr]
     (let* ([fn (get-fn expr)]
            [arg (get-arg expr)])
       (if (lambda? fn)
           (substitute (get-body fn) (get-var fn) arg)
           (let ([red (reduce fn)])
             (if (equal? red fn)
                 (list fn (reduce arg))
                 (list red arg))))))
    )
  )

(define (format-expr expr)
  (match expr
    ; (define S '(λ w : (λ y : (λ x : (y ((w y) x))))))
;    [(list 'λ w ': (list 'λ y ': (list 'λ x ': (list y (list (list w y) x))))) "S"]

;    [(list 'λ s ': (list 'λ z ': (list s (list s (list s z))))) "3"]
;    [(list 'λ s ': (list 'λ z ': (list s (list s z)))) "2"]
;    [(list 'λ s ': (list 'λ z ': (list s z))) "1"]
;    [(list 'λ s ': (list 'λ z ': z)) "0"]

    [(list 'λ var ': body) (format "(λ~a.~a)" var (format-expr body))]
    [(list fn arg) (format "(~a ~a)" (format-expr fn) (format-expr arg))]
    [var (format "~a" var)]
    )
  )

; displays λ-expression as a text or tree
(define (display-expr expr [info 'verbose] [n 0])
  (cond
    [(eqv? info 'verbose) (printf "~a: ~a~n" n (format-expr expr))]
    [(eqv? info 'tree) (show-pict (vl-append (text (format "Step: ~a" n) null 24)
                                             (draw-expr expr))
                                  400 400)]
    [else (void)])
  expr)

; reduces a lambda expression into a normal form, set info to 'verbose for displaying each reduction step
(define (eval expr [info 'quite])
  (fresh-symbol -1)
  (check-expr? expr)
  (define (iter expr n)
    (let ([new-expr (reduce (display-expr expr info n))])
      (if (equal? new-expr expr)
          expr
          (iter new-expr (add1 n)))))
  (iter expr 0)
  )


