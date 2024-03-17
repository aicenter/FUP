#lang racket

;;; Brainf*ck - interpreter
;;; As the symbols ., have their specific meaning in Racket, we replace them by the symbols *@

;;; Sample Brainf*ck programs
; program reading non-negative numbers on the first two positions and displaying their sum
(define add-prg
  '(@ > @ [- < + >] < *))

; program reading non-negative numbers on the first two positions and displaying their product
(define mul-prg
  '(@ > @ < [- > [- > + > + < <] > [- < + >] < <] > > > *))

; constant defining the size of tape
(define SIZE 10)

; constructor of the object tape of a given size
(define (make-tape size)
  (define tape (make-vector size 0))   ; initialize fresh tape
  (define ptr 0)                       ; pointer points to the first element

  (define (change op ptr)
    (vector-set! tape ptr (op (vector-ref tape ptr) 1)))

  (define (move op ptr)
    (let ([new-ptr (op ptr 1)])
      (if (or (< new-ptr 0) (> new-ptr size))
          (error "Moving outside tape")
          new-ptr)))

  (lambda (msg)
    (match msg
      ['tape (list tape ptr)]
      ['plus (change + ptr)]
      ['minus (change - ptr)]
      ['left (set! ptr (move - ptr))]
      ['right (set! ptr (move + ptr))]
      ['dot (vector-ref tape ptr)]
      ['comma (lambda (val) (vector-set! tape ptr val))]
      ['reset (vector-fill! tape 0) (set! ptr 0)])))

; defines a global tape used by the interpreter
(define tape (make-tape SIZE))

; evaluates all the commands beside comma
(define (eval-cmd cmd prg input)
  (match cmd
    ['+ (tape 'plus)] 
    ['- (tape 'minus)]
    ['< (tape 'left)]
    ['> (tape 'right)]
    ['* (printf "~a " (tape 'dot))]
    [_ (error "Unknown command")])
  (eval-prg prg input))   ; recursive call processing further commands

; evaluates comma command, i.e., (car input) -> tape[ptr]
(define (eval-comma prg input)
  (cond
    [(null? input) (error "Empty input")]
    [else ((tape 'comma) (car input))
          (eval-prg prg (cdr input))]))  ; recursive call processing further commands

(define (eval-cycle cycle prg input)
  (if (= (tape 'dot) 0)                         ; is cycle is finished? 
      (eval-prg prg input)                      ; if yes, recursive call preocessing further commands
      (let ([new-input (eval-prg cycle input)]) ; otherwise evaluate cycle code
        (eval-cycle cycle prg new-input))))     ; and execute the cycle again       

(define (log prg input [iwidth 5])
  (displayln (format "tape: ~a  input: ~a  cmd: ~a"
                     (tape 'tape)
                     (~a  input #:min-width iwidth #:align 'right #:left-pad-string " ")
                     (if (empty? prg) "" (car prg)))))

(define (eval-prg prg input)
  (log prg input)
  (match prg
    [(list) input]                ; are all commands processed? if yes, return remaining input
    [(list '@ rest ...) (eval-comma rest input)]
    [(list (? list? cycle) rest ...) (eval-cycle cycle rest input)]
    [(list cmd rest ...) (eval-cmd cmd rest input)]))

; executes the given program with the given input
(define (run-prg prg input)
  (tape 'reset)              ; fill tape by zeros
  (eval-prg prg input)       ; evaluate program
  (displayln "done"))

(define hello-world
  '(+ + + + + + + +
      [> + + + +
         [> + + > + + + > + + + > + < < < < -]
         > + > + > - > > + [<] < -]
      > > * > - - - * + + + + + + + * * + + + * > > * < - * < * + + +
      * - - - - - - * - - - - - - - - * > > + * > + + *))
