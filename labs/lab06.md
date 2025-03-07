<SolutionHider/>

# Lab 6: Interpreter of Brainf*ck

This lab is closely related to the corresponding lecture. In that lecture, I showed how to implement
an interpreter of a simple programming language Brainf\*ck (for details, see
[wikipedia](https://en.wikipedia.org/wiki/Brainfuck)). The syntax of Brainf\*ck is very simple. It is
just a sequence of eight possible characters: `< > + - , . [ ]`. The semantics of
this language is captured by a tape of a fixed size consisting of positive numbers and having a
pointer pointing to an active number. Brainf\*ck programs specify computations over this tape.


| Symbol | Substitute | Meaning |
| ------ | ---------- | ------- |
| <      | <          | move pointer to the left by one step |
| >      | >          | move pointer to the right by one step |
| +      | +          | increase the active number determined by pointer by one |
| -      | -          | decrease the active number determined by pointer by one |
| ,      | @          | read one number from input and store it  |
| .      | *          | display the active number|
| [ `code`] | [ `code` ] | while the active number is not zero, execute `code` |

As I explained in the lecture, Racket can represent any tree structure as data. Thus we don't need
to write a parser and represent Brainf*ck directly as a list of commands where nested lists recursively represent cycles. The only limitation with this approach is the special meaning of symbols `,`
and `.`
in Racket. So we replace them with symbols `@ *`
respectively. For instance, the program reading the first two numbers from input, adding them, and displaying the result `,>,[-<+>]<.` is represented in Racket as follows:
```racket
'(@ > @ [- < + >] < *)
```
The square brackets are interpreted in Racket as parentheses, so the cycle automatically creates a nested list.
Another example for your test cases is the program multiplying two numbers:
```racket
'(@ > @ < [- > [- > + > + < <] > [- < + >] < <] > > > *)
```

As the lecture introduced mutable data structures in Racket, I implemented the tape as an object (function closure)
containing a mutable fixed-size vector representing the tape and a number representing the index/pointer of the active number.
The goal of this lab is to modify my implementation from the lecture so that it is purely functional.
This means you must replace the tape object with a purely functional data structure. Next, the implementation of the interpreter
has to be modified a bit. My implementation creates a global tape that the interpreter modifies during the computation.
However, this is not a purely functional code. Thus the tape has to be a part of the state of the computation included among interpreter's
accumulators.

## Task 1
Create a purely functional representation of the tape of a fixed size with a pointer. I will provide detailed hints on how to proceed.
Define a structure `tape` consisting of three components
`left, val, right`
where `left` is the list representing numbers on the tape stored left from the pointer,
`val`
represents the active number and `right` is the list representing the number on the right.

```racket
(struct tape (left val right) #:transparent)

(define (fresh-tape size)
  (tape '() 0 (make-list (- size 1) 0)))
```

Your task is to finish the following function definitions:

```racket
(define (change op t)
  ...

(define (move dir t)
  ...
```

The function `(change op t)`
takes an operation `+` (resp. `-`) and a tape `t` and returns a new tape where the active number is
increased (resp. decreased). The second function `(move dir tape)` takes a direction `'left` (resp.
`'right`) and a tape `t` and returns a new tape where the pointer moves left (resp. right). In case
the active number is a boundary number and moving the pointer would get the pointer outside the
tape, the function should throw an error by calling `(error "outside")`. E.g., consider a tape
consisting of numbers 1,2,3,4,5,6,7 with 4 being the active number.
```racket
(define t (tape '(3 2 1) 4 '(5 6 7)))

(change - t) => '(tape (3 2 1) 3 (5 6 7))
(change + t) => '(tape (3 2 1) 5 (5 6 7))
(move 'left t) => '(tape (2 1) 3 (4 5 6 7))
(move 'right t) => '(tape (4 3 2 1) 5 (6 7))
```

<!--
::: details Solution
```racket
(define (change op t)
  (tape (tape-left t)
        (op (tape-val t) 1)
        (tape-right t)))

(define (move dir t)
  (match (cons dir t)
    [(cons 'left (tape '() _ _)) (error "Outside tape")]
    [(cons 'right (tape _ _ '())) (error "Outside tape")]
    [(cons 'left (tape left val right)) 
     (tape (cdr left) (car left) (cons val right))]
    [(cons 'right (tape left val right))
     (tape (cons val left) (car right) (cdr right))]))
```
:::
-->

## Task 2
Modify the implementation of the interpreter from the lecture so that it uses your purely functional tape.

::: tip Hint
The implementation from the lecture uses a global mutable tape which is defined once and then only reset when executing a program.
Apart from the tape, the interpreter keeps its state in accumulators `prg`
containing remaining commands and `input` storing the remaining input. You have to add one more accumulator tracking the tape.
:::

Moreover, the function `eval-prg` returns the remaining input after processing all commands. This allows us to use it to execute a code in a cycle
and obtain the remaining input to continue with the computation. In your modification, the function `eval-prg` has to return the remaining input
together with the actual tape. This can be done e.g. by returning a list consisting of the input and the tape.

::: details Interpreter from Lecture
For your convenience, my complete implementation is shown below.
```racket
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

  (define (change op vec ptr)
    (vector-set! tape ptr (op (vector-ref tape ptr) 1)))

  (define (move op ptr)
    (let ([new-ptr (op ptr 1)])
      (if (or (< new-ptr 0) (> new-ptr size))
          (error "Moving outside tape")
          new-ptr)))

  (lambda (msg)
    (match msg
      ['tape (list tape ptr)]
      ['plus (change + tape ptr)]
      ['minus (change - tape ptr)]
      ['left (set! ptr (move - ptr))]
      ['right (set! ptr (move + ptr))]
      ['dot (vector-ref tape ptr)]
      ['comma (lambda (val) (vector-set! tape ptr val))]
      ['reset (vector-fill! tape 0) (set! ptr 0)])))

; defines a global tape used by the interpreter
(define tape (make-tape SIZE))

; evaluates comma command, i.e., (car input) -> tape[ptr]
(define (eval-comma prg input)
  (cond
    ([null? input] (error "Empty input"))
    (else ((tape 'comma) (car input))
          (eval-prg prg (cdr input)))))  ; recursive call processing further commands

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

(define (eval-cycle cycle prg input)
  (if (= (tape 'dot) 0)                         ; is cycle is finished?
      (eval-prg prg input)                      ; if yes, recursive call preocessing further commands
      (let ([new-input (eval-prg cycle input)]) ; otherwise evaluate cycle code
        (eval-cycle cycle prg new-input))))     ; and execute the cycle again

(define (eval-prg prg input)
  (displayln (tape 'tape))
  (match prg
    [(list) input]                ; are all commands processed? if yes, return remaining input
    [(list '@ rest ...) (eval-comma rest input)]
    [(list (? list? cmd) rest ...) (eval-cycle cmd rest input)]
    [(list cmd rest ...) (eval-cmd cmd rest input)]))

; executes the given program with the given input
(define (run-prg prg input)
  (tape 'reset)              ; fill tape by zeros
  (eval-prg prg input)       ; evaluate program
  (printf "done~n"))
```
:::

<!--
/*
A solution to Task 2 can be found [[https://drive.google.com/file/d/1tPR3ZxEop3l7qmHoywekLTuc7KhwgS_x/view?usp=sharing|here]].
*/

-->
