#lang racket 
(provide eval state (struct-out state))

(struct state (x y dir) #:transparent)
(define (update-dir st dir)
  (state (state-x st) (state-y st) dir))
(define (update-x st x)
  (state x (state-y st) (state-dir st)))
(define (update-y st y)
  (state (state-x st) y (state-dir st)))

(define prg '(left move move right move))

(define (turn-left st)
  (let ([new-dir 
    (match (state-dir st)
      ['north 'west]
      ['west 'south]
      ['south 'east]
      ['east 'north])])
    (update-dir st new-dir)))

(define turn-right (compose turn-left turn-left turn-left))

(define (move st)
  (match (state-dir st)
    ['north (update-y st (+ (state-y st) 1))]
    ['south (update-y st (- (state-y st) 1))]
    ['west (update-x st (- (state-x st) 1))]
    ['east (update-x st (+ (state-x st) 1))]))

(define (eval-cmd cmd)
  (match cmd
    ['left turn-left]
    ['right turn-right]
    ['move move]))

(define (eval prg st)
  ((apply compose (map eval-cmd (reverse prg))) st))

