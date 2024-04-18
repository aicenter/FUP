#lang racket
(provide rps)

; ---------------------------------------------------------------------------------------------------
; Multiplayer Rock Paper Scisors:
; Determine the winner(s) after several rounds of multiplayer Rock Paper Scissors.

; Rules of multiplayer RPS: Players stand in a circle and all throw at once.
; If rock, paper, and scissors are all thrown, it is a stalemate, and they rethrow. If only two
; throws are present, all players with the losing throw are eliminated.

; As functional programmers, we believe in determinism :). The strategies of players are known in
; advance; e.g. for two rounds of three-player RPS, the players and actions are represented:
(define players '("alice" "bob" "charlie"))
(define strategies '((r p) (r r) (s p)))
; where Alice throws Rock in the first round, then Paper in the second. Charlie never gets to
; throw his second pick, as he will be eliminated after one round. Alice is the winner.
; ---------------------------------------------------------------------------------------------------

(define (game-finished? strats) (andmap null? strats))
(define (strats-current strats) (map car strats))
(define (strats-future strats) (map cdr strats))

(define (view xs bools)
  (for/list ((x xs) (b bools) #:when b) x))

(define (eliminated-throw throws)
  (match (remove-duplicates (sort throws symbol<?))
    ('(p r) 'r)
    ('(p s) 'p)
    ('(r s) 's)
    (_ '())))

(define (rps-survivors throws)
  (let* ((elim (eliminated-throw throws)))
    (map (curry (compose not equal?) elim) throws)))

(define (rps players strategies)
  (if (game-finished? strategies)
      players
      (let* ((current (strats-current strategies))
             (future (strats-future strategies))
             (mask (rps-survivors current)))
        (rps (view players mask) (view future mask)))))
