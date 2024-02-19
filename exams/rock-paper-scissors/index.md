---
title: "Exam Task: Multiplayer *Rock, Paper, Scissors*"
subtitle: "From: Exam 2023-06-19"
date: "2024-01-12"
author: "Niklas Heim"
author-url: ""
return-url: '../../'
return-text: '← Return home'
---

Determine the winner (or remaining players) after several rounds of multiplayer *Rock, Paper, Scissors*.

Players stand in a circle and all throw at once.  If rock, paper, and scissors are all thrown, it is
a stalemate, and they rethrow. If only two throws are present, all players with the losing throw are
eliminated.  The following function decides which throws from a set of set of *throws* in a
single round should be eliminated.
$$
\text{eliminated}(\text{throws}) \equiv \begin{cases}
	\{`rock`\} &\{`paper`, `rock`\} = \text{throws} \\
	\{`paper`\} &\{`paper`, `scissors`\} = \text{throws} \\
	\{`scissors`\} &\{`rock`, `scissors`\} = \text{throws} \\
	\emptyset &\text{otherwise}
	\end{cases}
$$

The actions of players are decided in advance; e.g. for two rounds of three-player RPS, the players
and actions are represented as:
```{.tight-code .scheme}
(define players '("alice" "bob" "charlie"))
(define strategies '((r p) (r r) (s p)))
```
where Alice throws Rock in the first round, then Paper in the second. Charlie never gets to throw
his second pick (Paper), as he will be eliminated after one round. Alice is the only remaining
player after the two rounds. The result should be `'("alice")`.

You will need to keep track of the players and their actions. In each round, figure out which throws
should be eliminated, then filter out the corresponding players and their strategies. Create two
helper functions: one that creates a boolean "mask" of all the winners of a single round, and one
that performs the filtering.

# Racket
Throws will be represented as the symbols `'r`, `'p` and `'s`.
```{.tight-code .scheme}
#lang racket
(provide rps)

(define (game-finished? strats) (or (null? strats) (ormap null? strats)))
(define (strats-current strats) (map car strats))
(define (strats-future strats) (map cdr strats))

(define (rps players strategies)
  (if (game-finished? strategies)
    players
    (let* ((current (strats-current strategies))
           (future (strats-future strategies)))
        ; Implement me!
      )
)
```
You can use the `remove-duplicates` function to remove duplicate throws and `sort` with `symbol<?` to order throws.

## Examples

Alice wins in the second round. Charlie loses immediately.
```{.tight-code .scheme}
(define players '("alice" "bob" "charlie"))
(define strategies '((r p) (r r) (s p)))

(rps players strategies); '("alice")
```

Charlie loses because rock beats scissors (single round).
```{.tight-code .scheme}
(define players '("alice" "bob" "charlie"))
(define strategies '((r) (r) (s)))

(rps players strategies); '("alice" "bob")
```

First two rounds are stalemates (stalemate, stalemate, win).
```{.tight-code .scheme}
(define players '("alice" "bob" "charlie"))
(define strategies '((r p r) (p s r) (s r p)))

(rps players strategies) ; '("charlie")
```

<details class="admonition">
<summary><strong>Solution</strong></summary>
```{.tight-code .scheme}
#lang racket

(provide rps)

(define players '("alice" "bob" "charlie"))
(define strategies '((r p) (r r) (s p)))

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
```
</details>

# Haskell

For simplicity, throws will be represented as the characters `'r'`, `'p'` and `'s'`.
```{.tight-code .haskell}
module Task4 (rps) where
import Data.List

isFinished xs = null xs || any null xs

currentStrategies = map head

futureStrategies = map tail

rps :: [String] -> [[Char]] -> [String]
rps players strategies | isFinished strategies = players
rps players strategies =
    let current = currentStrategies strategies
        future = futureStrategies strategies
    in [] -- Implement me !
```
You can use the `nub` function to remove duplicate throws and `sort` to order the throws.

## Examples

Alice wins in the second round. Charlie loses immediately.
```{.tight-code .haskell}
players = ["alice", "bob", "charlie"]
strategies = [['r', 'p'], ['r', 'r'], ['s', 'p']]

rps players strategies -- ["alice"]
```


Charlie loses because rock beats scissors (single round).
```{.tight-code .haskell}
players = ["alice", "bob", "charlie"]
strategies = [['r'], ['r'], ['s']]

rps players strategies -- ["alice", "bob"]
```


First two rounds are stalemates (stalemate, stalemate, win).
```{.tight-code .haskell}
players = ["alice", "bob", "charlie"]
strategies = [['r', 'p', 'r'], ['p', 's', 'r'], ['s', 'r', 'p']]

rps players strategies -- ["charlie"]
```

<details class="admonition">
<summary><strong>Solution</strong></summary>
```{.tight-code .haskell}
module Task4 (rps) where
import Data.List

isFinished xs = null xs || any null xs

currentStrategies = map head

futureStrategies = map tail

view xs bs = map fst $ filter snd $ zip xs bs

eliminated throws =
    let e = elim $ sort $ nub throws
    in map (/= e) throws
    where
        elim ['p', 'r'] = 'r'
        elim ['p', 's'] = 'p'
        elim ['r', 's'] = 's'
        elim _ = ' '

rps :: [String] -> [[Char]] -> [String]
rps players strategies | isFinished strategies = players
rps players strategies =
    let current = currentStrategies strategies
        future = futureStrategies strategies
        mask = eliminated current
    in rps (view players mask) (view future mask)
```
</details>
