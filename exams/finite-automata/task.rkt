#lang racket

(provide accepts
         lwords)

(struct transition (from-state symbol to-state))
(struct automaton (trans init-state final-states))

(define ((s-next fa a) s)
  (define tr (automaton-trans fa))
  (map transition-to-state
       (filter (lambda (t) (and (equal? s (transition-from-state t))
                                (equal? a (transition-symbol t))))
               tr)))

(define ((next fa) a ss)
  (define tr (automaton-trans fa))
  (apply append (map (s-next fa a) ss)))

(define (accepts fa w)
  (define states (foldl (next fa) (list (automaton-init-state fa)) (string->list w)))
  (if (eq? #f (ormap (lambda (s) (member s (automaton-final-states fa))) states))
      #f
      #t))

(define (words alp n)
  (cond
    ((= n 0) '())
    ((= n 1) (map list alp))
    (else (apply append
                 (map (lambda (w)
                        (map (lambda (a) (cons a w)) alp))
                      (words alp (- n 1)))))))

(define (lwords alp nfa n)
  (filter (lambda (w) (accepts nfa (list->string w)))
          (words (string->list alp) n)))


(define nfa
  (automaton
   (list (transition 1 #\a 2)
         (transition 2 #\b 2)
         (transition 1 #\a 3)
         (transition 3 #\b 4)
         (transition 4 #\a 3)
         (transition 2 #\a 4))
   1
   (list 2 3)))


(accepts nfa "aba")
(lwords "ab" nfa 3)
