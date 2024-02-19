#lang racket

(provide transpose (struct-out entry))
(struct entry (key vals) #:transparent)

(define (extract-keys rel)
  (sort (remove-duplicates (flatten (map entry-vals rel))) symbol<?))

(define ((get-val key) ent)
  (if (member key (entry-vals ent))
      (list (entry-key ent))
      '()))

(define ((get-vals rel) key)
  (entry key (sort (apply append (map (get-val key) rel)) <)))

(define (transpose rel)
  (let ([new-keys (extract-keys rel)])
    (map (get-vals rel) new-keys)))