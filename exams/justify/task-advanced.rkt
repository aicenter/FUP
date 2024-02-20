#lang racket

(require racket/match)

(define (total-chars words)
  (foldl + 0 (map string-length words)))

(define (pad words maxWidth)
  (let ((spaces (- maxWidth (foldl + 0 (map string-length words)))))
    (cond
     [(null? words) null]
     [(= 1 (length words))
      (string-append (car words) (make-string (- maxWidth (string-length (car words))) #\space))]
     [else
      (let*-values (((words-to-pad) (length (cdr words)))
		    ((lspace-len) (quotient spaces words-to-pad))
		    ((lspace) (make-string lspace-len #\space))
		    ((bspace-len) (ceiling (/ spaces words-to-pad)))
		    ((bspace) (make-string bspace-len #\space))
		    ((btimes) (- spaces (* words-to-pad lspace-len)))
		    ((bw lw) (split-at words (add1 btimes))))
	(string-append
	 (string-join bw bspace)
	 (if (null? lw) "" (string-join lw lspace #:before-first lspace))))])))


(define (pad-last words maxWidth)
  (let ((trail-space (- maxWidth (+ (sub1 (length words)) (total-chars words)))))
    (string-join words " " #:after-last (make-string trail-space #\space))))


(define (justify words maxWidth)
  (match words
    ['() ""]
    [(list a ...)
     #:when (<= (+ (sub1 (length a)) (foldl + 0 (map string-length a))) maxWidth)
     (list (pad-last a maxWidth))]
    [(list a ..1 b ..1)
     #:when (<= (+ (sub1 (length a)) (foldl + 0 (map string-length a))) maxWidth)
     (cons (pad a maxWidth) (justify b maxWidth))]))


(define words1 '("This" "is" "an" "example" "of" "text" "justification."))
(justify words1 16)
(map string-length (justify words1 16))

(define words2 '("What" "must" "be" "acknowledgment" "shall" "be"))
(justify words2 16)
(map string-length (justify words2 16))

(define words3 '("Science" "is" "what" "we"
                 "understand" "well" "enough"
                 "to" "explain" "to" "a" "computer."
                 "Art" "is" "everything" "else" "we" "do"))
(justify words3 20)
(map string-length (justify words3 20))
