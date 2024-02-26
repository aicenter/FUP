#lang racket

; (provide cheap-flight)

(define (nodes gr) (car gr))
(define (edges gr) (cadr gr))
(define (cost edge) (caddr edge))

; list of nodes
(define ns '(1 2 3 4 5 6)) ; listofnodes
; list of edges where each edge contains (start end cost)
(define es '((1 2 0.5) (1 3 1.0) (2 3 2.0) (2 5 1.0) (3 4 4.0) (4 5 1.0)))
; the graph; a list of nodes and edges
(define gr (list ns es))


(define (next-pos node graph)
  (define es (edges graph))
  (append
    (map (lambda (e) (list (car e) (cost e)))
         (filter (lambda (e) (= (cadr e) node)) es))
    (map (lambda (e) (list (cadr e) (cost e)))
         (filter (lambda (e) (= (car e) node)) es))))

; (define (extend path cost graph)
;   (if (empty? path) '()
;     (let* ([node (car path)]
;            [nps (next-pos node graph)])
;       (map (lambda (n c) (cons n path, cost+c))
;            (next-pos (car path) graph)))))


(define (extend path cost graph)
  (if (empty? path) '()
    (map (lambda (node-cost)
           (let* ([node (car node-cost)]
                  [c (cadr node-cost)])
             (list (cons node path) (+ cost c))))
         (next-pos (car path) graph))))

(define (bfs visited upaths q graph)
  (if (empty? upaths) '()
    (let* ([paths (sort upaths cheaper?)]
           [path-cost (car paths)]
           [path (car path-cost)]
           [node (car path)]
           [c (cadr path-cost)])
      (cond [(equal? node q) (cons (reverse path) c)]
            [(member node visited) (bfs visited paths q graph)]
            [else (bfs
                    (cons node visited)
                    (append paths (extend path c graph))
                    q graph)]
      ;(list paths path p c)
      ))
    ))

(define (cheaper? x y) (< (cadr x) (cadr y)))
