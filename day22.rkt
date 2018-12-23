#lang racket

(require graph)

(define erosion-cache (make-hash))

(define (risk-level x y depth target-x target-y)
  (modulo (erosion-level x y depth target-x target-y) 3))

(define (total-risk-level depth target-x target-y)
  (foldl + 0
    (for*/list ([y (range (add1 target-y))]
                [x (range (add1 target-x))])
      (risk-level x y depth target-x target-y))))

(define (erosion-level x y depth target-x target-y)
  (if (hash-has-key? erosion-cache (list x y depth))
    (hash-ref erosion-cache (list x y depth))
    (let ([level (modulo (+ (geo-index x y depth target-x target-y) depth) 20183)])
      (hash-set! erosion-cache (list x y depth) level)
      level)))

(define (geo-index x y depth target-x target-y)
  (cond
    [(and (eq? x 0) (eq? y 0)) 0]
    [(and (eq? target-x x) (eq? target-y y)) 0]
    [(eq? y 0) (* x 16807)]
    [(eq? x 0) (* y 48271)]
    [else (* (erosion-level (sub1 x) y depth target-x target-y)
             (erosion-level x (sub1 y) depth target-x target-y))]))

; allowed:
; 0 = gear or torch
; 1 = gear or none
; 2 = torch or none
(define (travel-time type equip target-type target-equip)
  (match (list type equip target-type target-equip)
    ; type 0
    [(list 0 'gear  0 'gear ) 1]
    [(list 0 'gear  0 'torch) 7]
    [(list 0 'torch 0 'gear ) 7]
    [(list 0 'torch 0 'torch) 1]
    [(list 0 'gear  1 'gear ) 1]
    [(list 0 'torch 2 'torch) 1]
    ; type 1
    [(list 1 'gear  0 'gear ) 1]
    [(list 1 'gear  1 'gear ) 1]
    [(list 1 'gear  1 'none ) 7]
    [(list 1 'none  1 'gear ) 7]
    [(list 1 'none  1 'none ) 1]
    [(list 1 'none  2 'none ) 1]
    ; type 2
    [(list 2 'torch 0 'torch) 1]
    [(list 2 'none  1 'none ) 1]
    [(list 2 'torch 2 'torch) 1]
    [(list 2 'torch 2 'none ) 7]
    [(list 2 'none  2 'torch) 7]
    [(list 2 'none  2 'none ) 1]
    ; all other impossible
    [else #f]))

(define (build-graph depth target-x target-y)
  (let ([g (weighted-graph/directed (list))])
    (for* ([y (range (+ target-y 25))]
           [x (range (+ target-x 25))]
           [e '(gear torch none)]) ; equipment
      (add-vertex! g (list x y e))
      (let ([type (risk-level x y depth target-x target-y)])
        ; neighbors
        (for ([target (list (cons (sub1 x) y)
                            (cons x (sub1 y))
                            (cons (add1 x) y)
                            (cons x (add1 y)))])
          (let ([xt (car target)]
                [yt (cdr target)])
            (when (and (>= xt 0) (>= yt 0))
              (add-vertex! g (list xt yt e))
              (let ([weight (travel-time type e (risk-level xt yt depth target-x target-y) e)])
                (when weight
                  (add-directed-edge! g (list x y e) (list xt yt e) weight))))))
        ; self
        (for ([e2 '(gear torch none)]) ; equipment to switch to
          (when (not (eq? e e2))
            (let ([weight (travel-time type e type e2)])
              (when weight
                (unless (has-edge? g (list x y e) (list x y e2))
                  (add-directed-edge! g (list x y e) (list x y e2) weight))))))))
    g))

(define (all-types depth target-x target-y)
  (let ([all (make-hash)])
    (for* ([y (range (add1 target-y))]
           [x (range (add1 target-x))])
      (hash-set! all (cons x y) (risk-level x y depth target-x target-y)))))

(require rackunit)

(test-case "erosion-level"
           (check-eq? (erosion-level 0 0 510 10 10) 510)
           (check-eq? (erosion-level 1 0 510 10 10) 17317)
           (check-eq? (erosion-level 0 1 510 10 10) 8415)
           (check-eq? (erosion-level 1 1 510 10 10) 1805)
           (check-eq? (erosion-level 10 10 510 10 10) 510))

(test-case "risk-level"
           (check-eq? (risk-level 0 0 510 10 10) 0)
           (check-eq? (risk-level 1 0 510 10 10) 1)
           (check-eq? (risk-level 2 0 510 10 10) 0)
           (check-eq? (risk-level 3 0 510 10 10) 2)
           (check-eq? (risk-level 0 1 510 10 10) 0)
           (check-eq? (risk-level 10 10 510 10 10) 0))

(test-case "total-risk-level"
           (check-eq? (total-risk-level 510 10 10) 114))

; allowed:
; 0 = gear or torch (rocky  .)
; 1 = gear or none  (wet    =)
; 2 = torch or none (narrow |)
(test-case "travel-time"
           (check-eq? (travel-time 0 'torch 0 'gear) 7)
           (check-eq? (travel-time 0 'gear 1 'gear) 1)
           (check-eq? (travel-time 0 'torch 1 'torch) #f)
           (check-eq? (travel-time 0 'torch 1 'none) #f))

(test-case "shortest path"
  (let ([g (build-graph 510 10 10)])
    (let-values ([(distances _) (dijkstra g (list 0 0 'torch))])
      (check-eq? (hash-ref distances (list 10 10 'torch)) 45))))

(define depth 11109)
(define target-x 9)
(define target-y 731)

(printf "part 1: ~a\n" (total-risk-level depth target-x target-y))

(printf "part 2: ~a\n"
  (let ([g (build-graph depth target-x target-y)])
    (let-values ([(distances _) (dijkstra g (list 0 0 'torch))])
      (hash-ref distances (list target-x target-y 'torch)))))
