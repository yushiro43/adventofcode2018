#lang racket

(define input (file->lines "day25-input.txt"))
;(define input (string-split "1,-1,-1,-2\n-2,-2,0,1\n0,2,1,3\n-2,3,-2,1\n0,2,3,-2\n-1,-1,1,-2\n0,-2,-1,0\n-2,2,3,-1\n1,2,2,0\n-1,-2,0,-2" "\n"))

(define points (for/list ([line input])
                 (map string->number (string-split line ","))))

(define constellations (list))

(define (distance p1 p2)
  (match-let ([(list x1 y1 z1 t1) p1]
              [(list x2 y2 z2 t2) p2])
    (+ (abs (- x1 x2))
       (abs (- y1 y2))
       (abs (- z1 z2))
       (abs (- t1 t2)))))

(for ([point points])
  (let ([connected (for/list ([constellation constellations]
                              #:when (findf (lambda (p) (<= (distance p point) 3)) (set->list constellation)))
                     constellation)])
    (cond
      [(> (length connected) 1)
       (let ([main (first connected)]
             [others (rest connected)])
         (set-add! main point)
         (for ([other others])
           (set! constellations (remove other constellations))
           (set-union! main other)))]
      [(= (length connected) 1)
       (set-add! (first connected) point)]
      [else
        (set! constellations (cons (mutable-set point) constellations))])))

(println (length constellations))
