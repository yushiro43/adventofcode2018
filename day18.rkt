#lang racket

(define input (file->lines "day18-input.txt"))
;(define input (string-split ".#.#...|#.\n.....#|##|\n.|..|...#.\n..|#.....#\n#.#|||#|#|\n...#.||...\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|." "\n"))

(define width (string-length (first input)))
(define height (length input))

(define the-map (make-hash))
(for ([line input]
      [y (in-naturals)])
  (for ([char (string->list line)]
        [x (in-naturals)])
    (hash-set! the-map (cons x y) char)))

(define (process-minute the-map)
  (let ([new-map (hash-copy the-map)])
    (for ([y (in-range height)])
      (for ([x (in-range width)])
        (let ([char (hash-ref the-map (cons x y))])
          (cond [(eq? char #\.) (process-open-ground x y the-map new-map)]
                [(eq? char #\|) (process-trees x y the-map new-map)]
                [(eq? char #\#) (process-lumberyard x y the-map new-map)]
                [else (raise "unknown tile")]))))
    new-map))

(define (adjacent-tiles x y the-map)
  (list (hash-ref the-map (cons (sub1 x) (sub1 y)) #f)
        (hash-ref the-map (cons       x  (sub1 y)) #f)
        (hash-ref the-map (cons (add1 x) (sub1 y)) #f)
        (hash-ref the-map (cons (sub1 x)       y ) #f)
        (hash-ref the-map (cons (add1 x)       y ) #f)
        (hash-ref the-map (cons (sub1 x) (add1 y)) #f)
        (hash-ref the-map (cons       x  (add1 y)) #f)
        (hash-ref the-map (cons (add1 x) (add1 y)) #f)))

(define (process-open-ground x y the-map new-map)
  (when (>= (count (lambda (c) (eq? c #\|)) (adjacent-tiles x y the-map)) 3)
    (hash-set! new-map (cons x y) #\|)))

(define (process-trees x y the-map new-map)
  (when (>= (count (lambda (c) (eq? c #\#)) (adjacent-tiles x y the-map)) 3)
    (hash-set! new-map (cons x y) #\#)))

(define (process-lumberyard x y the-map new-map)
  (let ([adjacent (adjacent-tiles x y the-map)])
    (when (not (and  (>= (count (lambda (c) (eq? c #\#)) adjacent) 1)
                     (>= (count (lambda (c) (eq? c #\|)) adjacent) 1)))
      (hash-set! new-map (cons x y) #\.))))

(define (display-map the-map)
  (for ([y (in-range height)])
    (for ([x (in-range width)])
      (let ([char (hash-ref the-map (cons x y))])
        (display char)))
    (display "\n")))

(for ([i (range 10)])
  (set! the-map (process-minute the-map)))

(display-map the-map)

(printf "part1: ~a\n" (* (length (filter (lambda (t) (eq? t #\|)) (hash-values the-map)))
                         (length (filter (lambda (t) (eq? t #\#)) (hash-values the-map)))))

(for ([i (range (- 1000 10))])
  (display i)
  (display "\r")
  (set! the-map (process-minute the-map)))
(displayln "")

;; get repeat factor
;; mine was 56
;
;(define map-1000 (hash-copy the-map))
;(for ([i (range 100)])
;  (display i)
;  (display "\r")
;  (set! the-map (process-minute the-map))
;  (when (equal? the-map map-1000)
;    (println (add1 i))
;    (exit)))

; (modulo (- 1000000000 1000) 56) => 0

; in my case, the map at 1000 is the same as the map at 1000000000
; so no need to alter it further!

; otherwise, you may need to process it a few more times:

;(for ([i (range 0)])
;  (display i)
;  (display "\r")
;  (set! the-map (process-minute the-map)))
;(displayln "")

(display-map the-map)

(printf "part2: ~a\n" (* (length (filter (lambda (t) (eq? t #\|)) (hash-values the-map)))
                         (length (filter (lambda (t) (eq? t #\#)) (hash-values the-map)))))
