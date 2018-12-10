#lang racket

(define input (file->lines "day10-input.txt"))
(define line-pattern #rx"position=< *(-?[0-9]+), *(-?[0-9]+)> velocity=< *(-?[0-9]+), *(-?[0-9]+)>")

(struct point (x y vx vy) #:mutable)

(define points (make-hash))

(for ([line input]
      [i (in-range (length input))])
  (match-let ([(list _ x y vx vy) (map string->number (regexp-match line-pattern line))])
    (hash-set! points i (point x y vx vy))))

(define (tick seconds)
  (if (zero? seconds)
    points
    (begin
      (for ([(i point) (in-hash points)])
        (set-point-x! point (+ (point-x point) (point-vx point)))
        (set-point-y! point (+ (point-y point) (point-vy point))))
      (tick (sub1 seconds)))))

(define (get-plane-size plane)
  (let ([min-y (apply min (hash-keys plane))]
        [max-y (apply max (hash-keys plane))]
        [min-x (apply min (append-map hash-keys (hash-values plane)))]
        [max-x (apply max (append-map hash-keys (hash-values plane)))])
    (values min-y max-y min-x max-x)))

(define (build-plane)
  (let ([plane (make-hash)])
    (for ([(i point) (in-hash points)])
      (let ([x (point-x point)]
            [y (point-y point)])
        (when (null? (hash-ref plane y '()))
          (hash-set! plane y (make-hash)))
        (hash-set! (hash-ref plane y) x #t)))
    plane))

(define (print-message)
  (let ([plane (build-plane)])
    (let-values ([(min-y max-y min-x max-x) (get-plane-size plane)])
      (for ([py (range min-y (add1 max-y))])
        (for ([px (range min-x (add1 max-x))])
          (if (hash-ref (hash-ref plane py (hash)) px #f)
            (display "x")
            (display ".")))
        (displayln "")))))

(define (find-smallest-plane seconds min-height min-width)
  (tick 1)
  (let ([plane (build-plane)])
    (let-values ([(min-y max-y min-x max-x) (get-plane-size plane)])
      (let ([height (- max-x min-x)]
            [width (- max-y min-y)])
        (if (or (< height min-height) (< width min-width))
          (find-smallest-plane (add1 seconds) height width)
          seconds)))))

; use this to find the number of seconds to use
;(find-smallest-plane 0 1000000 1000000)

(define _ (tick 10333))
(print-message)
