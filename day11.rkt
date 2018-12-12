#lang racket

(define input 1308)

(define (calc-cell-level x y serial-number)
  (let* ([rack-id (+ x 10)]
         [step1 (* rack-id y)]
         [step2 (+ step1 serial-number)]
         [step3 (* step2 rack-id)]
         [step3-list (string->list (number->string step3))]
         [step4 (if (>= (length step3-list) 3) (list-ref (reverse step3-list) 2) #\0)]
         [step4-int (- (char->integer step4) 48)]
         [step5 (- step4-int 5)])
    step5))

(define (build-grid serial-number)
  (apply vector (for/list ([row (in-range 1 301)])
    (apply vector (for/list ([col (in-range 1 301)])
      (calc-cell-level col row serial-number))))))

(define sum-power-cache (make-hash))

(define (sum-power x y size serial-number grid)
  (let ([key (list x y size)])
    (cond [(hash-has-key? sum-power-cache key) (hash-ref sum-power-cache key)]
          [(or (>= x 300) (>= y 300)) 0]
          [(= 1 size) (vector-ref (vector-ref grid y) x)]
          [(= 0 size) 0]
          [else
            (let ([power (- (+ (sum-power x y (sub1 size) serial-number grid)
                               (sum-power (add1 x) (add1 y) (sub1 size) serial-number grid)
                               (sum-power (+ x (- size 1)) y 1 serial-number grid)
                               (sum-power x (+ y (- size 1)) 1 serial-number grid))
                            (sum-power (add1 x) (add1 y) (- size 2) serial-number grid))])
              (hash-set! sum-power-cache key power)
              power)])))

(define (find-highest-power-square serial-number fixed-size)
  (set! sum-power-cache (make-hash))
  (let ([grid (build-grid serial-number)]
        [high 0]
        [high-x -1]
        [high-y -1]
        [high-size -1])
    (for* ([row (in-range 0 300)]
           [col (in-range 0 300)]
           [size (if fixed-size (list fixed-size) (range 1 (- 301 (max row col))))])
      ;(printf "~a%\n" (~r (* (/ row 300.0) 100) #:precision 1))
      (let ([power (sum-power col row size serial-number grid)])
        (when (> power high)
          (begin
            (set! high power)
            (set! high-x col)
            (set! high-y row)
            (set! high-size size)))))
    (values high (add1 high-x) (add1 high-y) high-size)))

(require rackunit)

(test-case "calc-cell-level 1"
           (check-eq? (calc-cell-level 122 79 57) -5))

(test-case "calc-cell-level 2"
           (check-eq? (calc-cell-level 217 196 39) 0))

(test-case "find-grid 1"
           (let-values ([(level x y s) (find-highest-power-square 18 3)])
             (check-eq? level 29)
             (check-eq? x 33)
             (check-eq? y 45)))

(let-values ([(level x y s) (find-highest-power-square input 3)])
  (printf "part1: ~s,~s\n" x y))

(let-values ([(level x y s) (find-highest-power-square input #f)])
  (printf "part2: ~s,~s,~s\n" x y s))
