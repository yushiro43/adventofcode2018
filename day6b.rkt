#lang racket

(define input (file->lines "day6-input.txt"))
;(define input (string-split "1, 3\n4, 5\n3, 1\n6, 7\n5, 5\n" "\n"))

(define point-names (map string (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")))

(define points
  (for/list ([line input])
            (map string->number (string-split line ", "))))

(define max-x (apply max (map car points)))
(define max-y (apply max (map cadr points)))

(define the-map (make-vector (add1 max-x) (vector)))
(define the-points (make-hash))

; place points on map
(for ([point points]
      [point-name point-names])
     (match-let ([(list x y) point])
                (let* ([row (vector-ref the-map x)]
                       [row (if (= 0 (vector-length row)) (make-vector (add1 max-y) " ") row)])
                  (vector-set! row y point-name)
                  (vector-set! the-map x row)
                  (hash-set! the-points point-name point))))

; extend rows that are too short
(for ([row the-map]
      [i (in-range (add1 max-x))])
     (if (= 0 (vector-length row))
       (vector-set! the-map i (make-vector (add1 max-y) " "))
       '()))

(define (get-point-distances-from x y)
  (for/list ([point (hash->list the-points)])
            (let* ([name (car point)]
                   [px (cadr point)]
                   [py (caddr point)]
                   [distance (+ (abs (- px x)) (abs (- py y)))])
              (list name distance))))

(define (find-closest-point x y)
  (let* ([distances (get-point-distances-from x y)]
         [sorted (sort distances < #:key cadr)]
         [closest1 (first sorted)]
         [closest2 (second sorted)])
    (if (= (cadr closest1) (cadr closest2))
      (values "." '())
      (values (car closest1) (cadr closest1)))))

(for ([row the-map]
      [x (in-range (add1 max-x))])
     (for ([value row]
           [y (in-range (add1 max-y))])
          (let ([sum (for/sum ([pd (get-point-distances-from x y)])
                              (match-let ([(list _ distance) pd])
                                         distance))])
            (vector-set! row y sum))))

(define region-size
  (for/sum ([row the-map]
            [x (in-range (add1 max-x))])
           (for/sum ([pos row]
                     [y (in-range (add1 max-y))])
                    (if (< pos 10000)
                      (begin (printf "~s, ~s\n" x y) 1)
                               0))))

(println region-size)

; print map
#;(for ([row the-map])
     (for ([pos row])
          (printf "~s   " pos))
     (displayln ""))
