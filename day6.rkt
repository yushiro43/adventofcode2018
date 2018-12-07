#lang racket

(define input (file->lines "day6-input.txt"))
;(define input (string-split "1, 1\n10, 1\n1, 10\n10, 10\n5, 5" "\n"))

(define point-names (map string (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")))

(define points
  (for/list ([line input])
            (map string->number (string-split line ", "))))

(define max-x (apply max (map car points)))
(define max-y (apply max (map cadr points)))

(define border-size 8)

(define (translate xy)
  (+ (sub1 xy) border-size))

(define (make-map)
  (apply vector (map (lambda (_)
                       (make-vector (+ max-y (* border-size 2)) " "))
                     (range (+ max-x (* border-size 2))))))

(define (fill-map)
  (for ([point points]
        [point-name point-names])
       (match-let ([(list x y) point])
                  (let ([row (vector-ref the-map (translate x))])
                    (vector-set! row (translate y) point-name)
                    (vector-set! the-map (translate x) row)
                    (hash-set! the-points point-name point)))))

(define the-map (make-map))
(define the-points (make-hash))
(fill-map)

(define (get-point-distances-from x y)
  (for/list ([point (hash->list the-points)])
            (let* ([name (car point)]
                   [px (translate (cadr point))]
                   [py (translate (caddr point))]
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

(define (find-areas)
  (for ([row the-map]
        [x (in-range (+ max-x (* border-size 2)))])
       (for ([value row]
             [y (in-range (+ max-y (* border-size 2)))]
             #:when (string=? " " value))
            (let-values ([(point-name distance) (find-closest-point x y)])
                        (vector-set! row y point-name))))

  (for*/fold ([area (hash)])
             ([point-name (hash-keys the-points)]
              [row the-map]
              [pos row])
             (if (string=? pos point-name)
               (hash-update area point-name add1 0)
               area)))

(define areas1 (hash-values (find-areas)))
(set! border-size 10)
(set! the-map (make-map))
(fill-map)
(define areas2 (hash-values (find-areas)))
(printf "largest non-infinite area: ~s\n" (last (sort (set-intersect areas1 areas2) <)))
