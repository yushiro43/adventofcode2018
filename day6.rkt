#lang racket

(define input (file->lines "day6-input.txt"))
;(define input (string-split "1, 1\n10, 1\n1, 10\n10, 10\n5, 5" "\n"))

(define point-names (map string (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")))

(define coords (for/list ([line input]) (map string->number (string-split line ", "))))
(define points (make-hash (for/list ([c coords] [n point-names]) (cons n c))))

(define max-x (apply max (map first (hash-values points))))
(define max-y (apply max (map last (hash-values points))))

(define border-size 0)

(define (translate xy)
  (+ (sub1 xy) border-size))

(define (make-map)
  (apply vector (map (lambda (_)
                       (make-vector (+ max-y (* border-size 2)) " "))
                     (range (+ max-x (* border-size 2))))))

(define (build-map points)
  (let ([the-map (make-map)])
    (for ([point (hash-values points)]
          [point-name point-names])
         (match-let ([(list x y) point])
                    (let ([row (vector-ref the-map (translate x))])
                      (vector-set! row (translate y) point-name)
                      (vector-set! the-map (translate x) row))))
    the-map))

(define (get-point-distances-from x y)
  (for/list ([point (hash->list points)])
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

(define (find-areas the-map)
  (for ([row the-map]
        [x (in-range (+ max-x (* border-size 2)))])
       (for ([value row]
             [y (in-range (+ max-y (* border-size 2)))]
             #:when (string=? " " value))
            (let-values ([(point-name distance) (find-closest-point x y)])
                        (vector-set! row y point-name))))

  (for*/fold ([area (hash)])
             ([point-name (hash-keys points)]
              [row the-map]
              [pos row])
             (if (string=? pos point-name)
               (hash-update area point-name add1 0)
               area)))

(define (fill-distance-totals the-map)
  (for ([row the-map]
        [x (in-range (add1 max-x))])
       (for ([value row]
             [y (in-range (add1 max-y))])
            (let ([sum (for/sum ([pd (get-point-distances-from x y)])
                                (match-let ([(list _ distance) pd])
                                           distance))])
              (vector-set! row y sum))))
  the-map)

(define (safe-region-size the-map)
  (for/sum ([row the-map]
            [x (in-range (add1 max-x))])
           (for/sum ([pos row]
                     [y (in-range (add1 max-y))])
                    (if (< pos 10000)
                      1
                      0))))

(set! border-size 8)
(let* ([map1 (build-map points)]
       [areas1 (hash-values (find-areas map1))])
  ; run it again with a different border size
  ; we'll assume areas that change are "infinite"
  (set! border-size 10)
  (let* ([map2 (build-map points)]
         [areas2 (hash-values (find-areas map2))]
         [non-changing (set-intersect areas1 areas2)])
    (printf "largest non-infinite area: ~s\n" (last (sort non-changing <)))))

(set! border-size 0)
(let* ([map3 (fill-distance-totals (build-map points))]
       [size (safe-region-size map3)])
  (printf "safe region: ~s\n" size))
