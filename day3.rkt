#lang racket

(require srfi/1)

(define input (file->lines "day3-input.txt"))

(define fabric (make-hash))

(define (claim->list x y w h)
  (let* ([horiz (range x (+ x w) 1)]
         [vert  (range y (+ y h) 1)])
    (append-map (lambda (xi)
                  (map (lambda (yi)
                         (list xi yi))
                       vert))
                horiz)))

(define claim-regex #rx"#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)")

(define (add-claim claim)
  (match claim
    [(list claim-num x y w h)
     (let* ([x (string->number x)]
            [y (string->number y)]
            [w (string->number w)]
            [h (string->number h)]
            [points (claim->list x y w h)])
       (for-each (lambda (p)
                   (let* ([x (car p)]
                          [y (cadr p)]
                          [claims (hash-ref fabric (list x y) '())])
                     (hash-set! fabric (list x y) (cons claim-num claims))))
                 points))]))

(define claims (map (lambda (line)
                      (cdr (regexp-match claim-regex line)))
                    input))

(for-each add-claim claims)

(printf "claim sq inches overlapping: ~a\n" (count (lambda (c) (>= (length c) 2)) (hash-values fabric)))

(define overlapping-claims (make-hash))
(for-each (lambda (claim)
            (let ([claim-num (car claim)])
              (hash-set! overlapping-claims claim-num #f))) claims)

(for-each (lambda (claims-at-point)
            (if (> (length claims-at-point) 1)
                (for-each (lambda (claim-num)
                            (hash-set! overlapping-claims claim-num #t)) claims-at-point)
                '()))
          (hash-values fabric))

(define non-overlapping-claims
  (filter identity
          (hash-map overlapping-claims
                    (lambda (claim-num overlapping)
                      (if overlapping
                          #f
                          claim-num)))))

(printf "non-overlapping claim number: ~a\n" (car non-overlapping-claims))