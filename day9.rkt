#lang racket

(define input (file->lines "day9-input.txt"))

(define input-match (regexp-match #rx"([0-9]+) players.* ([0-9]+) points" (first input)))
(define player-count (string->number (second input-match)))
(define last-marble (string->number (third input-match)))

;(define player-count 10)
;(define last-marble 1618)

(define score (make-hash))

(define marbles (range (add1 last-marble)))

(define circle (take marbles 1))
(define circle-length 1)
(set! marbles (drop marbles 1))

(define current-position 0)

(define (place-marble number)
  (let* ([position (if (<= circle-length 2) 1 (modulo (+ current-position 2) circle-length))]
         [position (if (= 0 position) circle-length position)])
    (set! current-position position)
    (set! circle (append (take circle position) (list number) (drop circle position)))
    (set! circle-length (add1 circle-length))))

(define (play iteration)
  ;(println score)
  ;(println (append (take circle current-position) (list (list (list-ref circle current-position))) (drop circle current-position)))
  (if (empty? marbles)
    circle
    (let ([player-num (modulo iteration player-count)]
          [marble (first marbles)])
      (println marble)
      (if (= (modulo marble 23) 0)
        (begin
          (hash-update! score player-num (lambda (s) (+ s marble)) 0)
          (let* ([pos-to-remove (modulo (- current-position 7) circle-length)]
                 [marble-to-remove (list-ref circle pos-to-remove)])
            (set! current-position pos-to-remove)
            (set! circle (append (take circle pos-to-remove) (drop circle (add1 pos-to-remove))))
            (set! circle-length (sub1 circle-length))
            (hash-update! score player-num (lambda (s) (+ s marble-to-remove)) 0)))
        (place-marble marble))
      (set! marbles (drop marbles 1))
      (play (add1 iteration)))))

(play 0)

(println (last (sort (hash-values score) <)))
