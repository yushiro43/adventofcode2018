#lang racket

(define input (file->lines "day9-input.txt"))

;(define player-count 9)
;(define last-marble 25)

;(define player-count 10)
;(define last-marble 1618)

(define input-match (regexp-match #rx"([0-9]+) players.* ([0-9]+) points" (first input)))
(define player-count (string->number (second input-match)))
(define last-marble (string->number (third input-match)))

(define score (make-hash))

(define marbles (range (add1 last-marble)))

(struct position (prev value next) #:mutable)

(define circle-length 0)
(define current-position '())

(define (build-circle)
  (define first-pos (position '() (first marbles) '()))
  (set-position-prev! first-pos first-pos)
  (set-position-next! first-pos first-pos)
  (set! circle-length 1)
  (set! current-position first-pos))

(set! marbles (drop marbles 1))

(define (place-marble number)
  (let* ([insert-pos (position-next current-position)]
         [old-next (position-next insert-pos)]
         [new-pos (position insert-pos number old-next)])
    (set-position-next! insert-pos new-pos)
    (set-position-prev! old-next new-pos)
    (set! current-position new-pos)
    (set! circle-length (add1 circle-length))))

(define (circle->list)
  (let-values ([(lst _)
    (for/fold ([l (list)]
               [c current-position])
      ([_ circle-length])
      (values
        (append l (list (position-value c)))
        (position-next c)))])
    lst))

(define (print-circle)
  (println (circle->list)))

(define (play iteration)
  ;(print-circle)
  (if (empty? marbles)
    current-position
    (let ([player-num (modulo iteration player-count)]
          [marble (first marbles)])
      ;(println marble)
      (if (= (modulo marble 23) 0)
        (begin
          (hash-update! score player-num (lambda (s) (+ s marble)) 0)
          (let* ([pos-to-remove (for/fold ([p current-position]) ([_ (range 7)]) (values (position-prev p)))])
            (set-position-next! (position-prev pos-to-remove) (position-next pos-to-remove))
            (set-position-prev! (position-next pos-to-remove) (position-prev pos-to-remove))
            (set! current-position (position-next pos-to-remove))
            (set! circle-length (sub1 circle-length))
            (hash-update! score player-num (lambda (s) (+ s (position-value pos-to-remove))) 0)))
        (place-marble marble))
      (set! marbles (drop marbles 1))
      (play (add1 iteration)))))

(build-circle)
(define p1 (play 0))
(printf "part 1: ~s\n" (last (sort (hash-values score) <)))

(set! last-marble (* 100 last-marble))
(set! score (make-hash))
(set! marbles (range (add1 last-marble)))
(build-circle)
(define p2 (play 0))
(printf "part 2: ~s\n" (last (sort (hash-values score) <)))
