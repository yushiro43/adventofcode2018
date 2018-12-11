#lang racket

(require "deque.rkt")

(define (play-turn marble circle scores player-num)
  (if (zero? (modulo marble 23))
    (begin
      (deque-rotate! circle -7)
      (let ([removed-marble (deque-pop-left! circle)])
        (hash-update! scores player-num (lambda (s) (+ s marble removed-marble)) 0))
      (deque-rotate! circle 1))
    (begin
      (deque-rotate! circle 2)
      (deque-prepend! circle marble))))

(define (play players marble-count)
  (let ([circle (deque 0)]
        [scores (make-hash)])
    (for ([marble (in-range 1 (add1 marble-count))])
      (let ([player-num (modulo marble players)])
        (play-turn marble circle scores player-num)
        #;(println (deque->list circle))))
    scores))

(define (high-score scores)
  (last (sort (hash-values scores) <)))

;(require rackunit)
;
;(test-case "with 9 players and 25 marbles"
;           (check-eq? (high-score (play 9 25)) 32))
;
;(test-case "with 10 players and 1618 marbles"
;           (check-eq? (high-score (play 10 1618)) 8317))

(let* ([input (file->lines "day9-input.txt")]
       [input-match (regexp-match #rx"([0-9]+) players.* ([0-9]+) points" (first input))]
       [player-count (string->number (second input-match))]
       [marble-count (string->number (third input-match))])
  (printf "part 1: ~s\n" (high-score (play player-count marble-count)))
  (printf "part 2: ~s\n" (high-score (play player-count (* 100 marble-count)))))
