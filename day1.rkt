#lang racket

(require srfi/1)

(define input (file->list "day1-input.txt"))

(define final-freq (fold + 0 input))

(println final-freq)

(define (find-repeat-freq counts frequency index max-index)
  (if (> index max-index)
      (find-repeat-freq counts frequency 0 max-index)
      (let* ([op (list-ref input index)]
             [new-freq (+ frequency op)]
             [old-count (hash-ref counts new-freq 0)]
             [count (+ old-count 1)])
        (hash-set! counts new-freq count)
        ;(println (list frequency op new-freq counts))
        (if (>= count 2)
            new-freq
            (find-repeat-freq counts new-freq (+ index 1) max-index)))))

(define repeat-freq
  (find-repeat-freq
   (make-hash '((0 . 1)))
   0
   0
   (- (length input) 1)))

(println repeat-freq)