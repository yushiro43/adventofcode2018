#lang racket

(define (make-recipes recipes recipes-length stop-at [find-this #f] [elf1 0] [elf2 1])
  (when (zero? (modulo recipes-length 10000))
    ;(println recipes-length)
    (when (and find-this (regexp-match find-this recipes))
      (println (car (first (regexp-match-positions find-this recipes))))
      (exit)))
  (if (>= recipes-length stop-at)
    (string-trim recipes)
    (let* ([r1 (string->number (substring recipes elf1 (add1 elf1)))]
           [r2 (string->number (substring recipes elf2 (add1 elf2)))]
           [sum (+ r1 r2)]
           [sum-string (number->string sum)])
      (for ([char (string->list sum-string)]
            [i (in-naturals)])
        (string-set! recipes (+ recipes-length i) char))
      (let ([new-length (+ recipes-length (string-length sum-string))])
        (make-recipes recipes
                      new-length
                      stop-at
                      find-this
                      (modulo (+ elf1 r1 1) new-length)
                      (modulo (+ elf2 r2 1) new-length))))))

(define (get-next-recipes start-recipes recipes-length start-count next-count)
  (substring
    (make-recipes
      start-recipes
      recipes-length
      (+ start-count next-count))
    start-count))

(require rackunit)

(define max-recipes 100000000)
(define start-recipes (string-append "37" (make-string max-recipes #\space)))

(test-case "get-next-recipes"
           (check-equal? (get-next-recipes start-recipes 2 9 10) "5158916779")
           (check-equal? (get-next-recipes start-recipes 2 2018 10) "5941429882"))

(define input "165061")

(printf "part 1: ~a\n"
        (get-next-recipes start-recipes 2 (string->number input) 10))

(printf "part 2: ~a\n"
        (make-recipes start-recipes 2 max-recipes input))
