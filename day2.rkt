#lang racket

(require srfi/1)

(define input (file->lines "day2-input.txt"))

(define (count-of char string)
  (count (lambda (c) (eq? c char)) (string->list string)))

(define (string->uniq-chars string)
  (remove-duplicates (string->list string)))

(define (repeat-by? desired-count string)
  (let* ([chars (remove-duplicates (string->list string))]
         [counts (map (lambda (c) (count-of c string)) chars)])
    (any (lambda (c) (= c desired-count)) counts)))

(let* ([count2 (count (lambda (s) (repeat-by? 2 s)) input)]
       [count3 (count (lambda (s) (repeat-by? 3 s)) input)]
       [checksum (* count2 count3)])
  (printf "checksum: ~s\n" checksum))

(define (distance s1 s2)
  (let* ([l1 (string->list s1)]
         [l2 (string->list s2)]
         [diffs (map (lambda (c1 c2)
                       (if (eq? c1 c2)
                           0
                           1))
                     l1 l2)])
    (reduce + 0 diffs)))

(define (find-off-by-one primaries)
  (let* ([primary (car primaries)]
         [matching (find (lambda (candidate) (= 1 (distance primary candidate))) input)])
    (if matching
       (values primary matching)
       (find-off-by-one (cdr primaries)))))

(let-values ([(primary matching) (find-off-by-one input)])
  (printf "primary:  ~a\n" primary)
  (printf "matching: ~a\n" matching)
  (let* ([l1 (string->list primary)]
         [l2 (string->list matching)]
         [same (reverse (foldl (lambda (c1 c2 result)
                        (if (eq? c1 c2)
                            (cons c1 result)
                            result))
                      '()
                      l1
                      l2))])
    (printf "common:   ~a\n" (list->string same))))