#lang racket

(define input (sort (file->lines "day4-input.txt") string<?))

(define asleep (make-hash))

(define guard-regex #rx"Guard #([0-9]+) begins shift")
(define sleep-regex #rx"([0-9]+)] falls asleep")
(define awake-regex #rx"([0-9]+)] wakes up")

(define current-guard '())
(define sleep-start '())

(for-each (lambda (line)
            (let ([guard-match (regexp-match guard-regex line)]
                  [start-match (regexp-match sleep-regex line)]
                  [stop-match  (regexp-match awake-regex line)])
              (cond [guard-match (set! current-guard (cadr guard-match))]
                    [start-match (set! sleep-start (string->number (cadr start-match)))]
                    [stop-match
                     (let ([minutes (range sleep-start (string->number (cadr stop-match)) 1)])
                       (hash-set! asleep
                                  current-guard
                                  (append (hash-ref asleep current-guard (list)) minutes)))])))
          input)

(define (find-sleepiest-guard guards sleepiest sleepiest-minutes)
  (if (empty? guards)
      sleepiest
      (let* ([guard (car guards)]
             [sleep-minutes (hash-ref asleep guard)]
             [sleep-total (length sleep-minutes)])
        (if (> sleep-total sleepiest-minutes)
            (find-sleepiest-guard (cdr guards) guard sleep-total)
            (find-sleepiest-guard (cdr guards) sleepiest sleepiest-minutes)))))

(define sleepiest-guard (find-sleepiest-guard (hash-keys asleep) '() 0))

(printf "strategy 1\n")
(printf "sleepiest guard: #~a\n" sleepiest-guard)

(define by-minute (make-hash))

(for-each (lambda (minute)
            (hash-set! by-minute minute (add1 (hash-ref by-minute minute 0))))
          (hash-ref asleep sleepiest-guard))

(printf "sleepiest minute: ~s\n"
        (car (last (sort (hash->list by-minute)
                         (lambda (a b)
                           (< (cdr a) (cdr b)))))))

(define by-minute-global (make-hash))

(define (inc-guard-count guard minute)
  (let* ([hash (hash-ref by-minute-global minute (hash))])
    (hash-set! by-minute-global minute (hash-set hash guard (add1 (hash-ref hash guard 0))))))

(for-each (lambda (guard)
            (for-each (lambda (minute) (inc-guard-count guard minute))
                      (hash-ref asleep guard)))
          (hash-keys asleep))

(define (sort-guards a b)
  (< (cdr a) (cdr b)))

(define sleepiest
  (last (sort (hash->list by-minute-global)
              (lambda (a b)
                (< (cdr (last (sort (hash->list (cdr a)) sort-guards)))
                   (cdr (last (sort (hash->list (cdr b)) sort-guards))))))))

(printf "strategy 2\n")
(printf "sleepiest guard: #~a\n" (car sleepiest))
(printf "sleepiest minute: ~a\n" (car (last (sort (hash->list (cdr sleepiest)) sort-guards))))