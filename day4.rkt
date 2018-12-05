#lang racket

(define input (sort (file->lines "day4-input.txt") string<?))

(define asleep (make-hash))

(define guard-regex #rx"Guard #([0-9]+) begins shift")
(define sleep-regex #rx"([0-9]+)] falls asleep")
(define awake-regex #rx"([0-9]+)] wakes up")

(define current-guard '())
(define sleep-start '())

(define (new-guard-match match)
  (set! current-guard (cadr match)))

(define (sleep-start-match match)
  (set! sleep-start (string->number (cadr match))))

(define (sleep-stop-match match)
  (let ([minutes (range sleep-start (string->number (cadr match)) 1)]
        [old-minutes (hash-ref asleep current-guard (list))])
    (hash-set! asleep
               current-guard
               (append old-minutes minutes))))

(for-each (lambda (line)
            (cond [(regexp-match guard-regex line) => new-guard-match]
                  [(regexp-match sleep-regex line) => sleep-start-match]
                  [(regexp-match awake-regex line) => sleep-stop-match]))
              input)

(define-values (sleepiest-guard max-minutes)
  (for/fold ([sleepiest '()]
             [max-minutes 0])
            ([guard (hash-keys asleep)])
    (let* ([minutes-list (hash-ref asleep guard)]
           [minutes (length minutes-list)])
      (if (> minutes max-minutes)
          (values guard minutes)
          (values sleepiest max-minutes)))))

(printf "strategy 1\n")
(printf "sleepiest guard: #~a\n" sleepiest-guard)

(define by-minute (make-hash))

(for-each (lambda (minute)
            (hash-update! by-minute minute add1 0))
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
            (for-each (curry inc-guard-count guard)
                      (hash-ref asleep guard)))
          (hash-keys asleep))

(define (max-hash-value hsh)
  (last (sort (hash->list hsh) #:key cdr <)))

(define sleepiest
  (last (sort (hash->list by-minute-global)
              #:key (lambda (pair) (cdr (max-hash-value (cdr pair))))
              <)))

(printf "strategy 2\n")
(printf "sleepiest guard: #~a\n" (car sleepiest))
(printf "sleepiest minute: ~a\n" (car (max-hash-value (cdr sleepiest))))