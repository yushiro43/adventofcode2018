#lang racket

(define input (file->lines "day7-input.txt"))

(define all-steps (apply set (flatten (for/list ([line input])
                                                (list
                                                  (cadr (regexp-match #rx"Step ([A-Z])" line))
                                                  (cadr (regexp-match #rx"step ([A-Z])" line)))))))

(define (without-dependencies lines steps done)
  (if (empty? lines)
    steps
    (let* ([line (car lines)]
           [depends (cadr (regexp-match #rx"Step ([A-Z])" line))]
           [step (cadr (regexp-match #rx"step ([A-Z])" line))])
      (if (set-member? done depends)
        (without-dependencies (cdr lines) steps done)
        (without-dependencies (cdr lines) (set-remove steps step) done)))))

(define (walk steps done)
  (let ([next-set (without-dependencies input steps done)])
    (if (set-empty? next-set)
      (displayln "")
      (let ([next (first (sort (set->list next-set) string<?))])
        (display next)
        (walk (set-remove steps next) (set-add done next))))))

(walk all-steps (set))

(define (get-time step)
  (+ 60 (- (char->integer (car (string->list step))) 64)))

(define worker-count 5)

(define (get-avail-worker workers)
  (let ([avail (filter (lambda (w) (not (cdr w))) (hash->list workers))])
    (if (empty? avail)
      #f
      (car (first avail)))))

(define (do-work workers done current-second)
  (let ([completed-work (filter (lambda (w) (and (cdr w) (>= current-second (sub1 (last w)))))
                                (hash->list workers))])
    (for ([work completed-work])
         (let ([worker (car work)]
               [step (cadr work)])
           (printf "second ~s, step ~a done, worker ~a back to work!\n" current-second step worker)
           (set! done (set-add done step))
           (set! workers (hash-set workers worker #f))))
    (values workers done)))

(define (walk-in-parallel steps done workers current-second)
  (let ([next-set (without-dependencies input steps done)])
    (if (>= (set-count done) (set-count all-steps))
      ; all done!
      current-second
      ; more work to be done
      (begin
        (set!-values (workers done) (do-work workers done current-second))
        (if (set-empty? next-set)
          ; no work available yet, tick a second
          (walk-in-parallel steps done workers (add1 current-second))
          ; work available
          (let* ([next (first (sort (set->list next-set) string<?))]
                 [worker (get-avail-worker workers)])
            (if worker
              ; available worker, enqueue some work
              (walk-in-parallel
                (set-remove steps next)
                done
                (hash-set workers worker (list next (+ current-second (get-time next))))
                current-second)
              ; no available worker, tick a second
              (walk-in-parallel steps done workers (add1 current-second)))))))))

(define workers
  (apply hash
         (flatten
           (map (lambda (w) (list w #f))
                (range 0 worker-count 1)))))

(printf "\n\ntotal seconds: ~a\n" (walk-in-parallel all-steps (set) workers 0))
