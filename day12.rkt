#lang racket

(define (find-rule rules pot)
  (findf (lambda (r)
           (regexp-match (regexp-quote (first r)) pot))
         rules))

(define (pad-state state start-index)
  (let* ([pad-right (if (string-contains? (substring state (- (string-length state) 5)) "#")
                      (string-append state "....")
                      state)])
    (if (string-contains? (substring pad-right 0 5) "#")
      (values
        (string-append "...." pad-right)
        (- start-index 4))
      (values
        pad-right
        start-index))))

(define (grow-generation state start-index rules)
  (let-values ([(state start-index) (pad-state state start-index)])
    (values
      (apply
        string-append
        (for/list ([index (in-range (string-length state))])
          (if (or (< index 2) (> index (- (string-length state) 3)))
            "."
            (let* ([current-pot (substring state (- index 2) (+ index 3))]
                   [matching-rule (find-rule rules current-pot)])
              (if matching-rule
                (last matching-rule)
                ".")))))
      start-index)))

(define (compress-state state start-index)
  (let* ([state (regexp-replace #rx"\\.+$" state "")]
         [prefix-dots (regexp-match #rx"^\\.+" state)])
    (if prefix-dots
      (let* ([prefix-len (string-length (first prefix-dots))]
             [state (substring state prefix-len)]
             [start-index (+ start-index prefix-len)])
        (values
          state 
          start-index))
      (values state start-index))))

(define (grow state start-index rules generations)
  ;(println generations)
  (if (zero? generations)
    (values state start-index)
    (let*-values ([(state start-index) (grow-generation state start-index rules)]
                  [(should-compress?) (zero? (modulo generations 100))]
                  [(state start-index) (if should-compress?
                                         (compress-state state start-index)
                                         (values state start-index))])
      (grow state start-index rules (sub1 generations)))))

(define (plant-indices state start-index)
  (map (lambda (i) (+ (car i) start-index))
       (regexp-match-positions* "#" state)))

(define (sum lst) (foldl + 0 lst))

(require rackunit)

(let* ([rules-string "...## => #\n..#.. => #\n.#... => #\n.#.#. => #\n.#.## => #\n.##.. => #\n.#### => #\n#.#.# => #\n#.### => #\n##.#. => #\n##.## => #\n###.. => #\n###.# => #\n####. => #"]
       [rules (for/list ([r (string-split rules-string "\n")]) (string-split r " => "))]
       [initial-state "#..#.#..##......###...###"])

  (test-case "grow"
             (let-values ([(s i) (grow initial-state 0 rules 0)])
               (check-equal? (string-trim s "." #:repeat? #t) "#..#.#..##......###...###"))
             (let-values ([(s i) (grow initial-state 0 rules 1)])
               (check-equal? (string-trim s "." #:repeat? #t) "#...#....#.....#..#..#..#"))
             (let-values ([(s i) (grow initial-state 0 rules 2)])
               (check-equal? (string-trim s "." #:repeat? #t) "##..##...##....#..#..#..##"))
             (let-values ([(s i) (grow initial-state 0 rules 20)])
               (check-equal? (string-trim s "." #:repeat? #t) "#....##....#####...#######....#.#..##")))

  (test-case "sum"
             (let-values ([(state start-index) (grow initial-state 0 rules 20)])
               (check-eq? (sum (plant-indices state start-index)) 325)))

  )


(let* ([input (file->lines "day12-input.txt")]
       [rules (for/list ([r (drop input 2)]) (string-split r " => "))]
       [initial-state (third (string-split (first input)))]
       [generations (if (> (vector-length (current-command-line-arguments)) 0)
                      (string->number (vector-ref (current-command-line-arguments) 0))
                      20)])
  (let*-values ([(final-state start-index) (grow initial-state 0 rules generations)]
                [(final-state start-index) (compress-state final-state start-index)])
    (printf "final state: ~a\nstart index: ~a\n" final-state start-index)
    (printf "part 1: ~a\n" (sum (plant-indices final-state start-index)))))
