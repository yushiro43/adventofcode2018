#lang racket

(define track-pieces
  (hash #\^ #\|
        #\v #\|
        #\< #\-
        #\> #\-))

(define direction-changes
  (hash (cons #\^ #\\) #\<
        (cons #\^ #\/) #\>
        (cons #\v #\\) #\>
        (cons #\v #\/) #\<
        (cons #\< #\\) #\^
        (cons #\< #\/) #\v
        (cons #\> #\\) #\v
        (cons #\> #\/) #\^))

(define intersection-turns
  ; 0 = left, 1 = straight, 2 = right
  (hash (cons #\^ 0) #\<
        (cons #\^ 1) #\^
        (cons #\^ 2) #\>
        (cons #\v 0) #\>
        (cons #\v 1) #\v
        (cons #\v 2) #\<
        (cons #\< 0) #\v
        (cons #\< 1) #\<
        (cons #\< 2) #\^
        (cons #\> 0) #\^
        (cons #\> 1) #\>
        (cons #\> 2) #\v))

(struct player (num direction x y dir-mod alive) #:mutable)

(define (build-track-and-players input)
  (let ([track (make-hash)]
        [players (make-hash)]
        [player-num 0])
    (for ([line (string-split input "\n")]
          [y (in-range (string-length input))])
      (for ([char (string->list line)]
            [x (in-range (string-length line))])
        (hash-set! track (cons x y) (hash-ref track-pieces char char))
        (when (member char '(#\^ #\v #\< #\>))
          (hash-set! players player-num (player player-num char x y 0 #t))
          (set! player-num (add1 player-num)))))
    (values track players)))

(define (player-positions players)
  (let ([positions (make-hash)])
    (for ([(player-num player) (in-hash players)])
      (when (player-alive player)
        (hash-update! positions
                      (cons (player-x player) (player-y player))
                      (lambda (lst) (cons player lst))
                      (list))))
    positions))

(define (players-in-order players)
  (let* ([positions (player-positions players)]
         [sorted (sort (hash-keys positions)
                       (lambda (pos1 pos2)
                         (let ([x1 (car pos1)]
                               [y1 (cdr pos1)]
                               [x2 (car pos2)]
                               [y2 (cdr pos2)])
                           (< (* x1 y1) (* x2 y2)))))])
    (flatten (for/list ([pos sorted])
               (hash-ref positions pos)))))

(define (turn-player player track-char)
  (let ([dir (player-direction player)])
    (if (eq? track-char #\+)
      (begin
        (let* ([dir-mod (modulo (player-dir-mod player) 3)]
               [new-dir (hash-ref intersection-turns (cons dir dir-mod))])
          (set-player-dir-mod! player (add1 (player-dir-mod player)))
          new-dir))
      (hash-ref direction-changes (cons dir track-char) dir))))

(define (move-player player track)
  (let* ([x (player-x player)]
         [y (player-y player)]
         [dir (player-direction player)]
         [next-x (hash-ref (hash #\< (sub1 x) #\> (add1 x)) dir x)]
         [next-y (hash-ref (hash #\^ (sub1 y) #\v (add1 y)) dir y)]
         [next-track (hash-ref track (cons next-x next-y))]
         [next-dir (turn-player player next-track)])
    (set-player-direction! player next-dir)
    (set-player-x! player next-x)
    (set-player-y! player next-y)))

(define (move-players players track return-impact)
  (for ([player (players-in-order players)])
    (move-player player track)
    (let ([collided (collision? players)])
      (when collided
        (for ([dead-player (hash-ref (player-positions players) collided)])
          (set-player-alive! dead-player #f))
        (when return-impact (return-impact collided))))))

(define (alive-players players)
  (filter (lambda (p) (player-alive p)) (hash-values players)))

(define (tick track players stop-on-first-impact)
  ;(display-track track players 0 100 0 25)
  ;(sleep 0.1)
  (call/cc (lambda (return)
    (let ([alive (alive-players players)])
      (if (eq? 1 (length alive))
        (cons (player-x (first alive)) (player-y (first alive)))
        (begin
          (move-players players track stop-on-first-impact)
          (tick track
                players
                (if stop-on-first-impact return #f))))))))

(define (collision? players)
  (let* ([positions (player-positions players)]
         [colliding (filter (lambda (pos) (> (length (hash-ref positions pos)) 1)) (hash-keys positions))])
    (if (empty? colliding)
      #f
      (first colliding))))

(define (display-track track players x width y height)
  (display "\033[2J\033[;H")
  (for ([y (in-range y (+ y height))])
    (for ([x (in-range x (+ x width))])
      (let ([track-char (hash-ref track (cons x y) " ")]
            [players-here (filter (lambda (p)
                                    (and (eq? (player-x p) x)
                                         (eq? (player-y p) y)))
                                  (hash-values players))])
        (cond [(empty? players-here) (display track-char)]
              [(eq? 1 (length players-here)) (display (player-direction (first players-here)))]
              [else (display "X")])))
    (displayln "")))

(require rackunit)

(test-case "sample input"
           (let*-values ([(input) "/->-\\       \n|   |  /----\\\n| /-+--+-\\  |\n| | |  | v  |\n\\-+-/  \\-+--/\n  \\------/"]
                         [(track players) (build-track-and-players input)])
             (check-equal? (tick track players #t) (cons 7 3)))
           (let*-values ([(input) "/>-<\\\n|   |\n| /<+-\\\n| | | v\n\\>+</ |\n  |   ^\n  \\<->/"]
                         [(track players) (build-track-and-players input)])
             (check-equal? (tick track players #f) (cons 6 4))))

(let*-values ([(input) (file->string "day13-input.txt")]
              [(track players) (build-track-and-players input)])
  (printf "part 1: ~s\n" (tick track players #t)))

(let*-values ([(input) (file->string "day13-input.txt")]
              [(track players) (build-track-and-players input)])
  (printf "part 2: ~s\n" (tick track players #f)))
