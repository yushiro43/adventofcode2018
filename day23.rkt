#lang racket

(struct bot (x y z r) #:transparent)

(define input (file->lines "day23-input.txt"))
;(define input (string-split "pos=<0,0,0>, r=4\npos=<1,0,0>, r=1\npos=<4,0,0>, r=3\npos=<0,2,0>, r=1\npos=<0,5,0>, r=3\npos=<0,0,3>, r=1\npos=<1,1,1>, r=1\npos=<1,1,2>, r=1\npos=<1,3,1>, r=1" "\n"))

(define (find-bots-in-range bot bots)
  (for/list ([b bots]
             #:when (<= (distance bot b) (bot-r bot)))
    b))

(define (distance b1 b2)
  (+ (abs (- (bot-x b1) (bot-x b2)))
     (abs (- (bot-y b1) (bot-y b2)))
     (abs (- (bot-z b1) (bot-z b2)))))

(define (counts-bots-in-range bots resolution [near-bot #f])
  (let ([h (make-hash)])
    (let ([min-x (if near-bot (- (bot-x near-bot) (* resolution 10)) (apply min (map bot-x bots)))]
          [max-x (if near-bot (+ (bot-x near-bot) (* resolution 10)) (apply max (map bot-x bots)))]
          [min-y (if near-bot (- (bot-y near-bot) (* resolution 10)) (apply min (map bot-y bots)))]
          [max-y (if near-bot (+ (bot-y near-bot) (* resolution 10)) (apply max (map bot-y bots)))]
          [min-z (if near-bot (- (bot-z near-bot) (* resolution 10)) (apply min (map bot-z bots)))]
          [max-z (if near-bot (+ (bot-z near-bot) (* resolution 10)) (apply max (map bot-z bots)))])
      (for* ([x (in-range min-x max-x resolution)]
             [y (in-range min-y max-y resolution)]
             [z (in-range min-z max-z resolution)]
             [b bots])
        (printf "~a of ~a\r" x max-x)
        (let ([key (bot x y z 0)])
          (when (<= (distance key b) (bot-r b))
            (if (hash-has-key? h key)
              (hash-update! h key add1)
              (hash-set! h key 1))))))
    (displayln "")
    h))

(define (sort-point-cloud points)
  (sort (hash->list points) (lambda (a b) (> (cdr a) (cdr b)))))

(let* ([bots (map (lambda (line) (apply bot (map (lambda (n) (string->number n)) (regexp-match* "-?[0-9]+" line)))) input)]
       [largest-radius (first (sort bots (lambda (a b) (> (bot-r a) (bot-r b)))))])

  (printf "part 1: ~a\n" (length (find-bots-in-range largest-radius bots)))

  ; ran with increasing resolution to narrow down the hot spot...

  ;(let* ([point-cloud (counts-bots-in-range bots 10000000)]
         ;[sorted (sort-point-cloud point-cloud)])
    ;(println (take sorted 25)))

  ; => 45858885,16231538,50952636 is closest with 834 bots in range

  ;(let* ([point-cloud (counts-bots-in-range bots 1000001 (bot 45858885 16231538 50952636 0))]
         ;[sorted (sort-point-cloud point-cloud)])
    ;(println (take sorted 25)))

  ; => 36858876,19231541,44952630 is closest with 892 bots in range

  ;(let* ([point-cloud (counts-bots-in-range bots 100009 (bot 36858876 19231541 44952630 0))]
         ;[sorted (sort-point-cloud point-cloud)]
         ;[most-bots (apply max (hash-values point-cloud))]
         ;[matching-points (map car (filter (lambda (p) (>= (cdr p) most-bots)) (hash->list point-cloud)))]
         ;[sorted-points (sort matching-points (lambda (a b) (< (distance (bot 0 0 0 0) a)
                                                               ;(distance (bot 0 0 0 0) b))))])
    ;(println (take sorted-points 10)))

  ; => at least 25 points with 892 bots in range
  ; => 36558849,19331550,44652603 nearest to 0,0,0

  ;(let* ([point-cloud (counts-bots-in-range bots 10000 (bot 36558849 19331550 44652603 0))]
         ;[sorted (sort-point-cloud point-cloud)]
         ;[most-bots (apply max (hash-values point-cloud))]
         ;[matching-points (map car (filter (lambda (p) (>= (cdr p) most-bots)) (hash->list point-cloud)))]
         ;[sorted-points (sort matching-points (lambda (a b) (< (distance (bot 0 0 0 0) a)
                                                               ;(distance (bot 0 0 0 0) b))))])
    ;(println (take sorted 10))
    ;(println (take sorted-points 10)))

  ; => 892 was the highest bot count
  ; => 36558849,19361550,44562603 nearest to 0,0,0

  ;(let* ([point-cloud (counts-bots-in-range bots 1001 (bot 36558849 19361550 44562603 0))]
         ;[sorted (sort-point-cloud point-cloud)]
         ;[most-bots (apply max (hash-values point-cloud))]
         ;[matching-points (map car (filter (lambda (p) (>= (cdr p) most-bots)) (hash->list point-cloud)))]
         ;[sorted-points (sort matching-points (lambda (a b) (< (distance (bot 0 0 0 0) a)
                                                               ;(distance (bot 0 0 0 0) b))))])
    ;(println (take sorted 10))
    ;(println (take sorted-points 10)))

  ; => 892 again!
  ; 36566857,19351540,44556597 nearest to 0,0,0

  ;(let* ([point-cloud (counts-bots-in-range bots 102 (bot 36566857 19351540 44556597 0))]
         ;[sorted (sort-point-cloud point-cloud)]
         ;[most-bots (apply max (hash-values point-cloud))]
         ;[matching-points (map car (filter (lambda (p) (>= (cdr p) most-bots)) (hash->list point-cloud)))]
         ;[sorted-points (sort matching-points (lambda (a b) (< (distance (bot 0 0 0 0) a)
                                                               ;(distance (bot 0 0 0 0) b))))])
    ;(println (take sorted 10))
    ;(println (take sorted-points 10)))

  ; => 901 bots!
  ; => 36567265,19350928,44555883

  ;(let* ([point-cloud (counts-bots-in-range bots 11 (bot 36567265 19350928 44555883 0))]
         ;[sorted (sort-point-cloud point-cloud)]
         ;[most-bots (apply max (hash-values point-cloud))]
         ;[matching-points (map car (filter (lambda (p) (>= (cdr p) most-bots)) (hash->list point-cloud)))]
         ;[sorted-points (sort matching-points (lambda (a b) (< (distance (bot 0 0 0 0) a)
                                                               ;(distance (bot 0 0 0 0) b))))])
    ;(println (take sorted 10))
    ;(println (take sorted-points 10)))

  ; => 906 bots!
  ; => 36567276,19350917,44555839

  (let* ([point-cloud (counts-bots-in-range bots 1 (bot 36567276 19350917 44555839 0))]
         [sorted (sort-point-cloud point-cloud)]
         [most-bots (apply max (hash-values point-cloud))]
         [matching-points (map car (filter (lambda (p) (>= (cdr p) most-bots)) (hash->list point-cloud)))]
         [sorted-points (sort matching-points (lambda (a b) (< (distance (bot 0 0 0 0) a)
                                                               (distance (bot 0 0 0 0) b))))])
    ;(println (take sorted 10))
    ;(println (take sorted-points 10))
    (printf "part 2: ~a\n" (distance (first sorted-points) (bot 0 0 0 0)))))
