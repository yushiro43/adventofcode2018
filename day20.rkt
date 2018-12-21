#lang racket

(require graph)

;(define input "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$")
;(define input "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")
(define input (string-trim (file->string "day20-input.txt")))

(define stack (list))
(define g (undirected-graph (list)))
(define x 0)
(define y 0)
(define min-x 0)
(define max-x 0)
(define min-y 0)
(define max-y 0)

(define directions (string->list (regexp-replace* "\\^|\\$" input "")))

(do ([ip 0])
    ((>= ip (length directions)))
  (let ([dir (list-ref directions ip)])
    ;(println (list ip dir (cons x y)))
    (add-vertex! g (cons x y))
    (set! max-x (max x max-x))
    (set! min-x (min x min-x))
    (set! max-y (max y max-y))
    (set! min-y (min y min-y))
    (cond
      [(eq? dir #\N)
       (add-edge! g (cons x y) (cons x (sub1 y)))
       (set! y (sub1 y))
       (set! ip (add1 ip))]
      [(eq? dir #\S)
       (add-edge! g (cons x y) (cons x (add1 y)))
       (set! y (add1 y))
       (set! ip (add1 ip))]
      [(eq? dir #\E)
       (add-edge! g (cons x y) (cons (add1 x) y))
       (set! x (add1 x))
       (set! ip (add1 ip))]
      [(eq? dir #\W)
       (add-edge! g (cons x y) (cons (sub1 x) y))
       (set! x (sub1 x))
       (set! ip (add1 ip))]
      [(eq? dir (integer->char 40)) ; (
       (set! stack (cons (cons x y) stack))
       (set! ip (add1 ip))]
      [(eq? dir (integer->char 41)) ; )
       (set! stack (drop stack 1))
       (set! ip (add1 ip))]
      [(eq? dir #\|)
       (set! ip (add1 ip))
       (set! x (car (first stack)))
       (set! y (cdr (first stack)))]
      [else (raise dir)])))

;(displayln (make-string (+ 1 (* 2 (add1 (- max-x min-x)))) #\#))
;(for ([y (range min-y (add1 max-y))])
;  (display "#")
;  (for ([x (range min-x (add1 max-x))])
;    (display (if (equal? (cons x y) (cons 0 0)) "X" "."))
;    (display (if (has-edge? g (cons x y) (cons (add1 x) y)) "|" "#")))
;  (display #\newline)
;  (display "#")
;  (for ([x (range min-x (add1 max-x))])
;    (display (if (has-edge? g (cons x y) (cons x (add1 y))) "-" "#"))
;    (display "#"))
;  (display #\newline))

(define paths
  (filter
    identity
    (for*/list ([y (range min-y (add1 max-y))]
                [x (range min-x (add1 max-x))])
      ;(println (list y max-y x max-x))
      (if (equal? (cons 0 0) (cons x y))
        #f
        (fewest-vertices-path g (cons 0 0) (cons x y))))))

(printf "part 1: ~a\n" (sub1 (length (first (sort paths (lambda (a b) (> (length a) (length b))))))))

(printf "part 2: ~a\n" (length (filter (lambda (p) (> (length p) 1000)) paths)))
