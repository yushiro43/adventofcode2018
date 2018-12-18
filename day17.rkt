#lang racket

(struct state (clay water wet roots min-x max-x min-y max-y lowest-drop top-most) #:mutable #:transparent)

(struct leaf (x y up left down right stuck) #:mutable #:transparent)

(define (build-state input)
  (let ([the-state (state (make-hash) (make-hash) (make-hash) (list (leaf 500 0 #f #f #f #f #f)) #f #f #f #f (cons 500 0) 0)]
        [lines (string-split input "\n")])
    (hash-set! (state-water the-state) (cons 500 0) (first (state-roots the-state)))
    (for ([line lines])
      (let* ([xx (map string->number (regexp-match* "[0-9]+" (second (regexp-match #rx"x=([0-9]+\\.\\.[0-9]+|[0-9]+)" line))))]
             [yy (map string->number (regexp-match* "[0-9]+" (second (regexp-match #rx"y=([0-9]+\\.\\.[0-9]+|[0-9]+)" line))))]
             [x1 (first xx)]
             [x2 (last xx)]
             [y1 (first yy)]
             [y2 (last yy)]
             [xx (range x1 (add1 x2))]
             [yy (range y1 (add1 y2))]
             [min-x (state-min-x the-state)]
             [max-x (state-max-x the-state)]
             [min-y (state-min-y the-state)]
             [max-y (state-max-y the-state)])
        (when (or (not min-x) (< x1 min-x)) (set-state-min-x! the-state x1))
        (when (or (not max-x) (> x2 max-x)) (set-state-max-x! the-state x2))
        (when (or (not min-y) (< y1 min-y)) (set-state-min-y! the-state y1))
        (when (or (not max-y) (> y2 max-y)) (set-state-max-y! the-state y2))
        (for* ([x xx]
               [y yy])
          (hash-set! (state-clay the-state) (cons x y) #\#))))
    the-state))

(define (display-map the-state [start-x 450] [width 100] [start-y 0] [height 35])
  (loop-over-map
    the-state
    (lambda (x y char last-col-on-line)
      (when (and (>= x start-x) (<= x (+ start-x width)) (>= y start-y) (<= y (+ start-y height))) (display char))
      (when (and (>= y start-y) (<= y (+ start-y height)) last-col-on-line) (display #\newline)))))

(define (display-map-to-file the-state path)
  (display-to-file "" path #:exists 'replace)
  (loop-over-map
    the-state
    (lambda (x y char last-col-on-line)
      (display-to-file char path #:exists 'append)
      (when last-col-on-line (display-to-file #\newline path #:exists 'append)))))

(define (loop-over-map the-state fn [reverse #f])
  (let* ([xx (map car (hash-keys (state-clay the-state)))]
         [yy (map cdr (hash-keys (state-clay the-state)))]
         [min-x (sub1 (apply min xx))]
         [max-x (add1 (apply max xx))]
         [min-y 0]
         [max-y (add1 (apply max yy))])
    (for* ([y (if reverse (range max-y (sub1 min-y) -1) (range min-y (add1 max-y)))]
           [x (if reverse (range max-x (sub1 min-x) -1) (range min-x (add1 max-x)))])
      (let ([char (if (and (eq? x 500) (eq? y 0))
                    #\+
                    (or
                      (let ([l (hash-ref (state-water the-state) (cons x y) #f)]) (cond [(not l) #f]
                                                                                        [(leaf-stuck l) #\x]
                                                                                        [else #\~]))
                      (hash-ref (state-clay the-state) (cons x y) #\.)))])
        (fn x y char (eq? x max-x))))))

(define (sort-by-y lst)
  (sort lst (lambda (a b) (> (cdr a) (cdr b)))))

(define update-roots-every 500)
(define update-roots-backup-by 100)
(define update-roots-max-streams 3)

(define (update-roots the-state)
  (let* ([y (max 0 (- (state-top-most the-state) update-roots-backup-by))]
         [roots (filter-map (lambda (x) (hash-ref (state-water the-state) (cons x y) #f))
                            (range (state-min-x the-state) (add1 (state-max-x the-state))))])
    ;(println (map (lambda (r) (cons (leaf-x r) (leaf-y r))) roots))
    (when (<= (length roots) update-roots-max-streams)
      (set-state-roots! the-state roots))))

(define (drop-water the-state start-x start-y iteration)
  (when (and (>= iteration update-roots-every) (zero? (modulo iteration update-roots-every)))
    (update-roots the-state))
  (set-state-top-most! the-state 0)
  (for ([root (state-roots the-state)])
    (each-leaf
      root
      (curry process-leaf the-state))))

(define (process-leaf the-state l [backtracking #f])
  (when l
    (let ([water (state-water the-state)]
          [wet (state-wet the-state)]
          [x (leaf-x l)]
          [y (leaf-y l)])
      (hash-set! wet (cons x y) #t)
      (when (> y (cdr (state-lowest-drop the-state)))
        (set-state-lowest-drop! the-state (cons x y)))
      (if (blocked-below? l x y the-state)
        (begin
          (if (and (blocked-left? l x y the-state)
                   (blocked-right? l x y the-state))
            ; backtrack to find a branching point
            (begin
              (when (not backtracking) (set-leaf-stuck! l #t))
              (let ([left (leaf-left l)]
                    [right (leaf-right l)])
                (when (and (or (not left) (leaf-stuck left))
                           (or (not right) (leaf-stuck right)))
                  (process-leaf the-state (leaf-up l) 'backtracking))))
            (begin
              (grow-left l x y the-state)
              (grow-right l x y the-state))))
        (grow-down l x y the-state)))))

(define (grow-left l x y the-state)
  (when (not (blocked-left? l x y the-state))
    (let ([new-leaf (leaf (sub1 x) y l #f #f l #f)])
      (when (> y (state-top-most the-state)) (set-state-top-most! the-state y))
      (hash-set! (state-water the-state) (cons (leaf-x new-leaf) (leaf-y new-leaf)) new-leaf)
      (set-leaf-left! l new-leaf))))

(define (grow-right l x y the-state)
  (when (not (blocked-right? l x y the-state))
    (let ([new-leaf (leaf (add1 x) y l l #f #f #f)])
      (when (> y (state-top-most the-state)) (set-state-top-most! the-state y))
      (hash-set! (state-water the-state) (cons (leaf-x new-leaf) (leaf-y new-leaf)) new-leaf)
      (set-leaf-right! l new-leaf))))

(define (grow-down l x y the-state)
  (let ([water (state-water the-state)])
    (when (and (<= (add1 y) (state-max-y the-state))
               (not (and (hash-ref water (cons x (+ y 1)) #f)
                         (hash-ref water (cons x (+ y 2)) #f))))
      (let ([new-leaf (leaf x (add1 y) l #f #f #f #f)])
        (when (> (add1 y) (state-top-most the-state)) (set-state-top-most! the-state (add1 y)))
        (hash-set! water (cons (leaf-x new-leaf) (leaf-y new-leaf)) new-leaf)
        (set-leaf-down! l new-leaf)))))

(define (each-leaf node fn [seen (make-hash)])
  (let ([left (leaf-left node)]
        [down (leaf-down node)]
        [right (leaf-right node)]
        [stuck (leaf-stuck node)])
    (when (not stuck)
      (hash-set! seen node #t)
      (if (or (and (or (not left) (leaf-stuck left))
                   (or (not down) (leaf-stuck down))
                   (or (not right) (leaf-stuck right)))
              (and (or (not left) (leaf-stuck left))
                   (or (not down) (leaf-stuck down))
                   (hash-has-key? seen right))
              (and (hash-has-key? seen left)
                   (or (not down) (leaf-stuck down))
                   (or (not right) (leaf-stuck right))))
        (fn node)
        (begin
          (when (and left (not (hash-has-key? seen left)))
            (each-leaf left fn seen))
          (when down
            (each-leaf down fn seen))
          (when (and right (not (hash-has-key? seen right)))
            (each-leaf right fn seen)))))))

(define (blocked-below? l x y the-state)
  (let ([below (hash-ref (state-water the-state) (cons x (add1 y)) #f)])
    (or (and below (leaf-stuck below))
        (leaf-down l)
        (hash-ref (state-clay the-state) (cons x (add1 y)) #f))))

(define (blocked-left? l x y the-state)
  (or (leaf-left l)
      (hash-ref (state-clay the-state) (cons (sub1 x) y) #f)))

(define (blocked-right? l x y the-state)
  (or (leaf-right l)
      (hash-ref (state-clay the-state) (cons (add1 x) y) #f)))

(define (wet-points-in-range the-state)
  (for/list ([point (hash-keys (state-wet the-state))]
             #:when (and (>= (cdr point) (state-min-y the-state))
                         (<= (cdr point) (state-max-y the-state))))
    point))

;(let ([the-state (build-state "x=495, y=2..7
;y=7, x=495..501
;x=501, y=3..7
;x=498, y=2..4
;x=506, y=1..2
;x=498, y=10..13
;x=504, y=10..13
;y=13, x=498..504")])
;  (for ([i (range 100)])
;    (drop-water the-state 500 0 i)
;    (display-map the-state)
;    (sleep 0.05))
;  (println (length (wet-points-in-range the-state)))
;  )

;(let ([the-state (build-state "x=495, y=2..7
;y=7, x=495..501
;x=501, y=2..7
;x=498, y=2..4
;x=506, y=1..2
;x=493, y=9..13
;x=504, y=10..13
;y=13, x=493..504")])
;  (for ([i (range 120)])
;    (display-map the-state)
;    (drop-water the-state 500 0 i)
;    (sleep 0.05))
;  (println (length (wet-points-in-range the-state)))
;  )

;(let ([the-state (build-state "x=495, y=2..7
;y=7, x=495..501
;x=501, y=2..7
;x=498, y=2..4
;x=506, y=1..2
;x=493, y=9..13
;x=504, y=10..13
;y=13, x=493..504
;y=20, x=493..508
;x=493, y=18..20
;x=508, y=18..20")])
;  (for ([i (range 200)])
;    (display-map the-state)
;    (drop-water the-state 500 0 i)
;    (sleep 0.01))
;  (println (length (wet-points-in-range the-state)))
;  )

(let ([the-state (build-state (file->string "day17-input.txt"))])
  (call/cc (lambda (done)
    (let ([last-wet-count -1])
      (for ([i (in-naturals)])
        (when (zero? (modulo i 3000))
          (display-map-to-file the-state "day17-map.txt")
          (let ([wet-count (length (wet-points-in-range the-state))])
            (if (= wet-count last-wet-count)
              (done)
              (set! last-wet-count wet-count))))
        (let ([center-x (car (state-lowest-drop the-state))]
              [center-y (cdr (state-lowest-drop the-state))])
          (when (zero? (modulo i 250))
            (display-map the-state (- center-x 70) 140 (- center-y 15) 34))
          (display (string-append (number->string i) "\r"))
          (drop-water the-state 500 0 i)))))))
