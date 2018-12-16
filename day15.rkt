#lang racket

(require graph)

(struct unit (type x y attack-power hit-points alive) #:mutable #:transparent)

(define elf-attack-power 3)
;(define elf-attack-power 25)
(define goblin-attack-power 3)

(define (build-state input)
  (let ([units-xy (make-hash)]
        [units (list)]
        [the-map (make-hash)])
    (for ([line (string-split input "\n")]
          [y (in-naturals)])
      (for ([char (string->list line)]
            [x (in-naturals)])
        (cond [(or (eq? char #\#)
                   (eq? char #\.))
               (hash-set! the-map (cons x y) char)]
              [(or (eq? char #\G)
                   (eq? char #\E))
               (let ([u (unit char x y (if (eq? char #\E) elf-attack-power goblin-attack-power) 200 #t)])
                 (hash-set! the-map (cons x y) u)
                 (set! units (cons u units)))])))
    (values units the-map (build-graph the-map))))

(define (one-away? p1 p2)
  (let ([x1 (car p1)]
        [y1 (cdr p1)]
        [x2 (car p2)]
        [y2 (cdr p2)])
    (or
      (and (eq? x1 x2) (eq? 1 (abs (- y1 y2))))
      (and (eq? y1 y2) (eq? 1 (abs (- x1 x2)))))))

(define (build-graph the-map)
  (undirected-graph
    (for*/list ([(p1 c1) (in-hash the-map)]
                [(p2 c2) (in-hash the-map)]
                #:when (and (eq? c1 #\.)
                            (eq? c2 #\.)
                            (one-away? p1 p2)))
      (list p1 p2))))

(define (units-in-read-order units)
  (sort (sort units (lambda (a b) (< (unit-x a) (unit-x b))))
        (lambda (a b) (< (unit-y a) (unit-y b)))))

(define (adjacent-points x y the-map)
  (let ([possible (list (cons       x  (sub1 y))
                        (cons (sub1 x)       y )
                        (cons (add1 x)       y )
                        (cons       x  (add1 y)))])
    (filter (lambda (p) (eq? #\. (hash-ref the-map p #f))) possible)))

(define (get-targets cur-unit units the-map)
  (let ([other-units (units-in-read-order (filter
                                            (lambda (u) (not (eq? (unit-type u) (unit-type cur-unit))))
                                            units))])
    (apply append (for/list ([u other-units])
                    (let* ([x (unit-x u)]
                           [y (unit-y u)])
                      (adjacent-points x y the-map))))))

(define (reachable-target-paths cur-unit target the-map graph)
  (filter identity
          (for/list ([start (adjacent-points (unit-x cur-unit) (unit-y cur-unit) the-map)])
            (cond [(equal? start target)
                   (list target)]
                  [(and (has-vertex? graph start) (has-vertex? graph target))
                   (fewest-vertices-path graph start target)]
                  [else #f]))))

(define (reachable-targets cur-unit units the-map graph)
  (let ([targets (get-targets cur-unit units the-map)])
    (for/list ([target targets]
               #:when (> (length (reachable-target-paths cur-unit target the-map graph)) 0))
      target)))

(define (target-sort cur-unit the-map graph a b)
  (< (apply min (map length (reachable-target-paths cur-unit a the-map graph)))
     (apply min (map length (reachable-target-paths cur-unit b the-map graph)))))

(define (choose-target-point cur-unit units the-map graph)
  (let ([targets (sort (reachable-targets cur-unit units the-map graph)
                       (curry target-sort cur-unit the-map graph))])
    (if (empty? targets)
      #f
      (first targets))))

(define (choose-path-to-target cur-unit target-point the-map graph)
  (let ([paths (reachable-target-paths cur-unit target-point the-map graph)])
    (if (empty? paths)
      #f
      (let* ([shortest (first (sort paths (lambda (a b) (< (length a) (length b)))))]
             [paths (filter (lambda (p) (eq? (length p) (length shortest))) paths)])
        (first (sort (sort paths (lambda (a b) (< (car (first a)) (car (first b)))))
                     (lambda (a b) (< (cdr (first a)) (cdr (first b))))))))))

(define (add-point-to-graph graph x y the-map)
  (add-vertex! graph (cons x y))
  (for ([point (adjacent-points x y the-map)])
    (add-edge! graph (cons x y) point)))

(define (move-unit-to unit x y the-map graph)
  (let ([old-x (unit-x unit)]
        [old-y (unit-y unit)])
    (set-unit-x! unit x)
    (set-unit-y! unit y)
    (hash-set! the-map (cons old-x old-y) #\.)
    (hash-set! the-map (cons x y) unit)
    (remove-vertex! graph (cons x y))
    (add-point-to-graph graph old-x old-y the-map)))

(define (move-unit unit units the-map graph)
  (let* ([target (choose-target-point unit units the-map graph)]
         [path (choose-path-to-target unit target the-map graph)])
    (when path
      (let ([next-point (first path)])
        (move-unit-to unit (car next-point) (cdr next-point) the-map graph)))))

(define (attack-enemy cur-unit enemies the-map graph)
  (let* ([sorted (sort enemies (lambda (a b) (< (unit-hit-points a) (unit-hit-points b))))]
         [hit-points (unit-hit-points (first sorted))]
         [candidates (filter (lambda (u) (eq? (unit-hit-points u) hit-points)) sorted)]
         [enemy (first (units-in-read-order candidates))])
    (set-unit-hit-points! enemy (- (unit-hit-points enemy) (unit-attack-power cur-unit)))
    (when (<= (unit-hit-points enemy) 0)
      ;(when (eq? (unit-type enemy) #\E) (raise "elf die, bad"))
      (set-unit-alive! enemy #f)
      (set-unit-hit-points! enemy 0)
      (hash-set! the-map (cons (unit-x enemy) (unit-y enemy)) #\.)
      (add-point-to-graph graph (unit-x enemy) (unit-y enemy) the-map))))

(define (adjacent-units cur-unit units)
  (for/list ([unit units]
             #:when (one-away? (cons (unit-x cur-unit) (unit-y cur-unit))
                               (cons (unit-x unit) (unit-y unit))))
    unit))

(define (perform-round units the-map graph [game-over #f])
  (for ([unit (units-in-read-order units)]
        #:when (unit-alive unit))
    (let* ([enemies (filter (lambda (u) (not (eq? (unit-type u) (unit-type unit)))) units)]
           [adjacent (adjacent-units unit enemies)])
      ; no more enemies, game over!
      (when (and (empty? enemies) game-over) (game-over))
      ; no enemies next to us, move closer
      (when (empty? adjacent)
        (move-unit unit units the-map graph))
      ; check for nearby enemies again
      (let ([adjacent (adjacent-units unit enemies)])
        (when (not (empty? adjacent))
          ; adjacent enemy, attack!
          (attack-enemy unit adjacent the-map graph)))
    (set! units (filter unit-alive units))))
  (values units graph))

(define (play-until-defeat units the-map graph)
  (let ([total-rounds 0])
    (call/cc (lambda (game-over)
               (for ([round (in-naturals)])
                 ;(println round)
                 ;(display-map the-map)
                 (set!-values (units graph)
                              (perform-round units the-map graph game-over))
                 (set! total-rounds (add1 total-rounds)))))
    (values units graph total-rounds)))

(define (map->string the-map)
  (let ([width (add1 (apply max (map car (hash-keys the-map))))]
        [height (add1 (apply max (map cdr (hash-keys the-map))))])
    (string-join (for/list ([y (in-range height)])
                   (list->string (append (for/list ([x (in-range width)])
                                           (let ([c (hash-ref the-map (cons x y))])
                                             (if (unit? c)
                                               (unit-type c)
                                               c)))
                                         (list #\newline))))
                 "")))

(define (display-map the-map)
  (displayln (map->string the-map)))

(require rackunit)

(let*-values ([(units the-map graph)
              (build-state "#######\n#E..G.#\n#...#.#\n#.G.#G#\n#######")]
              [(elf) (first (units-in-read-order units))]
              [(goblin) (second (units-in-read-order units))])

  ;  Targets:      In range:     Reachable:    Nearest:      Chosen:
  ;  #######       #######       #######       #######       #######
  ;  #E..G.#       #E.?G?#       #E.@G.#       #E.!G.#       #E.+G.#
  ;  #...#.#  -->  #.?.#?#  -->  #.@.#.#  -->  #.!.#.#  -->  #...#.#
  ;  #.G.#G#       #?G?#G#       #@G@#G#       #!G.#G#       #.G.#G#
  ;  #######       #######       #######       #######       #######

  (test-case "get-targets"
             (check-equal? (get-targets elf units the-map)
                           '((3 . 1) (5 . 1) (2 . 2) (1 . 3) (3 . 3) (5 . 2)))
             (check-equal? (get-targets goblin units the-map)
                           '((2 . 1) (1 . 2))))
  
  (test-case "reachable-targets"
             (check-equal? (reachable-targets elf units the-map graph)
                           '((3 . 1) (2 . 2) (1 . 3) (3 . 3)))
             (check-equal? (reachable-targets goblin units the-map graph)
                           '((2 . 1) (1 . 2))))

  (test-case "choose-target-point"
             (check-equal? (choose-target-point elf units the-map graph)
                           '(3 . 1))
             (check-equal? (choose-target-point goblin units the-map graph)
                           '(2 . 1))
             (let*-values ([(units the-map graph)
                            (build-state "#########\n#.......#\n#.......#\n#.......#\n#G..E...#\n#.......#\n#.......#\n#.......#\n#########")]
                           [(goblin) (first (units-in-read-order units))]
                           [(elf) (second (units-in-read-order units))])
               (check-equal? (choose-target-point goblin units the-map graph)
                             '(3 . 4))))
  )

(let*-values ([(units the-map graph)
              (build-state "#######\n#.E...#\n#.....#\n#...G.#\n#######")]
              [(elf) (first (units-in-read-order units))]
              [(target-point) (choose-target-point elf units the-map graph)])

  ;  In range:     Nearest:      Chosen:       Distance:     Step:
  ;  #######       #######       #######       #######       #######
  ;  #.E...#       #.E...#       #.E...#       #4E212#       #..E..#
  ;  #...?.#  -->  #...!.#  -->  #...+.#  -->  #32101#  -->  #.....#
  ;  #..?G?#       #..!G.#       #...G.#       #432G2#       #...G.#
  ;  #######       #######       #######       #######       #######

  (test-case "choose-path-to-target"
             (check-equal? (choose-path-to-target elf target-point the-map graph)
                           '((3 . 1) (4 . 1) (4 . 2)))))

(let*-values ([(units the-map graph)
              (build-state "#########\n#G..G..G#\n#.......#\n#.......#\n#G..E..G#\n#.......#\n#.......#\n#G..G..G#\n#########")])

  ;   round 0      round 1      round 2      round 3
  ;  #########    #########    #########    #########
  ;  #G..G..G#    #.G...G.#    #..G.G..#    #.......#
  ;  #.......#    #...G...#    #...G...#    #..GGG..#
  ;  #.......#    #...E..G#    #.G.E.G.#    #..GEG..#
  ;  #G..E..G# -> #.G.....# -> #.......# -> #G..G...#
  ;  #.......#    #.......#    #G..G..G#    #......G#
  ;  #.......#    #G..G..G#    #.......#    #.......#
  ;  #G..G..G#    #.......#    #.......#    #.......#
  ;  #########    #########    #########    #########

  (test-case "perform-round"
             (set!-values (units graph) (perform-round units the-map graph))
             (check-equal? (map->string the-map)
                           "#########\n#.G...G.#\n#...G...#\n#...E..G#\n#.G.....#\n#.......#\n#G..G..G#\n#.......#\n#########\n")
             (set!-values (units graph) (perform-round units the-map graph))
             (check-equal? (map->string the-map)
                           "#########\n#..G.G..#\n#...G...#\n#.G.E.G.#\n#.......#\n#G..G..G#\n#.......#\n#.......#\n#########\n")
             (set!-values (units graph) (perform-round units the-map graph))
             (check-equal? (map->string the-map)
                           "#########\n#.......#\n#..GGG..#\n#..GEG..#\n#G..G...#\n#......G#\n#.......#\n#.......#\n#########\n")))

(let*-values ([(units the-map graph)
              (build-state "#######\n#.G...#\n#...EG#\n#.#.#G#\n#..G#E#\n#.....#\n#######")])

  ;  #######
  ;  #.G...#
  ;  #...EG#
  ;  #.#.#G#
  ;  #..G#E#
  ;  #.....#
  ;  #######

  (test-case "play-until-defeat"
             (let*-values ([(units graph rounds) (play-until-defeat units the-map graph)]
                           [(hit-sum) (foldl + 0 (map unit-hit-points units))]
                           [(outcome) (* rounds hit-sum)])
               (check-eq? rounds 47)
               (check-eq? hit-sum 590)
               (check-eq? outcome 27730))))

(let*-values ([(units the-map graph)
              (build-state "#######\n#E.G#.#\n#.#G..#\n#G.#.G#\n#G..#.#\n#...E.#\n#######")])

  ;  #######       #######   
  ;  #E.G#.#       #G.G#.#   G(200), G(98)
  ;  #.#G..#       #.#G..#   G(200)
  ;  #G.#.G#  -->  #..#..#   
  ;  #G..#.#       #...#G#   G(95)
  ;  #...E.#       #...G.#   G(200)
  ;  #######       #######   

  (test-case "play-until-defeat"
             (let*-values ([(units graph rounds) (play-until-defeat units the-map graph)]
                           [(hit-sum) (foldl + 0 (map unit-hit-points units))]
                           [(outcome) (* rounds hit-sum)])
               (check-eq? rounds 35)
               (check-eq? hit-sum 793)
               (check-eq? outcome 27755))))

(let*-values ([(units the-map graph)
              (build-state "#######\n#.E...#\n#.#..G#\n#.###.#\n#E#G#G#\n#...#G#\n#######")])

  ;  #######       #######   
  ;  #.E...#       #.....#   
  ;  #.#..G#       #.#G..#   G(200)
  ;  #.###.#  -->  #.###.#   
  ;  #E#G#G#       #.#.#.#   
  ;  #...#G#       #G.G#G#   G(98), G(38), G(200)
  ;  #######       #######

  (test-case "play-until-defeat"
             (let*-values ([(units graph rounds) (play-until-defeat units the-map graph)]
                           [(hit-sum) (foldl + 0 (map unit-hit-points units))]
                           [(outcome) (* rounds hit-sum)])
               (check-eq? rounds 54)
               (check-eq? hit-sum 536)
               (check-eq? outcome 28944))))

(let*-values ([(units the-map graph)
              (build-state "#########\n#G......#\n#.E.#...#\n#..##..G#\n#...##..#\n#...#...#\n#.G...G.#\n#.....G.#\n#########")])

  ;  #########    #########   
  ;  #G......#    #.G.....#   G(137)
  ;  #.E.#...#    #G.G#...#   G(200), G(200)
  ;  #..##..G#    #.G##...#   G(200)
  ;  #...##..# -> #...##..#   
  ;  #...#...#    #.G.#...#   G(200)
  ;  #.G...G.#    #.......#   
  ;  #.....G.#    #.......#   
  ;  #########    #########   

  (test-case "play-until-defeat"
             (let*-values ([(units graph rounds) (play-until-defeat units the-map graph)]
                           [(hit-sum) (foldl + 0 (map unit-hit-points units))]
                           [(outcome) (* rounds hit-sum)])
               (check-eq? rounds 20)
               (check-eq? hit-sum 937)
               (check-eq? outcome 18740))))

(let*-values ([(units the-map graph)
              (build-state "#######\n#E..EG#\n#.#G.E#\n#E.##E#\n#G..#.#\n#..E#.#\n#######")])

  ;  #######       #######   
  ;  #E..EG#       #.E.E.#   E(164), E(197)
  ;  #.#G.E#       #.#E..#   E(200)
  ;  #E.##E#  -->  #E.##.#   E(98)
  ;  #G..#.#       #.E.#.#   E(200)
  ;  #..E#.#       #...#.#   
  ;  #######       #######   

  (test-case "broken play-until-defeat"
             (let*-values ([(units graph rounds) (play-until-defeat units the-map graph)]
                           [(hit-sum) (foldl + 0 (map unit-hit-points units))]
                           [(outcome) (* rounds hit-sum)])
               (check-eq? rounds 46)
               (check-eq? hit-sum 859)
               (check-eq? outcome 39514))))

#;(let*-values ([(units the-map graph) (build-state (file->string "day15-input.txt"))]
              [(units graph rounds) (play-until-defeat units the-map graph)]
              [(hit-sum) (foldl + 0 (map unit-hit-points units))]
              [(outcome) (* rounds hit-sum)])
  (println outcome))
