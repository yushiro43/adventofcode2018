#lang racket

(struct group (type index units hit-points immune-to weak-to damage-points damage-type initiative) #:mutable #:transparent)

(define (build-groups input)
  (let ([groups (list)]
        [type '()]
        [index 0]
        [lines (string-split input "\n")])
    (for ([line lines])
      (match line
        ["" '()]
        [(regexp #rx"Immune System") (set! type 'immunesystem) (set! index 0)]
        [(regexp #rx"Infection") (set! type 'infection) (set! index 0)]
        [else
          (set! index (add1 index))
          (let* ([m1 (regexp-match #rx"([0-9]+) units each with ([0-9]+) hit points" line)]
                 [units (string->number (list-ref m1 1))]
                 [hit-points (string->number (list-ref m1 2))]
                 [m2 (regexp-match #rx"immune to ([a-z ,]+)" line)]
                 [immune-to (if m2 (string-split (list-ref m2 1) ", ") #f)]
                 [m3 (regexp-match #rx"weak to ([a-z ,]+)" line)]
                 [weak-to (if m3 (string-split (list-ref m3 1) ", ") #f)]
                 [m4 (regexp-match #rx"with an attack that does ([0-9]+) ([a-z]+) damage at initiative ([0-9]+)" line)]
                 [damage-points (string->number (list-ref m4 1))]
                 [damage-type (list-ref m4 2)]
                 [initiative (string->number (list-ref m4 3))])
            (set! groups (cons
                           (group type index units hit-points immune-to weak-to damage-points damage-type initiative)
                           groups)))]))
    groups))

(define (effective-power g)
  (* (group-units g) (group-damage-points g)))

(define (sort-by-effective-power group)
  (string-append (if (eq? (group-type group) 'immunesystem) "1" "0")
                 (~r (effective-power group) #:min-width 20 #:pad-string "0")
                 (~r (group-initiative group) #:min-width 20 #:pad-string "0")))

(define (sort-groups-for-target-selection groups)
  (sort groups (lambda (a b) (string>? (sort-by-effective-power a) (sort-by-effective-power b)))))

(define (sort-groups-for-attack groups)
  (sort groups (lambda (a b) (> (group-initiative a) (group-initiative b)))))

(define (damage-amount g1 g2)
  (let ([mod (cond
               [(and (group-weak-to g2) (member (group-damage-type g1) (group-weak-to g2))) 2]
               [(and (group-immune-to g2) (member (group-damage-type g1) (group-immune-to g2))) 0]
               [else 1])])
    (* (effective-power g1) mod)))

(define (damage-amount-in-units g1 g2)
  (let ([raw-damage (damage-amount g1 g2)])
    (min (group-units g2)
         (inexact->exact (floor (/ (exact->inexact raw-damage) (group-hit-points g2)))))))

(define (sort-by-damage-amount group target)
  (string-append (~r (damage-amount group target) #:min-width 20 #:pad-string "0")
                 (~r (effective-power target) #:min-width 20 #:pad-string "0")
                 (~r (group-initiative target) #:min-width 20 #:pad-string "0")))

(define (choose-target g other-groups)
  (let ([enemies (filter (lambda (og) (not (eq? (group-type g) (group-type og)))) other-groups)])
    (let* ([sorted (sort enemies (lambda (a b) (string>? (sort-by-damage-amount g a)
                                                         (sort-by-damage-amount g b))))]
           [sorted (filter (lambda (t) (> (damage-amount g t) 0) sorted) sorted)])
      (if (empty? sorted)
        #f
        (if (zero? (damage-amount g (first sorted)))
          #f
          (first sorted))))))

(define (group-description g)
  (format "~a ~a" (group-type g) (group-index g)))

(define (alive g)
  (> (group-units g) 0))

(define (fight groups)
  (let ([groups-sorted (sort-groups-for-target-selection (filter alive groups))]
        [targets (make-hash)])
    ; target selection phase
    (for ([g groups-sorted])
      (let* ([other-groups (filter alive groups-sorted)]
             [target-group (choose-target g other-groups)])
        (when target-group
          ;(printf "~a would deal ~a ~a damage\n" (group-description g) (group-description target-group) (damage-amount g target-group))
          (hash-set! targets (group-description g) target-group)
          (set! groups-sorted (sort-groups-for-target-selection (remove target-group groups-sorted))))))
    ; attack phase
    (let ([groups-sorted (sort-groups-for-attack (filter alive groups))])
      (for ([g groups-sorted])
        (when (alive g)
          (let ([target (hash-ref targets (group-description g) #f)])
            (when target
              (let* ([units-killed (damage-amount-in-units g target)])
                (when (> units-killed 0)
                  ;(printf "~a attacks ~a, killing ~a\n" (group-description g) (group-description target) units-killed)
                  (set-group-units! target (- (group-units target) units-killed)))))))))))

(define (display-alive-groups groups)
  (for ([g (filter alive groups)])
    (println g)))

(define (fight-until-game-over groups)
  (let ([immunesystem (foldl + 0 (map group-units (filter (lambda (g) (eq? (group-type g) 'immunesystem)) groups)))]
        [infection (foldl + 0 (map group-units (filter (lambda (g) (eq? (group-type g) 'infection)) groups)))])
    ;(printf "immune system units: ~a\ninfection units:     ~a\n\n" immunesystem infection)
    ;(display-alive-groups groups)
    (if (or (zero? immunesystem) (zero? infection))
      groups
      (begin
        (fight groups)
        (fight-until-game-over groups)))))

(define (boost-group g boost)
  (set-group-damage-points! g (+ (group-damage-points g) boost)))

(define input (file->string "day24-input.txt"))
(define sample-input "Immune System:\n17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2\n989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3\n\nInfection:\n801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1\n4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4")

(let ([groups (build-groups input)])
  (fight-until-game-over groups)
  (printf "part 1: ~a\n" (foldl + 0 (map group-units (filter alive groups)))))

(let ([groups (build-groups input)]
      ;[boost-ammount (string->number (vector-ref (current-command-line-arguments) 0))])
      [boost-ammount 131])
  (for ([g groups]
        #:when (eq? (group-type g) 'immunesystem))
    (boost-group g boost-ammount))
  (fight-until-game-over groups)
  (if (findf (lambda (g) (eq? (group-type g) 'infection)) (filter alive groups))
    (printf "immune system is dead :-(\n")
    (printf "part 2: ~a\n" (foldl + 0 (map group-units (filter alive groups))))))
