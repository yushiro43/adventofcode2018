#lang racket

(define instr-fn (hash
                   "addr" (lambda (a b c mem) (hash-set! mem c (+ (hash-ref mem a) (hash-ref mem b))))
                   "addi" (lambda (a b c mem) (hash-set! mem c (+ (hash-ref mem a) b)))
                   "mulr" (lambda (a b c mem) (hash-set! mem c (* (hash-ref mem a) (hash-ref mem b))))
                   "muli" (lambda (a b c mem) (hash-set! mem c (* (hash-ref mem a) b)))
                   "banr" (lambda (a b c mem) (hash-set! mem c (bitwise-and (hash-ref mem a) (hash-ref mem b))))
                   "bani" (lambda (a b c mem) (hash-set! mem c (bitwise-and (hash-ref mem a) b)))
                   "borr" (lambda (a b c mem) (hash-set! mem c (bitwise-ior (hash-ref mem a) (hash-ref mem b))))
                   "bori" (lambda (a b c mem) (hash-set! mem c (bitwise-ior (hash-ref mem a) b)))
                   "setr" (lambda (a _ c mem) (hash-set! mem c (hash-ref mem a)))
                   "seti" (lambda (a _ c mem) (hash-set! mem c a))
                   "gtir" (lambda (a b c mem) (hash-set! mem c (if (> a (hash-ref mem b)) 1 0)))
                   "gtri" (lambda (a b c mem) (hash-set! mem c (if (> (hash-ref mem a) b) 1 0)))
                   "gtrr" (lambda (a b c mem) (hash-set! mem c (if (> (hash-ref mem a) (hash-ref mem b)) 1 0)))
                   "eqir" (lambda (a b c mem) (hash-set! mem c (if (= a (hash-ref mem b)) 1 0)))
                   "eqri" (lambda (a b c mem) (hash-set! mem c (if (= (hash-ref mem a) b) 1 0)))
                   "eqrr" (lambda (a b c mem) (hash-set! mem c (if (= (hash-ref mem a) (hash-ref mem b)) 1 0)))))

(define (run-program program mem)
  (let* ([program-lines (string-split program "\n")]
         [instructions (filter (lambda (line) (not (regexp-match "^#" line))) program-lines)]
         [directives (filter (lambda (line) (regexp-match "^#" line)) program-lines)]
         [ip-reg -1]
         [ip 0])
    (for ([directive directives])
      (set! ip-reg (string->number (first (regexp-match "[0-9]" directive)))))
    (do ([instruction (list-ref instructions ip)
                      (if (and (>= ip 0) (<= ip (sub1 (length instructions))))
                        (list-ref instructions ip)
                        #f)])
        ((not instruction))
      (let ([op (first (regexp-match "^[a-z]+" instruction))]
            [args (map string->number (regexp-match* "[0-9]+" instruction))])
        (match-let ([(list a b c) args])
          (let ([instr-fn (hash-ref instr-fn op)])
            (hash-set! mem ip-reg ip)
            (apply instr-fn (list a b c mem))
            (println (list ip instruction mem))
            (set! ip (hash-ref mem ip-reg)))))
      (set! ip (add1 ip)))
    (hash-ref mem 0)))

(define program (file->string "day21-input.txt"))

(printf "part 1: ~a\n" (run-program program (make-hash (list (cons 0 (string->number (vector-ref (current-command-line-arguments) 0)))
                                                             (cons 1 0)
                                                             (cons 2 0)
                                                             (cons 3 0)
                                                             (cons 4 0)
                                                             (cons 5 0)))))
