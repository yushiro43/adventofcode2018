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
         [outs (make-hash)]
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
            (set! ip (hash-ref mem ip-reg)))))
      ;(println mem)
      (hash-set! outs (hash-ref mem 0) #t)
      (set! ip (add1 ip)))
    (println (hash-keys outs))
    (hash-ref mem 0)))

(define program (file->string "day19-input.txt"))
;(define program "#ip 0\nseti 5 0 1\nseti 6 0 2\naddi 0 1 0\naddr 1 2 3\nsetr 1 0 0\nseti 8 0 4\nseti 9 0 5")

(printf "part 1: ~a\n" (run-program program (make-hash '((0 . 0) (1 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0)))))

; I inspected memory and figured it out the program is finding factors of a large number (864 for part 1 and
; 10551264 for part 2) and adding them together. I plugged the larger number for part 2 into an online calculator
; to get the factors and pasted them below ;-)

(printf "part 2: ~a\n" (+ 1 2 3 4 6 8 12 16 24 32 48 96 131 262 393 524 786 839
                          1048 1572 1678 2096 2517 3144 3356 4192 5034 6288 6712
                          10068 12576 13424 20136 26848 40272 80544 109909 219818
                          329727 439636 659454 879272 1318908 1758544 2637816
                          3517088 5275632 10551264))
