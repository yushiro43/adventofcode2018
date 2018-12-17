#lang racket

(define instructions (hash
                       'addr (lambda (a b c mem) (hash-set! mem c (+ (hash-ref mem a) (hash-ref mem b))))
                       'addi (lambda (a b c mem) (hash-set! mem c (+ (hash-ref mem a) b)))
                       'mulr (lambda (a b c mem) (hash-set! mem c (* (hash-ref mem a) (hash-ref mem b))))
                       'muli (lambda (a b c mem) (hash-set! mem c (* (hash-ref mem a) b)))
                       'banr (lambda (a b c mem) (hash-set! mem c (bitwise-and (hash-ref mem a) (hash-ref mem b))))
                       'bani (lambda (a b c mem) (hash-set! mem c (bitwise-and (hash-ref mem a) b)))
                       'borr (lambda (a b c mem) (hash-set! mem c (bitwise-ior (hash-ref mem a) (hash-ref mem b))))
                       'bori (lambda (a b c mem) (hash-set! mem c (bitwise-ior (hash-ref mem a) b)))
                       'setr (lambda (a _ c mem) (hash-set! mem c (hash-ref mem a)))
                       'seti (lambda (a _ c mem) (hash-set! mem c a))
                       'gtir (lambda (a b c mem) (hash-set! mem c (if (> a (hash-ref mem b)) 1 0)))
                       'gtri (lambda (a b c mem) (hash-set! mem c (if (> (hash-ref mem a) b) 1 0)))
                       'gtrr (lambda (a b c mem) (hash-set! mem c (if (> (hash-ref mem a) (hash-ref mem b)) 1 0)))
                       'eqir (lambda (a b c mem) (hash-set! mem c (if (= a (hash-ref mem b)) 1 0)))
                       'eqri (lambda (a b c mem) (hash-set! mem c (if (= (hash-ref mem a) b) 1 0)))
                       'eqrr (lambda (a b c mem) (hash-set! mem c (if (= (hash-ref mem a) (hash-ref mem b)) 1 0)))))

(define input (file->string "day16-input.txt"))
(match-define (list samples program) (string-split input "\n\n\n"))
(set! samples (string-split samples "\n\n"))

(define (match-instruction before input after name instruction)
  (let ([mem (make-hash (for/list ([i (in-naturals)] [val before]) (cons i val)))])
    (apply instruction (append (drop input 1) (list mem)))
    (equal? (for/list ([r (range 4)]) (hash-ref mem r))
            after)))

(define (matching-instructions sample instructions)
  (match-let ([(list before input after) (string-split sample "\n")])
    (let* ([before (map string->number (regexp-match* "[0-9]+" before))]
           [input (map string->number (string-split input))]
           [after (map string->number (regexp-match* "[0-9]+" after))]
           [matching
             (for/list ([(name instr) (in-hash instructions)]
                        #:when (match-instruction before input after name instr))
               name)])
      matching)))

(define (find-instruction-opcodes samples avail-instructions [found (hash)])
  (if (hash-empty? avail-instructions)
    found
    (let ([sample (findf (lambda (s) (= (length (matching-instructions s avail-instructions)) 1)) samples)])
      (let* ([opcode (string->number (first (string-split (second (string-split sample "\n")))))]
             [remaining-samples (remove sample samples)]
             [instr-name (first (matching-instructions sample avail-instructions))]
             [remaining-instructions (hash-remove avail-instructions instr-name)])
        (find-instruction-opcodes remaining-samples remaining-instructions (hash-set found opcode (hash-ref instructions instr-name)))))))

(printf "part 1: ~a\n"
        (length 
          (filter
            (lambda (s) (>= (length (matching-instructions s instructions)) 3))
            samples)))

(define instructions-with-opcodes (find-instruction-opcodes samples instructions))

(printf "part 2: ~a\n"
  (let ([mem (make-hash '((0 . 0) (1 . 0) (2 . 0) (3 . 0)))])
    (for ([instruction (string-split program "\n")])
      (match-let ([(list opcode a b c) (map string->number (regexp-match* "[0-9]+" instruction))])
        (let ([instr-fn (hash-ref instructions-with-opcodes opcode)])
          (apply instr-fn (list a b c mem)))))
    (hash-ref mem 0)))
