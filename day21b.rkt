#lang racket

(define r0 0)
(define r1 0)
(define r2 0)
(define r3 0)
(define r4 0)
(define r5 0)

(define (j0)
  (set! r4 123)                           ; 00  seti 123 0 4
  (set! r4 (bitwise-and r4 456))          ; 01  bani 4 456 4
  (if (eq? r4 72)                         ; 02  eqri 4 72 4
    (begin
      (set! r4 0)                         ; 05  seti 0 6 4
      (j6))
    (j0)))

(define outs (list))

(define (j6)
  (set! r1 (bitwise-ior r4 65536))        ; 06  bori 4 65536 1
  (set! r4 678134)                        ; 07  seti 678134 1 4
  (j8))

(define (j8)
  (set! r5 (bitwise-and r1 255))          ; 08  bani 1 255 5
  (set! r4 (+ r4 r5))                     ; 09  addr 4 5 4
  (set! r4 (bitwise-and r4 16777215))     ; 10  bani 4 16777215 4
  (set! r4 (* r4 65899))                  ; 11  muli 4 65899 4
  (set! r4 (bitwise-and r4 16777215))     ; 12  bani 4 16777215 4
  (if (> 256 r1)                          ; 13  gtir 256 1 5
    (if (eq? r4 r0)                       ; 28  eqrr 4 0 5
      (done)
      (begin
        ; used to find the first repeating number in r4
        ;(if (member r4 outs)
          ;(begin
            ;(println (first outs))
            ;(exit))
          ;(set! outs (cons r4 outs)))
        (j6)))
    (begin
      (set! r5 0)                         ; 17  seti 0 1 5
      (j18))))

(define (j18)
  (set! r2 (add1 r5))                     ; 18  addi 5 1 2
  (set! r2 (* r2 256))                    ; 19  muli 2 256 2
  (if (> r2 r1)                           ; 20  gtrr 2 1 2
    (begin
      (set! r1 r5)                        ; 26  setr 5 3 1
      (j8))                               ; 27  seti 7 8 3
    (begin
      (set! r5 (add1 r5))                 ; 24  addi 5 1 5
      (j18))))                            ; 25  seti 17 1 3

(define (show-mem)
  (println (list r0 r1 r2 r3 r4 r5)))

(define (done)
  (show-mem)
  (exit))

(set! r0 (string->number (vector-ref (current-command-line-arguments) 0)))

(j0)
