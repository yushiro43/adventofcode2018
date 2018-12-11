#lang racket

(provide deque
         list->deque
         deque-length
         deque-first
         deque-last
         deque-ref
         deque->list
         deque-append!
         deque-prepend!
         deque-pop!
         deque-pop-left!
         deque-replace!
         deque-rotate!)

(struct deque-node (prev val next) #:mutable)
(struct deque-wrapper (start len) #:mutable)

(define (deque . vals)
  (let ([q (deque-wrapper '() 0)])
    (for ([val vals])
      (deque-append! q val))
    q))

(define (list->deque lst)
  (apply deque lst))

(define (deque-length q)
  (deque-wrapper-len q))

(define (deque-first q)
  (deque-node-val (deque-wrapper-start q)))

(define (deque-last q)
  (deque-node-val (deque-node-prev (deque-wrapper-start q))))

(define (deque-ref q index)
  (deque-node-val (deque-ref-node q index)))

(define (deque->list q)
  (if (zero? (deque-length q))
    '()
    (letrec ([fn (lambda (node cnt lst)
                   (if (zero? cnt)
                     lst
                     (fn (deque-node-prev node) (sub1 cnt) (cons (deque-node-val node) lst))))])
      (fn (deque-node-prev (deque-wrapper-start q)) (deque-wrapper-len q) '()))))

(define (deque-append! q val)
  (if (zero? (deque-wrapper-len q))
    (let ([new-node (deque-node '() val '())])
      (set-deque-node-prev! new-node new-node)
      (set-deque-node-next! new-node new-node)
      (set-deque-wrapper-start! q new-node)
      (set-deque-wrapper-len! q 1))
    (let* ([first-node (deque-wrapper-start q)]
           [last-node (deque-node-prev first-node)]
           [new-node (deque-node last-node val first-node)])
      (set-deque-node-prev! first-node new-node)
      (set-deque-node-next! last-node new-node)
      (set-deque-wrapper-len! q (add1 (deque-wrapper-len q))))))

(define (deque-prepend! q val)
  (deque-append! q val)
  (deque-rotate! q -1))

(define (deque-pop! q)
  (let* ([first-node (deque-wrapper-start q)]
         [last-node (deque-node-prev first-node)]
         [new-last-node (deque-node-prev last-node)])
    (set-deque-node-next! new-last-node first-node)
    (set-deque-node-prev! first-node new-last-node)
    (set-deque-wrapper-len! q (sub1 (deque-wrapper-len q)))
    (when (zero? (deque-length q)) (set-deque-wrapper-start! q '()))
    (deque-node-val last-node)))

(define (deque-pop-left! q)
  (let* ([first-node (deque-wrapper-start q)]
         [last-node (deque-node-prev first-node)]
         [new-first-node (deque-node-next first-node)])
    (set-deque-node-prev! new-first-node last-node)
    (set-deque-node-next! last-node new-first-node)
    (set-deque-wrapper-len! q (sub1 (deque-wrapper-len q)))
    (when (zero? (deque-length q)) (set-deque-wrapper-start! q '()))
    (deque-node-val first-node)))

(define (deque-replace! q index val)
  (let ([node (deque-ref-node q index)])
    (set-deque-node-val! node val))
  val)

(define (deque-rotate! q cnt)
  (when (zero? (deque-length q))
    (raise (exn:fail:contract "The deque is empty." (current-continuation-marks))))
  (set-deque-wrapper-start! q (deque-ref-node q cnt))
  (deque-first q))

; private stuff

(define (deque-ref-node q index)
  (if (zero? (deque-length q))
    (raise (exn:fail:contract "The deque is empty." (current-continuation-marks)))
    (letrec ([fn (lambda (n i)
                   (cond [(zero? i) n]
                         [(negative? i)
                          (fn (deque-node-prev n) (add1 i))]
                         [(positive? i)
                          (fn (deque-node-next n) (sub1 i))]))])
      (fn (deque-wrapper-start q) index))))
