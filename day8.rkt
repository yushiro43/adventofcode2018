#lang racket

;(define input (map string->number (string-split "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")))

; 2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
;------------------------------------
; 2 3                           1 1 2
;     0 3/10 11 12|1 1        2
;                      0 1/99

(define input (file->list "day8-input.txt"))

(define meta-data (list))

(define vals (make-hash))

(define (sum lst) (foldl + 0 lst))

(define (build-tree input count-at-this-level current-child breadcrumbs)
  (if (> current-child count-at-this-level)
    ; no more input at this level; return upward
    input
    ; consume all the things
    (let ([child-nodes (first input)]
          [meta-count (second input)])
      ; consume children; down one level
      (let ([input (build-tree (drop input 2) child-nodes 1 (cons 1 breadcrumbs))])
        ; consume meta
        (set! meta-data (append meta-data (take input meta-count)))
        (if (> child-nodes 0)
          ; sum vals from children named by metadata
          (hash-set! vals breadcrumbs (sum (for/list ([m (take input meta-count)])
                                                     (hash-ref vals (cons m breadcrumbs) 0))))
          ; sum metadata itself
          (hash-set! vals breadcrumbs (sum (take input meta-count))))
        ; consume siblings on this level
        (let* ([next-child (add1 current-child)]
               [breadcrumbs (cons next-child (drop breadcrumbs 1))])
          (build-tree (drop input meta-count) count-at-this-level next-child breadcrumbs))))))

(define _ (build-tree input 1 1 '(1)))

(printf "metadata sum: ~s\n" (sum meta-data))

(printf "root val: ~s\n" (hash-ref vals '(1)))
