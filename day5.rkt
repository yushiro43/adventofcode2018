#lang racket

(define input (string-trim (file->string "day5-input.txt")))

(define pairs #rx"Aa|aA|Bb|bB|Cc|cC|Dd|dD|Ee|eE|Ff|fF|Gg|gG|Hh|hH|Ii|iI|Jj|jJ|Kk|kK|Ll|lL|Mm|mM|Nn|nN|Oo|oO|Pp|pP|Qq|qQ|Rr|rR|Ss|sS|Tt|tT|Uu|uU|Vv|vV|Ww|wW|Xx|xX|Yy|yY|Zz|zZ")

(define (string-replace-index string index length)
  (cond [(<= index 0) (substring string (+ index length))]
        [(>= index (string-length string)) (substring string 0 index)]
        [else (string-append
               (substring string 0 index)
               (substring string (+ index length)))]))

(define (collapse input)
  (do ([match (regexp-match-positions pairs input)])
    ((not match))
    (let ([pos (caar match)])
      (set! input (string-replace-index input pos 2))
      (set! match (regexp-match-positions pairs input))))
  input)

; pass lower-case character to eliminate, or "" for none
(define char (vector-ref (current-command-line-arguments) 0))

(let* ([my-input (string-replace input char "")]
       [my-input (string-replace my-input (string-upcase char) "")]
       [collapsed (collapse my-input)])
  (println (string-length input))
  (println (string-length my-input))
  (println (string-length collapsed)))
