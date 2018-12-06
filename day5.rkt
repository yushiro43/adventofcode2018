#lang racket

(define input (string-trim (file->string "day5-input.txt")))

(define pairs #rx"Aa|aA|Bb|bB|Cc|cC|Dd|dD|Ee|eE|Ff|fF|Gg|gG|Hh|hH|Ii|iI|Jj|jJ|Kk|kK|Ll|lL|Mm|mM|Nn|nN|Oo|oO|Pp|pP|Qq|qQ|Rr|rR|Ss|sS|Tt|tT|Uu|uU|Vv|vV|Ww|wW|Xx|xX|Yy|yY|Zz|zZ")

(define (string-replace-index string index length)
  (string-append (substring string 0 index)
                 (substring string (+ index length))))

(define (collapse input len)
  (let* ([input (regexp-replace* pairs input "")]
         [new-len (string-length input)])
    (if (= new-len len)
      input
      (collapse input new-len))))

; pass lower-case character to eliminate, or "" for none
(define char (vector-ref (current-command-line-arguments) 0))

(let* ([input (string-replace input char "")]
       [input (string-replace input (string-upcase char) "")]
       [collapsed (collapse input (string-length input))])
  (println (string-length collapsed)))
