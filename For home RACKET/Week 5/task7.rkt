#lang racket

(define (longest s1 s2)
  (list->string(sort (remove-duplicates (string->list(string-append s1 s2))) char<?))
  )

; I am 100% sure this is not the solution you expected :D However I looked through the documentation for strings you refered to in notes:
; (https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string%29%29)
; and the documentation for pairs and lists:
; (https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._list-ref%29%29)
; and I found out we can convert from string to list and list to string with string->list and vice versa and
; use the sort function for lists with chars by putting "char<?" as the procedure.
; The given example was "strict comparison functions (e.g., < or string<?; not <= or string<=?)" so I decided to try it with char and it worked :D

(equal? (longest "xyaabbbccccdefww" "xxxxyyyyabklmopq") "abcdefklmopqwxy")
(equal? (longest "abcdefghijklmnopqrstuvwxyz" "abcdefghijklmnopqrstuvwxyz") "abcdefghijklmnopqrstuvwxyz")