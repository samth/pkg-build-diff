#lang racket

;; this quite ad-hoc file extracts information from a giant (80M) downloaded
;; master build log to help assess build fails

(require scramble/regexp
         threading
         sugar)

(provide archiving-log-hash)

(define log-lines (file->lines "/tmp/log.txt"))

(define slices
  (slicef-at log-lines (λ (l)
                         (or (regexp-match (px ^ "== Archiving") l)
                             (regexp-match (px ^ "Creating catalog") l)))))
(define archiving-slices
  (reverse (rest (reverse (rest slices)))))

(define archiving-log-hash
  (for/hash ([slice archiving-slices])
    (values (second
             (regexp-match (px ^ "== Archiving " (report (+ (chars (complement " ")))) " ==" $)
                           (first slice)))
            slice)))

