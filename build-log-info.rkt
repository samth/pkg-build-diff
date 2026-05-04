#lang racket

;; this quite ad-hoc file extracts information from a giant (80M) downloaded
;; master build log to help assess build fails

(require scramble/regexp
         threading
         sugar)

(provide archiving-log-hash)

;; read 
(define log-lines (file->lines "/tmp/log.txt"))
(printf "read ~v lines from file.\n" (length log-lines))

(define slices
  (slicef-at log-lines (λ (l)
                         (or (regexp-match (px ^ "== Archiving") l)
                             (regexp-match (px ^ "Creating catalog") l)))))

(printf "broke log into ~v slices.\n" (length slices))

;; drop first and last slices; first is setup, last (huge) is the whole rest of
;; the file after the archiving slices
(define archiving-slices
  (reverse (rest (reverse (rest slices)))))

;; pull the package name from the `Archiving` line
(define (slice-package-name archiving-line)
  (second
   (regexp-match (px ^ "== Archiving " (report (+ (chars (complement " ")))) " ==" $)
                 archiving-line)))

;; a hash from package name to the archiving block of the log
(define archiving-log-hash
  (for/hash ([slice archiving-slices])
    (values (slice-package-name (first slice))
            slice)))

(hash-keys archiving-log-hash)



