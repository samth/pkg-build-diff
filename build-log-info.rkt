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

(define creating-catalog-px (px ^ "Creating catalog"))

(define slices
  (slicef-at log-lines (λ (l)
                         (or (regexp-match (px ^ "== Archiving") l)
                             (regexp-match creating-catalog-px l)))))

(printf "broke log into ~v slices.\n" (length slices))

(let ()
  (define first-slice-length (length (first slices)))
  (cond [(<= 6 first-slice-length 14)
         (printf "length of first slice is ~a, which is pretty close to the expected length.\n"
                 first-slice-length)]
        [else (error 'first-slice-length
                     "length of first slice is ~a which is not that close to the expected length.")]))

(let ()
  (unless (regexp-match creating-catalog-px (first (last slices)))
    (error 'last-slice-first-line
           "Expected first line of last slice to begin with `Creating catalog`"))
  (define last-slice-length (length (last slices)))
  (cond [(< last-slice-length 500000)
         (error 'last-slice-length
                "last slice length was lower than expected, investigate?")]
        [(< last-slice-length 2000000)
         (printf "last slice length was ~a, which seems plausible.\n"
                 last-slice-length)]
        [else
         (error 'last-slice-length
                "last-slice-length is ~a, which seems higher than expected.")]))


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



