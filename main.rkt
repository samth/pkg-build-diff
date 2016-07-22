#lang racket

(require net/url (prefix-in u: net/url))


;; is the file old? default 1 day
(define (not-old? p [threshold (* 60 60 12)])
  (< (- (current-seconds) (file-or-directory-modify-seconds p))
     threshold))

(provide get-pure-port/cached)

(define (get-pure-port/cached url)
  (define td (find-system-path 'temp-dir))
  (define p (build-path td (string->path-element (regexp-replace* #rx"/" (u:url->string url) "_"))))
  (if (and (file-exists? p) (not-old? p))
      (begin
        ;(printf "using cached path for ~a\n" (u:url->string url))
        (open-input-file p))
      (begin
        (printf ">>> fetching ~a\n" (u:url->string url))
        (let ([file-p (open-output-file p #:exists 'truncate)]
              [result (open-output-string)])
          (call/input-url url get-pure-port
                          (λ (i)
                            (copy-port i file-p result)
                            (open-input-string (get-output-string result))))))))

(define (url->value u)
  (define u*
    (cond [(string? u) (string->url u)]
          [(url? u) u]
          [else (error 'url->value "expected url-string?")]))
  (call/input-url u* get-pure-port/cached read))

(define (url->string u)
  (define u*
    (cond [(string? u) (string->url u)]
          [(url? u) u]
          [else (error 'url->value "expected url-string?")]))
  (call/input-url u* get-pure-port/cached port->string))

(define release-site "https://pkg-build.racket-lang.org/")
;(define release-pre-site "http://next-pkg-build.racket-lang.org/")
(define snapshot-site "https://plt.eecs.northwestern.edu/pkg-build/")

(define (pkg->author p)
  (hash-ref (url->value (format "https://pkgs.racket-lang.org/pkg/~a" p)) 'author))



(define release (url->value (string-append release-site "summary.rktd")))
;(define release-pre (url->value (string-append release-pre-site "summary.rktd")))
(define snapshot (url->value (string-append snapshot-site "summary.rktd")))

(define (status r)
  (define h
    (make-hash (map cons
                    '(failure-log conflicts-log dep-failure-log min-failure-log test-failure-log)
                    '(build-fail install-conflict dep-fail needs-extra-deps test-fail))))
  (define (log->status v)
    (hash-ref h v))
  (define fail
    (for/first ([k '(failure-log conflicts-log dep-failure-log test-failure-log min-failure-log)]
                #:when (hash-ref r k #f))
      (log->status k)))
  (or fail
      (if (null? (hash-ref r 'docs #f))
          'no-docs
          'success)))

(define code-level
  (make-hash `((build-fail . 0)
               (install-conflict . 1)
               (dep-fail . 2)
               (needs-extra-deps . 4)
               (test-fail . 3)
               (no-docs . 5)
               (success . 6))))

;; is the record for b in a worse state (earlier failure) than a?
(define (worse? a b)
  (> (hash-ref code-level (status a)) (hash-ref code-level (status b))))

(define (diff a b)
  (define init
    (for*/hash ([(k v) a]
                [v2 (in-value (hash-ref b k #f))]
                #:when (not (equal? v v2)))
      (define v* (hash-remove v 'docs))
      (define v2* (and v2 (hash-remove v2 'docs)))
      (if (equal? v* v2*)
          (values k #f)
          (values k (list v v2)))))
  (for/hash ([(k v) init]
             #:when v)
    (values k v)))

(define (compare v1 v2 #:better? [better? #f])
  (for*/hash ([k (remove-duplicates (append (hash-keys v1) (hash-keys v2)))]
              [v (in-value (hash-ref v1 k 'missing))]
              [other (in-value (hash-ref v2 k 'missing))]
              #:when
              (or (eq? other 'missing)
                  (if better? (worse? other v) (worse? v other))))
    (values k (list (if (symbol? v) v (status v))
                    (if (symbol? other) other (status other))))))

(define ((all-has-key key) h) (for/list ([(k v) h]
                                       #:when (hash-ref v key #f))
                              k))

(define conflicts (all-has-key 'conflicts-log))
(define build-fail (all-has-key 'failure-log))
(define test-fail (all-has-key 'test-failure-log))
(define dep-fail (all-has-key 'dep-failure-log))
(define (all-pkgs h) (hash-keys h))

(define diffs (diff release snapshot))

(define (explain-build-failure h server-url)
  (define log-url (hash-ref h 'failure-log))
  (unless log-url
    (error 'explain-build-failure "can't explain nonexistent failure"))
  (define log-str (url->string (string-append server-url log-url)))
  (cond
    [(regexp-match "version mismatch for dependency" log-str) 'old-version]
    [(regexp-match "PLaneT could not download the package \"lizorkin" log-str) 'lizorkin-planet]
    [(regexp-match "PLaneT could not download the package \"dyoo" log-str) 'dyoo-planet]
    [(regexp-match "PLaneT could not download the package \"neil" log-str) 'neil-planet]
    [(regexp-match "PLaneT could not download the package \"jaymccarthy" log-str) 'jay-planet]
    [(regexp-match "PLaneT could not download the package \"schematics" log-str) 'schematics-planet]
    [(regexp-match "PLaneT could not download the package" log-str) 'planet]
    [(regexp-match "cannot use empty checksum" log-str) 'bad-pkg-metadata]
    [(regexp-match "collection not found" log-str)
     (cond [(regexp-match (regexp-quote "collection: \"unstable") log-str)
           'missing-unstable]
           [(regexp-match (regexp-quote "module path: unstable") log-str)
           'missing-unstable]
           [else 'missing-module])]
    [(regexp-match "cannot open module file" log-str)
     (cond [(regexp-match (regexp-quote "collection: \"unstable") log-str)
           'missing-unstable]
           [(regexp-match (regexp-quote "module path: unstable") log-str)
           'missing-unstable]
           [else 'missing-module])]
    [(regexp-match "tcp-connect: connection failed" log-str) 'used-network]
    [(regexp-match "identifier already imported" log-str) 'duplicate-import]
    [(regexp-match "raco pkg install: cannot find package on catalogs" log-str)
     (cond [(regexp-match "package: planet-neil" log-str) 'neil-planet]
           [(regexp-match "package: planet-schematics" log-str) 'schematics-planet]
           [(regexp-match "package: planet-" log-str) 'planet]
           [else 'dep-not-exist])]
    [(regexp-match "raco setup:.*no #%.* syntax transformer is bound" log-str) 'macro-error]
    [(regexp-match "cannot instantiate `racket/gui/base' a second time" log-str) 'gui-docs]
    [else 'unknown]))

(define (list-small a b)
  (if (equal? a b) a (list a b)))

(define (reverse-hash orig)
  (for/fold ([h (hash)])
            ([(k v) orig])
    (hash-update h v (λ (old) (cons k old)) null)))

(define (compare-sites s1 s2)
  (define s1-hash (url->value (string-append s1 "summary.rktd")))
  (define s2-hash (url->value (string-append s2 "summary.rktd")))
  (define s1-fails (build-fail s1-hash))
  (define s2-fails (build-fail s2-hash))
  (define result-hash (compare s1-hash s2-hash))
  (define explain-hash
    (for/hash ([p (remove-duplicates (append s1-fails s2-fails))])
      (values p
              (list-small (and (member p s1-fails)
                               (explain-build-failure (hash-ref s1-hash p) s1))
                          (and (member p s2-fails)
                               (explain-build-failure (hash-ref s2-hash p) s2))))))
  (list 'worse result-hash
        'better (compare s1-hash s2-hash #:better? #t)
        'build-failure-explain (reverse-hash explain-hash)
        'failing-plt-authors (reverse-hash (for*/hash ([(k v) (in-hash explain-hash)]
                                                      [a (in-value (pkg->author k))]
                                                      #:when (plt? a))
                                             (values k a)))
        'failing-other-authors (reverse-hash (for*/hash ([(k v) (in-hash explain-hash)]
                                                      [a (in-value (pkg->author k))]
                                                      #:unless (plt? a))
                                               (values k a)))))

(define (plt? a)
  (or (regexp-match "@racket-lang.org" a)
      (member a '("jay.mccarthy@gmail.com"
                  "eli@barzilay.org"
                  "jensaxel@soegaard.net"
                  "neil.toronto@gmail.com"
                  "spencer@florence.io"
                  "tonygarnockjones@gmail.com"))))


(module+ main
  (pretty-print (compare-sites release-site snapshot-site)))
