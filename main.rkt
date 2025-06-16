#lang racket

(require net/url (prefix-in u: net/url))


;; is the file old? default 1 day
(define (not-old? p [threshold (* 60 60 12)])
  (< (- (current-seconds) (file-or-directory-modify-seconds p))
     threshold))

(define caching? (make-parameter #f))

(provide get-pure-port/cached)

(define (get-pure-port/cached url)
  (define td (find-system-path 'temp-dir))
  (define p (build-path td (string->path-element (regexp-replace* #rx"/" (u:url->string url) "_"))))
  (if (and (file-exists? p) (not-old? p) (caching?))
      (begin
        (printf "using cached path ~a for ~a\n" p (u:url->string url))
        (open-input-file p))
      (begin
        (printf ">>> fetching ~a\n" (u:url->string url))
        (let ([file-p (open-output-file p #:exists 'truncate)]
              [result (open-output-string)])
          (with-handlers ([exn? (λ (e) (delete-file p) (raise e))])
            (call/input-url url (lambda (u [h null]) (get-pure-port u h #:redirections 5))
                          (λ (i)
                            (copy-port i file-p result)
                            (open-input-string (get-output-string result)))))))))

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
(define release-pre-site "http://next-pkg-build.racket-lang.org/")
(define nwu-release-pre-site "https://plt.cs.northwestern.edu/release-pkg-build/")
(define snapshot-site "https://plt.cs.northwestern.edu/pkg-build/")
(define new-snapshot-site "https://plt.cs.northwestern.edu/new-snapshots/")

(define (pkg->author p)
  (define pkg-info (url->value (format "https://pkgs.racket-lang.org/pkg/~a" p)))
  (cond [(hash? pkg-info)
         (hash-ref pkg-info 'author)]
        [else "<unknown>"])) ; something went wrong

;; fetch the summary hash, make sure it's a hash
(define (fetch-summary-hash site)
  (define url (string-append site "summary.rktd"))
  (define value (url->value url))
  (unless (hash? value)
    (error 'fetch-summary-hash
           "expected hash table from url ~v, got something else: ~e"
           url value))
  value)

(define release (fetch-summary-hash release-site))
;(define release-pre (fetch-summary-hash release-pre-site))
(define snapshot (fetch-summary-hash snapshot-site))

;(define release (url->value (string-append release-site "summary.rktd")))
;(define release-pre (url->value (string-append release-pre-site "summary.rktd")))
;(define snapshot (url->value (string-append snapshot-site "summary.rktd")))

(define/contract (status r)
  (-> hash? any)
  (define h
    (make-hash (map cons
                    '(failure-log conflicts-log dep-failure-log min-failure-log test-failure-log)
                    '(build-fail install-conflict dep-fail needs-extra-deps test-fail))))
  (define (log->status v)
    (hash-ref h v))
  (define fail
    (for/first ([k '(failure-log conflicts-log test-failure-log dep-failure-log min-failure-log)]
                #:when (hash-ref r k #f))
      (log->status k)))
  (or fail
      (if (null? (hash-ref r 'docs #f))
          'no-docs
          'success)))

(define code-level
  (make-hash `((build-fail . 0)
               (install-conflict . 1)
               (dep-fail . 3)
               (needs-extra-deps . 4)
               (test-fail . 2)
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
              #:unless 
              (or (eq? other 'missing) (eq? v 'missing))
              #:when
              (if better? (worse? other v) (worse? v other)))
    (values k (list (if (symbol? v) v (status v))
                    (if (symbol? other) other (status other))))))

(define/contract ((all-has-key key) h) 
  (-> symbol? (-> (hash/c any/c (hash/c symbol? any/c)) any))
  (for/list ([(k v) h]
             #:when (hash-ref v key #f))
    k))

(define conflicts (all-has-key 'conflicts-log))
(define build-fail (all-has-key 'failure-log))
(define test-fail (all-has-key 'test-failure-log))
(define dep-fail (all-has-key 'dep-failure-log))
(define (all-pkgs h) (hash-keys h))

;(define diffs (diff release snapshot))

(define (explain-build-failure h server-url)
  (unless (hash? h)
    (raise-argument-error 'explain-build-failure "hash?" h))
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
    [(regexp-match "timeout" log-str) 'timeout]
    [else 'unknown]))

(define (list-small a b)
  (if (equal? a b) a (list a b)))

(define (reverse-hash orig)
  (for/fold ([h (hash)])
            ([(k v) (force orig)])
    (hash-update h v (λ (old) (cons k old)) null)))

(define (compare-sites s1 s2)
  (define s1-hash (url->value (string-append s1 "summary.rktd")))
  (define s2-hash (url->value (string-append s2 "summary.rktd")))
  (unless (hash? s1-hash)
    (error 'compare-sites "site ~a did not have a hash\n   value was: ~s" s1 s1-hash))
  (unless (hash? s2-hash)
    (error 'compare-sites "site ~a did not have a hash\n   value was: ~s" s2 s2-hash))
  ;(printf "~s ~s\n" s1-hash s2-hash)
  (define s1-fails (build-fail s1-hash))
  (define s2-fails (build-fail s2-hash))
  (define result-hash (compare s1-hash s2-hash))
  (define explain-hash
    (delay
      (for/hash ([p (remove-duplicates (append s1-fails s2-fails))])
        (values p
                (list-small (and (member p s1-fails)
                                 (explain-build-failure (hash-ref s1-hash p) s1))
                            (and (member p s2-fails)
                                 (explain-build-failure (hash-ref s2-hash p) s2)))))))
  (append (list 'url-1 s1)
          (list 'url-2 s2)
          (list 'worse result-hash)
          (list 'better (compare s1-hash s2-hash #:better? #t))
          (if (explain?)
              (list 
               'build-failure-explain (reverse-hash (force explain-hash)) 
               'failing-plt-authors (reverse-hash (for*/hash ([(k v) (in-hash (force explain-hash))]
                                                              [a (in-value (pkg->author k))]
                                                              #:when (plt? a))
                                                    (values k a)))
               'failing-other-authors (reverse-hash (for*/hash ([(k v) (in-hash (force explain-hash))]
                                                                [a (in-value (pkg->author k))]
                                                                #:unless (plt? a))
                                                      (values k a))))
               null)))

(define (plt? a)
  (or (regexp-match "@racket-lang.org" a)
      (member a '("jay.mccarthy@gmail.com"
                  "eli@barzilay.org"
                  "jensaxel@soegaard.net"
                  "neil.toronto@gmail.com"
                  "spencer@florence.io"
                  "tonygarnockjones@gmail.com"
                  "types@ccs.neu.edu"
                  "mflatt@cs.utah.edu"
                  ""
                  "leif@leifandersen.net"))))

(define explain? (make-parameter #f))

(define (check-printer h)
  (match-define (list 'url-1 u1 'url-2 u2 'worse w 'better _) h)
  (define l (sort #:key car  (hash-map w cons) string<?))
  (printf "### Build Failures\n")
  (for ([(p result) (in-dict l)]
        #:when (equal? (second result) 'build-fail))
    (printf "- [ ] ~a ~aserver/built/fail/~a.txt\n" p u2 p))
  (printf "### Test Failures\n")
  (for ([(p result) (in-dict l)]
        #:when (equal? (second result) 'test-fail))
    (printf "- [ ] ~a ~aserver/built/test-fail/~a.txt\n" p u2 p))
  (printf "### Dependency Failures\n")
  (for ([(p result) (in-dict l)]
        #:when (equal? (second result) 'dep-fail))
    (printf "- [ ] ~a ~aserver/built/deps/~a.txt\n" p u2 p))
  (printf "### Other Failures\n")
  (for ([(p result) (in-dict l)]
        #:unless (member (second result) '(test-fail build-fail dep-fail)))
    (printf "- [ ] ~a\n" p))
  )


(module+ main
  (require racket/cmdline)
  (define next-release? #f)
  (define nightly? #t)
  (define checkboxes #f)
  (define new-snapshot #f)
  (command-line
   #:once-each
   [("--release") "compare the next release" (set! next-release? #t)]
   [("--new-snapshot") "compare the new snapshots site" (set! new-snapshot #t)]
   [("--checkboxes") "print as checkboxes for GitHub" (set! checkboxes #t)]
   [("--explain") "provide more details" (explain? #t)])
  (define (printer v)
    (if checkboxes
        (check-printer v)
        (pretty-print v)))
  (when nightly?
    (printf "\n\n\tCurrent Release vs HEAD\n========================================\n")
    (printer (compare-sites release-site snapshot-site)))
  (when new-snapshot
    (printf "\n\n\tCurrent Release vs New Snapshot Site\n========================================\n")
    (printer (compare-sites release-site new-snapshot-site)))
  (when next-release?
    (printf "\n\n\tCurrent Release vs Next Release\n========================================\n")
    (printer (compare-sites release-site nwu-release-pre-site)))
  )
