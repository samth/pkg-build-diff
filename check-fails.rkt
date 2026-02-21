#lang racket

;; this file is a medium-hacky bit of code to help categorize the many
;; pkg-build-diff fails associated with 9.1.

(require threading
         scramble/regexp
         "main.rkt")

(caching? #t)

(match-define
  (list 'url-1
        url1
        'url-2
        url2
        ;; is there always a worse? not sure
        'worse
        worse-table
        ;; is there always a better? not sure
        'better
        better-table)
  (compare-sites release-site nwu-release-pre-site))


(define build-fail-pkgs
  (~> worse-table
      (hash->list)
      (filter (λ (pr) (equal? (caddr pr) 'build-fail)) _)
      (map first _)))



(define gitea-lines
  (regexp-split (px "\n")
                "tcp-connect: host not found
  hostname: gitea.suzanne.soy
  port number: 443
  system error: Temporary failure in name resolution; gai_err=-3"))

;; is `a` a sublist of `b` ?
;; this implementation sucks when a is long...
(define (is-sublist? a b)
  (if (< (length b) (length a))
      #f
      (if (equal? (take b (length a)) a)
          #t
          (is-sublist? a (rest b)))))

;; um ... caching?
(define (gitea-fail? pkg)
  (is-sublist?
   gitea-lines
   (regexp-split (px "\n")
                 (url->string
                  (fail-log-url pkg url2 'build-fail)))))

(define-values (a b)
  (partition gitea-fail? build-fail-pkgs))











