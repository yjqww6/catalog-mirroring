#lang racket/base
(require racket/list net/url racket/string)

(provide (all-defined-out))

(define (may-append-git replaced)
  (cond
    [(and (string-prefix? replaced "http")
          (not (string-contains? replaced ".git")))
     (define u (string->url replaced))
     (url->string (append-git u))]
    [else replaced]))

(define (append-git u)
  (let ([ps (filter (λ (p) (not (string=? "" (path/param-path p)))) (url-path u))])
    (struct-copy
     url u
     [path
      (list-update
       ps
       (- (length ps) 1)
       (λ (p)
         (path/param
          (string-append (path/param-path p) ".git")
          '())))])))

(define (join-path path . ps)
  (append path (map (λ (p) (path/param p '())) ps)))

(define-logger mirror)

(define (call/timeout timeout thunk)
  (let ([ch (make-channel)]
        [cust (make-custodian)])
    (parameterize ([current-custodian cust])
      (let ([t (thread
                (λ ()
                  (channel-put
                   ch
                   (with-handlers ([exn? void])
                     (thunk)))))])
        (cond
          [(sync/timeout timeout ch) => values]
          [else
           (custodian-shutdown-all cust)
           (error 'timeout-get)])))))