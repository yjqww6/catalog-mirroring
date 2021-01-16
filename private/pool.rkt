#lang racket/base
(require racket/unit racket/match)
(provide (all-defined-out))

(define-signature connection^
  (make-conn
   close-conn))

(define-signature pool^
  (call-with-conn))

(define-unit unlimited-pool@
  (import connection^)
  (export pool^)

  (define cust (make-custodian))
  (define pool (make-hash))
  (define sema (make-semaphore 1))

  (define (call-with-conn key proc #:retry [retry 3])
    (parameterize ([current-custodian cust])
      (let try ([n retry])
        (let/cc cc
          (define conn
            (call-with-semaphore
             sema
             (λ ()
               (match (hash-ref pool key (λ () #f))
                 [(cons conn cs)
                  (hash-set! pool key cs)
                  conn]
                 [else
                  (make-conn key)]))))
          (begin0
            (with-handlers* ([exn:fail? (λ (e)
                                          (close-conn conn)
                                          (cond
                                            [(> n 0)
                                             (call-in-continuation cc (λ () (try (- n 1))))]
                                            [else (raise e)]))])
              (proc conn))
            (call-with-semaphore
             sema
             (λ ()
               (hash-update! pool
                             key (λ (cs) (cons conn cs)) (λ () (list conn))))))))))
  )