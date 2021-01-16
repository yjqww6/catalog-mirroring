#lang racket/base
(require "private/utils.rkt" "private/pool.rkt"
         racket/string racket/unit
         net/url
         web-server/dispatch
         web-server/http/request-structs
         web-server/http/response-structs
         web-server/http/empty
         (prefix-in seq: web-server/dispatchers/dispatch-sequencer)
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         (prefix-in timeout: web-server/dispatchers/dispatch-timeout))

(define upstream-catalog (make-parameter (string->url "https://pkgs.racket-lang.org")))

(define source-mirror (make-parameter values))

(define limit (make-parameter (make-semaphore 8)))

(define APPLICATION/OCTET-STREAM #"application/octet-stream")

(define (pkg-rewrite ht)
  (let* ([ht
          (hash-set ht
                    'source
                    (url->string ((source-mirror) (string->url (hash-ref ht 'source)))))])
    (cond
      [(hash-ref ht 'versions (λ () #f))
       =>
       (λ (versions)
         (hash-set ht
                   'versions
                   (for/hash ([(k v) (in-hash versions)])
                     (values k (pkg-rewrite v)))))]
      [else ht])))

(define get
  (let ()
    (define (make-conn _) (make-http-connection))
    (define close-conn http-connection-close)
    (define-values/invoke-unit/infer unlimited-pool@)
    (λ (url)
      (define k (cons (url-host url) (url-port url)))
      (call-with-conn
       k
       (λ (conn) 
         (call/timeout
          10
          (λ ()
            (define-values (port _)
              (get-pure-port/headers url #:connection conn))
            (begin0
              (read port)
              (let loop ()
                (unless (eof-object? (read port))
                  (loop)))))))))))

(define (catalog-common req path-rewrite result-rewrite)
  (define up (upstream-catalog))
  (define redirected
    (struct-copy url (request-uri req)
                 [scheme (url-scheme up)]
                 [host (url-host up)]
                 [port (url-port up)]
                 [path (path-rewrite (url-path up))]))
  (log-mirror-info "fetch upstream ~a" (url->string redirected))
  (define ht (result-rewrite (call-with-semaphore (limit) (λ () (get redirected)))))
  (log-mirror-info "fetch upstream done ~a" (url->string redirected))
  (response/output (λ (output) (write ht output)) #:mime-type APPLICATION/OCTET-STREAM))

(define (pkg req name)
  (catalog-common req (λ (p) (join-path p "pkg" name)) pkg-rewrite))

(define (pkgs req)
  (catalog-common req (λ (p) (join-path p "pkgs")) values))

(define (pkgs-all req)
  (catalog-common req (λ (p) (join-path p "pkgs-all"))
                  (λ (ht)
                    (for/hash ([(k v) (in-hash ht)])
                      (values k (pkg-rewrite v))))))

(define-values (catalog-dispatch catalog-url)
  (dispatch-rules
   [("pkg" (string-arg)) pkg]
   [("pkgs") pkgs]
   [("pkgs-all") pkgs-all]))

(define (make-dispatcher sema)
  (seq:make
   (timeout:make 300)
   (lift:make (λ (req) (catalog-dispatch req)))
   (lift:make (λ (req) (response/empty #:code 404)))))

(define (make-mirror src-url mirror-url k)
  (λ (uri)
    (define str (url->string uri))
    (cond
      [(string-contains? str src-url)
       (define replaced (string-replace str src-url mirror-url))
       (define final (may-append-git replaced))
       (log-mirror-info "redirected : ~a -> ~a" str final)
       (string->url final)]
      [else (k uri)])))

(module+ main
  (require racket/cmdline
           web-server/servlet-dispatch)
  (define ip (make-parameter "localhost"))
  (define port (make-parameter 8787))

  (command-line
   #:program "catalog-mapping"
   #:once-each
   [("-p" "--port") p "listen port" (port (string->number p))]
   [("--listen-ip") i "listen ip" (ip i)]
   [("--upstream") => (compose1 upstream-catalog string->url)
                   (list (string-append "specifying upstream catalog server, default to "
                                        (url->string (upstream-catalog))))]
   #:multi
   [("-m" "--mirror") src-url mirror-url "specifying mirror"
                      (source-mirror
                       (make-mirror src-url mirror-url (source-mirror)))]
   #:usage-help "racket main.rkt -m https://github.com https://somegithubmirror.xxx")
  
  (serve/launch/wait make-dispatcher
                     #:listen-ip (ip)
                     #:port (port))
  
  )
