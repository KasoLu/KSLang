#lang racket

(provide (all-defined-out))
(require "utils.rkt")

(define @decorate
  (lambda (parser d-ctx d-token d-succ d-fail)
    (lambda (tokens ctx k-token k-succ k-fail)
      (parser tokens (d-ctx ctx) (d-token k-token) (d-succ k-succ) (d-fail k-fail)))))

(define @success
  (lambda (parser bind)
    (lambda (tokens ctx k-token k-succ k-fail)
      (let ([succ (lambda (t tokens* ctx*) ((bind t) tokens* ctx* k-token k-succ k-fail))])
        (parser tokens ctx k-token succ k-fail)))))

(define @failure
  (lambda (parser bind)
    (lambda (tokens ctx k-token k-succ k-fail)
      (let ([fail (lambda (tokens* ctx*) ((bind) tokens ctx k-token k-succ k-fail))])
        (parser tokens ctx k-token k-succ fail)))))

(define @const
  (lambda (x)
    (lambda (tokens ctx k-token k-succ k-fail)
      (k-succ x tokens ctx))))

(define @error
  (lambda ()
    (lambda (tokens ctx k-token k-succ k-fail)
      (k-fail tokens ctx))))

(define @token
  (lambda (pred)
    (lambda (tokens ctx k-token k-succ k-fail)
      (cond [(null? tokens) (k-fail tokens ctx)]
            [(pred tokens ctx) (k-token tokens ctx k-succ)]
            [else (k-fail tokens ctx)]))))

(define @and
  (lambda parsers
    (define combine
      (lambda (p ps)
        (@success p (lambda (t) (@success ps (lambda (ts) (@const (cons t ts))))))))
    (let ([parser (foldr combine (@const '()) parsers)])
      (lambda (tokens ctx k-token k-succ k-fail)
        (parser tokens ctx k-token k-succ (lambda (tokens* ctx*) (k-fail tokens ctx)))))))

(define @or
  (lambda parsers
    (foldr (lambda (p ps) (@failure p (lambda () ps))) (@error) parsers)))

(define @not
  (lambda (parser)
    (lambda (tokens ctx k-token k-succ k-fail)
      (parser tokens ctx k-token
        (lambda (t tokens* ctx*)
          (k-fail tokens ctx))
        (lambda (tokens* ctx*)
          (if (null? tokens)
            (k-fail tokens ctx)
            (k-token tokens ctx k-succ)))))))

(define @?
  (lambda (parser)
    (@or parser (@const '()))))

(define @*
  (lambda (parser)
    (@or (@+ parser) (@const '()))))

(define @+
  (lambda (parser)
    (@and parser (lambda $ (apply (@* parser) $)))))
