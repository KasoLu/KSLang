#lang racket

(provide (all-defined-out))
(require "utils.rkt")

(define @decorate
  (lambda (parser d-ctx d-succ d-fail)
    (lambda (tokens ctx k-ctx k-succ k-fail)
      (parser tokens (d-ctx ctx) k-ctx (d-succ k-succ) (d-fail k-fail)))))

(define @success
  (lambda (parser bind)
    (lambda (tokens ctx k-ctx k-succ k-fail)
      (let ([succ (lambda (t tokens ctx) ((bind t) tokens ctx k-ctx k-succ k-fail))])
        (parser tokens ctx k-ctx succ k-fail)))))

(define @failure
  (lambda (parser bind)
    (lambda (tokens ctx k-ctx k-succ k-fail)
      (let ([fail (lambda (tokens* ctx*) ((bind) tokens ctx k-ctx k-succ k-fail))])
        (parser tokens ctx k-ctx k-succ fail)))))

(define @const
  (lambda (x)
    (lambda (tokens ctx k-ctx k-succ k-fail)
      (k-succ x tokens ctx))))

(define @error
  (lambda ()
    (lambda (tokens ctx k-ctx k-succ k-fail)
      (k-fail tokens ctx))))

(define @token
  (lambda (pred)
    (lambda (tokens ctx k-ctx k-succ k-fail)
      (cond [(null? tokens) (k-fail tokens ctx)]
            [(pred (car tokens)) ((k-ctx k-succ) (car tokens) (cdr tokens) ctx)]
            [else (k-fail tokens ctx)]))))

(define @and
  (lambda parsers
    (define and-parser
      (let loop ([parsers parsers])
        (if (null? parsers)
            (@const '())
            (@success (car parsers) 
              (lambda (t)
                (@success (loop (cdr parsers))
                  (lambda (ts)
                    (@const (cons t ts)))))))))
    (lambda (tokens ctx k-ctx k-succ k-fail)
      (and-parser tokens ctx k-ctx k-succ (lambda (tokens* ctx*) (k-fail tokens ctx))))))

(define @or
  (lambda parsers
    (let loop ([parsers parsers])
      (if (null? parsers)
        (@error)
        (@failure (car parsers) 
          (lambda () (loop (cdr parsers))))))))

(define @not
  (lambda (parser)
    (lambda (tokens ctx k-ctx k-succ k-fail)
      (parser tokens ctx k-ctx
        (lambda (t tokens* ctx*)
          (k-fail tokens ctx))
        (lambda (tokens* ctx*)
          (if (null? tokens)
            (k-fail tokens ctx)
            ((k-ctx k-succ) (car tokens) (cdr tokens) ctx)))))))

(define @?
  (lambda (parser)
    (@or parser (@const '()))))

(define @*
  (lambda (parser)
    (@or (@+ parser) (@const '()))))

(define @+
  (lambda (parser)
    (@and parser (lambda $ (apply (@* parser) $)))))
