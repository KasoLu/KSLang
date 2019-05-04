#lang racket

(provide (all-defined-out))
(require racket/trace)

;(define-syntax lambda
;  (syntax-rules ()
;    [(_ kw body ...) (trace-lambda kw body ...)]))

(define context-make
  (lambda () 
    (make-hash)))

(define context-get
  (lambda (ctx key) 
    (hash-ref ctx key 
      (lambda () (error 'context-get "'~a' not found in ~a" key ctx)))))

(define context-set!
  (lambda (ctx key val) 
    (hash-set! ctx key val)))
