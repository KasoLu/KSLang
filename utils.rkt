#lang racket

(provide (all-defined-out))
(require racket/trace)

;(define-syntax lambda
;  (syntax-rules ()
;    [(_ kw body ...) (trace-lambda kw body ...)]))

(define test
  (lambda (parser . strs)
    (define loop
      (lambda (str)
        (printf "case:\"~a\"~n" str)
        (parser (string->list str) '() 
          (lambda (k-succ tokens ctx) (k-succ (car tokens) (cdr tokens) ctx))
          (lambda (t tokens ctx) (printf "parsed: \"~a\", remain: ~a, context: ~a~n~n" (flatten t) tokens ctx))
          (lambda (tokens ctx) (printf "remain: ~a, context: ~a~n~n" tokens ctx)))))
    (for-each loop strs)))
