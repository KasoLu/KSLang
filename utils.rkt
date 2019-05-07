#lang racket

(provide (all-defined-out))
(require racket/trace)

;(define-syntax lambda
;  (syntax-rules ()
;    [(_ kw body ...) (trace-lambda kw body ...)]))

(define pipe
  (lambda funcs
    (lambda (v)
      (foldl (lambda (f v) (f v)) v funcs))))

(define chars->symbol
  (pipe flatten list->string string->symbol))

(define chars->number
  (pipe flatten list->string string->number))
