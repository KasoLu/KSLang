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
  (pipe list->string string->symbol))

(define chars->number
  (pipe list->string string->number))

(define read-file
  (lambda (filename)
    (let ([port (open-input-file filename #:mode 'text)])
      (let loop ([line (read-line port)] [all ""])
        (cond
          [(eof-object? line) all]
          [else
           (loop (read-line port)
                 (string-append all line "\n"))])))))
