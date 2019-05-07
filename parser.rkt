#lang racket

(require "utils.rkt")
(require "parsec.rkt")
(require "lexical.rkt")

;; ----- struct -----
(struct AST-prgm [exps] #:transparent)
(struct AST-num [val] #:transparent)
;(struct AST-bool [val] #:transparent)
(struct AST-func [vars body] #:transparent)
(struct AST-let [vars exps body] #:transparent)

;; ----- helper -----
(define *keywords* '(let func cond else))
(define *literal* '(true false))

(define report-parse-error
  (lambda (tag t line column)
    (error tag "[~a:~a] '~a' is not satified~n" line column t)))

;(define context-extract-line-column
;  (lambda (ctx)
;    (list (context-get 'line ctx) (context-get 'column ctx))))

(define curr-token car)
(define rest-token cdr)

;; ----- compound parser -----
(define $parse
  (lambda (pred succ fail)
    (let ([parser (@token (lambda (tokens) (pred (Lex-token-val (curr-token tokens)))))]
          [d-succ (lambda (k-succ) (lambda (t tokens ctx) (k-succ (succ t) tokens ctx)))])
      (@decorate parser identity identity d-succ
        (lambda (tokens ctx)
          (let ([line (context-get 'line ctx)] [column (context-get 'column ctx)])
            (if (null? tokens)
              (error '$parse "tokens is empty")
              (fail (Lex-token-val (car tokens)) line column))))))))

;(define $program ;;= <Expression>+
;  (@success (@+ $expr)
;    (lambda (t)
;      (@const (AST-prgm t)))))

;(define $expr ;;= <expr-num | expr-bool | expr-func | expr-let>
;  (@success (@or )))

;(define $expr-num ;;= <number>
;  (@token 
;    (lambda (t)
;      (match t
;        [(Lex-token val line column)
;         (if (number? val)
;           (AST-num val)
;           (report-parse-error 'number val line column))]))))
(define $expr-num
  ($parse number?
    (lambda (val) (AST-num val))
    (lambda (tokens ctx)
      (if (null? tokens)
        (report-parse-error 'number )))))

;(define $expr-func ;;= func ( <identifier>* ) expr
;  (@and ($item (lambda))))

