#lang racket

(require "lexical.rkt")
(require "parser.rkt")

(struct Val            []               #:transparent)
(struct Val:Nil   Val  []               #:transparent)
(struct Val:Num   Val  [val]            #:transparent)
(struct Val:Bool  Val  [val]            #:transparent)
(struct Val:Func  Val  [vars body env]  #:transparent)
(struct Val:Void  Val  []               #:transparent)

(struct Env [table prev] #:transparent)

(define run
  (lambda (text)
    (parse text 
      (lambda (ast) 
        (value-of-prgm ast (void)
          (lambda (val) (pretty-display val)))))))

(define value-of-prgm
  (lambda (prgm fail cont)
    (match prgm
      [(AST:Prgm exprs)
       (let loop ([exprs exprs] [env (env-init)] [val (Val:Nil)])
         (if (null? exprs)
           (cont val)
           (value-of-expr (car exprs) env fail
             (lambda (val)
               (loop (cdr exprs) env val)))))])))

(define value-of-expr
  (lambda (expr env fail cont)
    (match expr
      [(AST:Expr:Num _ num)
       (cont (Val:Num num))]
      [(AST:Expr:Var idx var)
       (cont (env-apply env var idx))]
      [(AST:Expr:Func _ vars body)
       (cont (Val:Func vars body env))]
      [(AST:Expr:Let _ binds)
       (let ([tbl (env->table env)])
         (let loop ([binds binds])
           (if (null? binds)
             (cont (Val:Void))
             (match (car binds)
               [(list var expr)
                (value-of-expr expr (env-extend env tbl) fail
                  (lambda (val)
                    (table-update! tbl var val)
                    (loop (cdr binds))))]))))]
      [(AST:Expr:Call idx func args)
       (value-of-expr func env fail
         (lambda (func-val)
           (let loop ([args args] [vals '()])
             (if (null? args)
               (value-of-call func-val (reverse vals) fail cont)
               (value-of-expr (car args) env fail
                 (lambda (arg-val)
                   (loop (cdr args) (cons arg-val vals))))))))]
      )))

(define value-of-call
  (lambda (func args fail cont)
    (match func
      [(Val:Func vars body env)
       (let ([tbl (table-make)])
         (for-each (curry table-update! tbl) vars args)
         (let loop ([body body] [val (Val:Nil)])
           (if (null? body)
             (cont val)
             (value-of-expr (car body) (env-extend env tbl) fail
               (lambda (val)
                 (loop (cdr body) val))))))])))

; ----- env ----- ;
(define table-make
  (lambda () (make-hasheqv)))

(define table-update!
  (lambda (tbl key val)
    (hash-set! tbl key val)))

(define env-init
  (lambda () (Env (table-make) (void))))

(define env-extend
  (lambda (env tbl)
    (Env tbl env)))

(define env-apply
  (lambda (env key idx)
    (let loop ([env env])
      (if (void? env)
        (match idx
          [(Index line cursor)
           (error 'env-apply "[~a:~a] '~a' not binding" line cursor key)])
        (match env
          [(Env tbl prev)
           (hash-ref tbl key (lambda () (loop prev)))])))))

(define env->table
  (lambda (env)
    (if (void? env)
      (error 'env->table "env is empty")
      (Env-table env))))

; ------ test ------ ;
(define-syntax test
  (syntax-rules ()
    [(_ text ...)
     (for-each 
       (lambda (str) 
         (printf "cases: \"~a\"~n" str)
         (with-handlers ([exn:fail? (lambda (exn) (pretty-display (exn-message exn)))])
           (run str))
         (printf "~n"))
       (list text ...))]))

(test 
  "" 
  "10" 
  "var1" 
  "let a = 10, b = var2" 
  "let a = 10, b = func (a, b) { 20 }
   b(1, 2)")
