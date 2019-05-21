#lang racket

(require "utils.rkt")
(require "lexical.rkt")
(require "parser.rkt")

(struct Val                         []                    #:transparent)
(struct Val:Nil           Val       []                    #:transparent)
(struct Val:Void          Val       []                    #:transparent)
(struct Val:Num           Val       [val]                 #:transparent)
(struct Val:Bool          Val       [val]                 #:transparent)
(struct Val:Func          Val       []                    #:transparent)
(struct Val:Func:Builtin  Val:Func  [func]                #:transparent)
(struct Val:Func:Externs  Val:Func  [vars body env]       #:transparent)

(struct Env [table prev] #:transparent)

(define run-file
  (lambda (file)
    (run (read-file file))))

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
       (let loop ([exprs exprs] [env (env-init!)] [val (Val:Nil)])
         (if (null? exprs)
           (cont val)
           (value-of-expr (car exprs) env fail
             (lambda (val env)
               (loop (cdr exprs) env val)))))])))

(define value-of-expr
  (lambda (expr env fail cont)
    (match expr
      [(AST:Expr:Num _ num)
       (cont (Val:Num num) env)]
      [(AST:Expr:Var idx var)
       (cont (env-apply env var idx) env)]
      [(AST:Expr:Func _ vars body)
       (cont (Val:Func:Externs vars body env) env)]
      [(AST:Expr:Let _ binds)
       (let ([env (env-extend env (table-make))])
         (let loop ([binds binds])
           (if (null? binds)
             (cont (Val:Void) env)
             (match (car binds)
               [(list var expr)
                (value-of-expr expr env fail
                  (lambda (val _)
                    (table-update! (env->table env) var val)
                    (loop (cdr binds))))]))))]
      [(AST:Expr:Call idx func argss)
       (value-of-expr func env fail
         (lambda (func-val _)
           (let loop1 ([argss argss] [func func-val])
             (if (null? argss)
               (cont func env)
               (let loop2 ([args (car argss)] [vals '()])
                 (if (null? args)
                   (value-of-call func (reverse vals) env fail
                     (lambda (val _)
                       (loop1 (cdr argss) val)))
                   (value-of-expr (car args) env fail
                     (lambda (arg-val env)
                       (loop2 (cdr args) (cons arg-val vals))))))))))]
      [(AST:Expr:Cond (Index line cursor) tests exprs)
       (let loop ([tests tests] [exprs exprs])
         (if (null? tests)
           (error 'AST:Expr:Cond "[~a:~a] all tests were failed" line cursor)
           (match (car tests)
             [(AST:Expr:Var _ 'else)
              (value-of-multi (car exprs) env fail cont)]
             [else
              (value-of-expr (car tests) env fail
                (lambda (t _)
                  (match t
                    [(Val:Bool #t) (value-of-multi (car exprs) env fail cont)]
                    [(Val:Bool #f) (loop (cdr tests) (cdr exprs))])))])))]
      [else
       123])))

(define value-of-call
  (lambda (func args env fail cont)
    (match func
      [(Val:Func:Externs vars body env)
       (let ([env (env-extend env (table-make))])
         (for-each (curry table-update! (env->table env)) vars args)
         (value-of-multi body env fail cont))]
      [(Val:Func:Builtin func)
       (func args env fail cont)])))

(define value-of-multi
  (lambda (exprs env fail cont)
    (let loop ([exprs exprs] [val (Val:Nil)])
      (if (null? exprs)
        (cont val env)
        (value-of-expr (car exprs) env fail
          (lambda (val _)
            (loop (cdr exprs) val)))))))

; ----- env ----- ;
(define table-make
  (lambda () (make-hasheqv)))

(define table-update!
  (lambda (tbl key val)
    (hash-set! tbl key val)))

(define env-init!
  (lambda () 
    (let ([tbl (table-make)])
      (table-update! tbl 'add add-func)
      (table-update! tbl 'sub sub-func)
      (table-update! tbl 'mul mul-func)
      (table-update! tbl 'div div-func)
      (table-update! tbl 'eq? eq?-func)
      (Env tbl (void)))))

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

; ----- built-in ------ ;
(define add-func
  (Val:Func:Builtin
    (lambda (args env fail cont)
      (cont (Val:Num (apply + (map Val:Num-val args))) env))))

(define sub-func
  (Val:Func:Builtin
    (lambda (args env fail cont)
      (cont (Val:Num (apply - (map Val:Num-val args))) env))))

(define mul-func
  (Val:Func:Builtin
    (lambda (args env fail cont)
      (cont (Val:Num (apply * (map Val:Num-val args))) env))))

(define div-func
  (Val:Func:Builtin
    (lambda (args env fail cont)
      (cont (Val:Num (apply / (map Val:Num-val args))) env))))

(define eq?-func
  (Val:Func:Builtin
    (lambda (args env fail cont)
      (cont (Val:Bool (apply equal? args)) env))))

; ------ helper ------ ;
(define error-report
  (lambda (env exn)
    (printf "err: ~a~n" (exn-message exn))
    (pretty-display env)))

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

;(test 
;  "" 
;  "10" 
;  "var1" 
;  "let a = 10, b = var2" 
;  "let a = 10, b = func (c, d) { add(a, d) }
;   let a = 20
;   b(1, 2)")

(run-file "test.ks")
