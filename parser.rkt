#lang racket

(require "parsec.rkt")
(require "lexical.rkt")
(require "utils.rkt")

(provide parse (all-defined-out))

; ------ struct ------ ;
(struct AST                       []              #:transparent)
(struct AST:Prgm        AST       [exprs]         #:transparent)
(struct AST:Expr        AST       [index]         #:transparent)
(struct AST:Expr:Num    AST:Expr  [val]           #:transparent)
(struct AST:Expr:Var    AST:Expr  [val]           #:transparent)
(struct AST:Expr:Let    AST:Expr  [binds]         #:transparent)
(struct AST:Expr:Func   AST:Expr  [vars body]     #:transparent)
(struct AST:Expr:Call   AST:Expr  [func vars]     #:transparent)
(struct AST:Expr:Cond   AST:Expr  [tests exprs]   #:transparent)

(struct Error [tag message] #:transparent)

; ------ data ------- ;
(define *keywords* (list "let" "func" "cond" "else"))

; ------ func ------- ;
(define parse
  (lambda (tokens cont)
    (scan tokens
      (lambda (ts)
        (($program) ts
         (lambda (ast rs) (cont ast))
         (lambda (err rs) (error (Error-message err))))))))

; ------ parser ------ ;
($:: ($program)
     (lambda (ts k-succ k-fail)
       (let loop ([ts ts] [exprs '()])
         (($expr) ts
          (lambda (t rs)
            (let ([exprs (cons t exprs)])
              (if (null? rs)
                (k-succ (AST:Prgm (reverse exprs)) rs)
                (loop rs exprs))))
          k-fail))))

($:: ($expr)
     (@catch 
       (@opt
         ($expr-call)
         ($expr-let)
         ($expr-func)
         ($expr-cond)
         ($expr-var)
         ($expr-num)
         )
       (error-handle 'throw 'expression)))

($:: ($expr-num)
     (@number AST:Expr:Num))

($:: ($expr-var)
     (@identifier AST:Expr:Var))

($:: ($expr-let)
     ($:: ($expr-let-begin)
          (@~ "let"))
     ($:: ($expr-let-body)
          (@catch
            (@.+ (@cat ($identifier) (@_ "=") ($expr))
                 (@_ ","))
            (error-handle 'abort 'expr-let)))
     (@succ
       (@seq ($expr-let-begin) ($expr-let-body))
       (lambda (ts)
         (@const (apply AST:Expr:Let ts)))))

($:: ($expr-func)
     ($:: ($expr-func-begin)
          (@~ "func"))
     ($:: ($expr-func-vars)
          (@catch
            (@cat (@_ "(") (@.* ($identifier) (@_ ",")) (@_ ")"))
            (error-handle 'abort 'expr-func)))
     ($:: ($expr-func-body)
          (@catch
            (@seq (@_ "{") (@* ($expr)) (@_ "}"))
            (error-handle 'abort 'expr-func)))
     (@succ 
       (@seq ($expr-func-begin) ($expr-func-vars) ($expr-func-body))
       (lambda (ts)
         (match ts
           [(list idx (list vars) (list _ body _))
            (@const (AST:Expr:Func idx vars body))]))))

($:: ($expr-call)
     ($:: ($expr-call-func)
          (@try 'expr-call-func
            (@opt ($expr-var)
                  ($expr-func))))
     ($:: ($expr-call-args)
          (@catch
            (@+ (@seq (@_ "(") (@.* ($expr) (@_ ",")) (@_ ")")))
            (error-handle 'throw 'expr-call)))
     (@succ 
       (@seq ($expr-call-func) ($expr-call-args))
       (lambda (ts)
         (match ts
           [(list func argss)
            (let loop ([argss argss] [vals '()])
              (if (null? argss)
                (@const (AST:Expr:Call (AST:Expr-index func) func (reverse vals)))
                (match (car argss)
                  [(list _ args _) (loop (cdr argss) (cons args vals))])))]
           [(list func (list _ args _))
            (@const (AST:Expr:Call (AST:Expr-index func) func args))]))))

($:: ($expr-cond)
     ($:: ($expr-cond-begin)
          (@~ "cond"))
     ($:: ($expr-cond-test)
          (@catch
            (@seq (@_ "{") 
                  (@* (@cat (@opt (@~ "else") ($expr))
                            (@_ "->")
                            ($expr-cond-body)))
                  (@_ "}"))
            (error-handle 'abort 'expr-cond)))
     ($:: ($expr-cond-body)
          (@catch
            (@seq (@_ "{") (@* ($expr)) (@_ "}"))
            (error-handle 'abort 'expr-cond)))
     (@succ 
       (@seq ($expr-cond-begin) ($expr-cond-test))
       (lambda (t)
         (match t
           [(list idx (list _ tests _))
            (let loop ([tests tests] [test-exprs '()] [body-exprs '()])
              (if (null? tests)
                (@const (AST:Expr:Cond idx (reverse test-exprs) (reverse body-exprs)))
                (match (car tests)
                  [(list idx/expr (list _ exprs _))
                   (let ([else/expr (if (Index? idx/expr) (AST:Expr:Var idx/expr 'else) idx/expr)])
                     (loop (cdr tests) (cons else/expr test-exprs) (cons exprs body-exprs)))])))]))))

($:: ($number)
     (@number (lambda (idx num) num)))

($:: ($identifier)
     (@identifier (lambda (idx id) id)))

; ------ func parser ------ ;
(@:: (@try tag p)
     (lambda (vs k-succ k-fail)
       (p vs k-succ (lambda (e rs) (k-fail (Error tag #f) rs)))))

(@:: (@catch p func)
     (lambda (vs k-succ k-fail)
       (p vs k-succ (lambda (e rs) (k-fail (func e rs) rs)))))

(@:: (@~ str)
     (lambda (ts k-succ k-fail)
       (match ts
         [(list (Token idx (? (curry equal? str) val)) rs ...)
          (k-succ idx rs)]
         [_ (k-fail (Error str "") ts)])))

(@:: (@_ str)
     (@skip (@~ str)))

(@:: (@number func)
     (lambda (ts k-succ k-fail)
       (match ts
         [(list (Token idx (? number? val)) rs ...)
          (k-succ (func idx val) rs)]
         [_ (k-fail (Error 'number "") ts)])))

(@:: (@identifier func)
     (lambda (ts k-succ k-fail)
       (match ts
         [(list (Token idx (? identifier? val)) rs ...)
          (k-succ (func idx (string->symbol val)) rs)]
         [_ (k-fail (Error 'identifier "") ts)])))

; ------ helper ------ ;
(define identifier?
  (lambda (val)
    (and (string? val)
         (char-alphabetic? (string-ref val 0))
         (not (member val *keywords*)))))

(define handle-empty-error
  (lambda (means kind err)
    (let ([tag (if (not err) kind (Error-tag err))])
      (let ([msg (format "~a: expect '~a' but tokens was ended" kind tag)])
        (match means
          ['throw (Error kind msg)]
          ['abort (error msg)])))))

(define handle-parse-error
  (lambda (means kind err val line cursor)
    (let ([tag (if (not err) kind (Error-tag err))])
      (let ([msg (format "~a:~a:~a: expect '~a' but found '~a'" kind line cursor tag val)])
        (match means
          ['throw (Error kind msg)]
          ['abort (error msg)])))))

(define error-handle
  (lambda (means kind)
    (lambda (err rs)
      (if (null? rs)
        (handle-empty-error means kind err)
        (match (car rs)
          [(Token (Index line cursor) val)
           (handle-parse-error means kind err val line cursor)])))))

;--------- test ----------;
(define-syntax @test
  (syntax-rules ()
    [(_ p strs ...)
     (for-each
       (lambda (str) 
         (printf "cases: \"~a\" -> ~a~n" str (quote p))
         (scan str
           (lambda (ts)
             (with-handlers ([exn:fail? (lambda (exn) (pretty-display (exn-message exn)))])
               (p ts
                  (lambda (val ts) (printf "parse: ~a~nrests: ~a~n" val ts))
                  (lambda (err ts) (printf "error: ~a~nrests: ~a~n" err ts))))))
         (printf "~n"))
       (list strs ...))]))

;(@test (@~ "let") "" "let" "func" "test")
;(@test (@_ "let") "" "let" "func" "test")
;(@test ($expr-num) "" "10" "a" "20" "30b")
;(@test ($expr-var) "" "var1" "10" "var2")
;(@test ($expr-let) 
;       "let" "var1 = 10, var2 = 20" "let var1, var2 = 20" "let var1 = 10 var2 = 20" "let var1 = 10,, var2 = 20"
;       "let var1 = 10" "let var1 = 10, var2 = 20"
;       )
;(@test ($expr-func-begin)
;       "" "fun" "func" "func1" "funn")
;(@test ($expr-func-vars) 
;       #;"var1" #;"(var1" #;"var1)" #;"(var1 var2)" #;"(var1, var2"
;       "(var1)" "(var1, var2)")
;(@test ($expr-func-body)
;       #;"10" #;"{ 10" #;"10 }" #;"{ 10 , 20 }" #;"{ { 10 }" #;"{ 10 { }}"
;       "{ }" "{ 10 }" "{ 10 20 }")
;(@test ($expr-func)
;       "func" "fun (var1, var2) {}" "func (var1 var2) {}" "func (var1, var2 {}" "func (var1, var2) { 10 { 20 }" 
;       "func (var1, var2) { }" "func (var1, var2) { 10 20 }")
;(@test ($expr-call-func) 
;        "10" "func (var1 var2) {}"
;        "var1" "func (var1, var2) {}")
;(@test ($expr-call-args)
;       "var" "(var" "var)" "(var1 var2)" "(var1,, var2)" "(, var2)" "(var1, )" "(,)"
;       "()" "(var1)" "(var1, var2)")
;(@test ($expr-call)
;       "" "10" "func (var1, var2) { 10 } (var2, var3)")
;(@test ($expr)
;       "func (var1, var2) { 10 } (var2, var3)")
;(@test ($program) 
;       "" "a" "10" "func" "let a = 10 b = 10" "var(10 20)"
;       "let b = func (c) { let d = 1, e = func (f, g) { h(i, j) } l(m, n) }")
;(@test ($expr-cond)
;       "" "cond" "cond { var -> { 10 } else -> { 20 } }")
;(parse 
;"let b = func (c) { let d = 1, e = func (f, g) { h(i, j) } l(m, n) }"
;(lambda (ast) (pretty-display ast)))
