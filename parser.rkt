#lang racket

(require "parsec.rkt")
(require "lexical.rkt")
(require "utils.rkt")

; ------ struct ------ ;
(struct AST                       []          #:transparent)
(struct AST:Prgm        AST       [exprs]     #:transparent)
(struct AST:Expr        AST       [index]     #:transparent)
(struct AST:Expr:Num    AST:Expr  [val]       #:transparent)
(struct AST:Expr:Var    AST:Expr  [val]       #:transparent)
(struct AST:Expr:Let    AST:Expr  [binds]     #:transparent)
(struct AST:Expr:Func   AST:Expr  [vars body] #:transparent)
(struct AST:Expr:Call   AST:Expr  [func vars] #:transparent)

(struct Error [tag message] #:transparent)

; ------ data ------- ;
(define *keywords* (list "let" "func" "cond" "else"))

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
         ($expr-let)
         ($expr-func)
         ($expr-call)
         ($expr-var)
         ($expr-num)
         )
       (error-handle 'expression)))

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
            (error-handle 'expr-let)))
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
            (error-handle 'expr-func)))
     ($:: ($expr-func-body)
          (@catch
            (@seq (@_ "{") (@* ($expr)) (@_ "}"))
            (error-handle 'expr-func)))
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
            (@seq (@_ "(") (@.* ($expr) (@_ ",")) (@_ ")"))
            (error-handle 'expr-call)))
     (@succ 
       (@seq ($expr-call-func) ($expr-call-args))
       (lambda (ts)
         (match ts
           [(list func (list _ args _))
            (@const (AST:Expr:Call (AST:Expr-index func) func args))]))))

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

(define report-empty-error
  (lambda (kind err)
    (let ([tag (if (not err) kind (Error-tag err))])
      (Error kind (format "expect '~a' but tokens was ended" tag)))))

(define report-parse-error
  (lambda (kind err val line cursor)
    (let ([tag (if (not err) kind (Error-tag err))])
      (Error kind (format "~a:~a: expect '~a' but found '~a'" line cursor tag val)))))

(define error-handle
  (lambda (kind)
    (lambda (err rs)
      (if (null? rs)
        (report-empty-error kind err)
        (match (car rs)
          [(Token (Index line cursor) val)
           (report-parse-error kind err val line cursor)])))))

;--------- test ----------;
(define-syntax @test
  (syntax-rules ()
    [(_ p strs ...)
     (for-each
       (lambda (str) 
         (printf "cases: \"~a\" -> ~a~n" str (quote p))
         (scan str
           (lambda (ts)
             (p ts
                (lambda (val ts) (printf "parse: ~a~nrests: ~a~n" val ts))
                (lambda (err ts) (printf "error: ~a~nrests: ~a~n" err ts)))))
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
;(@test ($program) 
;       "" "a" "10" "func" "let a = 10 b = 10" "var(10 20)"
;       "let b = func (c) { let d = 1, e = func (f, g) { h(i, j) } l(m, n) }")
