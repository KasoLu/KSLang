#lang racket

(require "parsec.rkt")
(require "lexical.rkt")
(require "utils.rkt")

(struct Program [exprs index] #:transparent)
(struct Expr [expr index] #:transparent)
(struct Num [val] #:transparent)
(struct Var [val] #:transparent)
(struct Let [vars exprs] #:transparent)
(struct Func [vars body] #:transparent)
(struct Call [func vars] #:transparent)

(define *keywords* '("let" "func" "cond" "else"))

(define report-parse-error
  (lambda (tag t)
    (match t
      [(Token val (Index line cursor))
       (error tag "[~a:~a]: '~a' is not satisfied" line cursor val)]
      [else
       (error tag "'~a' is not a token" t)])))

;(define report-empty-error
;  (lambda (tag)
;    (error tag "tokens is empty")))

(@:: (@~ str)
     (lambda (ts cont)
       (if (null? ts)
         (cont #f ts)
         (match (car ts)
           [(Token text idx)
            (if (equal? text str)
              (cont idx (cdr ts))
              (cont #f ts))]
           [else
            (report-parse-error '@~ (car ts))]))))

(@:: (@_ str)
     (@skip (@~ str)))

(@:: (@number func)
     (lambda (ts cont)
       (if (null? ts)
         (cont #f ts)
         (match (car ts)
           [(Token num idx) 
            (if (number? num)
              (cont (func num idx) (cdr ts))
              (cont #f ts))]
           [else (report-parse-error 'number (car ts))]))))

(@:: (@identifier func)
     (lambda (ts cont)
       (if (null? ts)
         (cont #f ts)
         (match (car ts)
           [(Token text idx) 
            (if (char-alphabetic? (string-ref text 0))
              (if (member text *keywords*)
                (report-parse-error 'identifier (car ts))
                (cont (func (string->symbol text) idx) (cdr ts)))
              (cont #f ts))]
           [else (report-parse-error 'identifier (car ts))]))))

(@:: (@.* p sep)
     (@? (@.+ p sep)))

(@:: (@.+ p sep)
     (@cat p (@* (@cat (@skip sep) p))))

($:: ($number)
     (@number (lambda (num idx) num)))

($:: ($identifier)
     (@identifier (lambda (id idx) id)))

; program ::= <expr>+
($:: ($program)
     (@succ (@+ ($expr))
       (lambda (exprs)
         (@const (Program exprs (Expr-index (car exprs)))))))

($:: ($expr)
     (@opt ($expr-num) ($expr-var) ($expr-let) ($expr-func) ($expr-call)))

; multi-expr ::= { <Expression>* }
($:: ($expr-multi)
     (@cat (@_ "{") (@* ($expr)) (@_ "}")))

; expr ::= <number>
($:: ($expr-num)
     (@number 
       (lambda (num idx)
         (Expr (Num num) idx))))

; expr ::= <identifier>
($:: ($expr-var)
     (@identifier
       (lambda (id idx)
         (Expr (Var id) idx))))

; expr ::= let <Identifier = Expression>+(,)
($:: ($expr-let)
     (@succ (@seq (@~ "let")
                  (@.+ ($expr-let-bind) (@_ ",")))
            (lambda (ts)
              (match ts
                [(list idx bind)
                 (let loop ([bind bind] [vars '()] [exprs '()])
                   (cond [(null? bind) (@const (Expr (Let (reverse vars) (reverse exprs)) idx))]
                         [(symbol? (car bind)) (loop (cdr bind) (cons (car bind) vars) exprs)]
                         [else (loop (cdr bind) vars (cons (car bind) exprs))]))]))))

($:: ($expr-let-bind)
     (@cat ($identifier) (@_ "=") ($expr)))

; expr ::= func ( <Identifier>*(,) ) MutliExpression
($:: ($expr-func)
     (@succ (@seq (@~ "func") ($expr-func-vars) ($expr-multi))
            (lambda (ts)
              (match ts
                [(list idx vars body)
                 (@const (Expr (Func vars body) idx))]))))

($:: ($expr-func-vars)
     (@cat (@_ "(") (@.* ($identifier) (@_ ",")) (@_ ")")))

; expr ::= Expression <( <Expression>*(,) )>+
($:: ($expr-call)
     (@succ (@seq ($expr-call-func) (@+ ($expr-call-args)))
            (lambda (ts)
              (match ts
                [(list func argss)
                 (@const (foldl (lambda (args func) (Expr (Call func args) (Expr-index func))) func argss))]))))

($:: ($expr-call-func)
     (@opt ($expr-var) ($expr-func)))

($:: ($expr-call-args)
     (@flat (@seq (@_ "(") (@.* ($expr) (@_ ",")) (@_ ")"))))

;(define parse
;  (lambda (text)
;    (scan text
;      (lambda (ts)
;        (($program) ts
;          (lambda (ast)
;            (pretty-display ast)))))))

(define-syntax @test
  (syntax-rules ()
    [(_ p strs ...)
     (for-each
       (lambda (str) 
         (printf "cases: \"~a\" -> ~a~n" str (quote p))
         (scan str
               (lambda (ts)
                 (p ts 
                    (lambda (ast ts)
                      (pretty-display ast)))))
         (printf "~n"))
       (list strs ...))]))

;(@test (@~ "let") "" "let" "func" "test")
;(@test (@_ "let") "" "let" "func" "test")
;(@test (@.* (@~ "let") (@~ ",")) "" "let" "let, let" "let,,let" "let,")
;(@test (@.+ (@~ "let") (@~ ",")) "" "let" "let, let" "let,,let" "let,")
;(@test ($expr-num) "10" "20")
;(@test ($expr-var) "var1" "var2")
;(@test ($expr-let) "let var1 = 10" "let var1 = 10, var2 = 20" "let" "let var1, var2 = 20" "let var1 = 10 var2 = 20")
;(@test ($expr-func) "func" "func var1, var2 {}" "func (var1) {}" "func (var1 var2) {}" "func (var1, var2) {10 20}")
;(@test ($expr-call) "var1(var2)" "var1(var2, var3)(var4, var5)(var6, var7)" "var1(var2 var3)")
;(parse
;"
;let def-var = func (param-var1, param-var2, param-var3) {
;  let var0 = 1, var2 = var0, var3 = func (v1, v2) { add(v1, v2) }
;  var3(var0, add(10, 20))
;}
;")

