#lang racket

(require "parsec.rkt")
(require "lexical.rkt")
(require "utils.rkt")

;; ===== struct ===== ;;
(struct Program [exprs index] #:transparent)
(struct Expr [expr index] #:transparent)
(struct Num [val] #:transparent)
(struct Var [val] #:transparent)
(struct Let [vars exprs] #:transparent)
(struct Func [vars body] #:transparent)
(struct Call [func vars] #:transparent)

;; ===== data ===== ;;
(define *keywords* '("let" "func" "cond" "else"))
(define *shown-error* "")

(define parse
  (lambda (text cont)
    (scan text
      (lambda (ts)
        (($program) ts
          (lambda (ast ts)
            (if (or (pair? ts) (not ast))
              (display *shown-error*)
              (cont ast))))))))

;; ===== func parser ===== ;;
(@:: (@~ str)
     (lambda (ts cont)
       (if (null? ts)
         (begin (set-empty-error! '@~)
                (cont #f ts))
         (match (car ts)
           [(Token text idx)
            (if (equal? text str)
              (cont idx (cdr ts))
              (cont #f ts))]
           [else
            (set-not-token-error! '@~ (car ts))
            (cont #f ts)]))))

(@:: (@_ str)
     (@skip (@~ str)))

(@:: (@number func)
     (lambda (ts cont)
       (if (null? ts)
         (begin (set-empty-error! '@number)
                (cont #f ts))
         (match (car ts)
           [(Token val idx) 
            (if (number? val)
              (cont (func val idx) (cdr ts))
              (cont #f ts))]
           [else 
            (set-not-token-error! '@number (car ts))
            (cont #f ts)]))))

(@:: (@identifier func)
     (lambda (ts cont)
       (if (null? ts)
         (begin (set-empty-error! '@identifier)
                (cont #f ts))
         (match (car ts)
           [(Token val (Index line cursor)) 
            (if (string? val)
              (if (char-alphabetic? (string-ref val 0))
                (if (member val *keywords*)
                  (begin (set-parse-error! (format "@identifier - [~a:~a]: '~a' is a keyword~n" line cursor val)) 
                         (cont #f ts))
                  (cont (func (string->symbol val) (Index line cursor)) (cdr ts)))
                (cont #f ts))
              (cont #f ts))]
           [else 
            (set-not-token-error! '@identifier (car ts))
            (cont #f ts)]))))

(@:: (@.* p sep)
     (@? (@.+ p sep)))

(@:: (@.+ p sep)
     (@cat p (@* (@cat (@skip sep) p))))

;; ===== parser ===== ;;
($:: ($program)
     (@succ (@+ ($expr))
       (lambda (exprs)
         (@const (Program exprs (Expr-index (car exprs)))))))

($:: ($expr)
     (@alt ($expr-num)
           ($expr-var)
           ($expr-let)
           ($expr-func)
           ($expr-call)))

($:: ($expr-multi)
     (@cat (@_ "{") (@* ($expr)) (@_ "}")))

($:: ($expr-num)
     (@number 
       (lambda (num idx)
         (Expr (Num num) idx))))

($:: ($expr-var)
     (@identifier
       (lambda (id idx)
         (Expr (Var id) idx))))

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

($:: ($expr-func)
     (@succ (@seq (@~ "func") ($expr-func-vars) ($expr-multi))
            (lambda (ts)
              (match ts
                [(list idx vars body)
                 (@const (Expr (Func vars body) idx))]))))

($:: ($expr-func-vars)
     (@cat (@_ "(") (@.* ($identifier) (@_ ",")) (@_ ")")))

($:: ($expr-call)
     (@succ (@seq ($expr-call-func) ($expr-call-args))
            (lambda (ts)
              (match ts
                [(list func args)
                 (@const (Expr (Call func args) (Expr-index func)))]))))

($:: ($expr-call-func)
     (@alt ($expr-var) ($expr-func) ($expr-call)))

($:: ($expr-call-args)
     (@succ (@seq (@~ "(") (@.* ($expr) (@_ ",")) (@~ ")"))
            (lambda (v)
              (match v
                [(list _ args _) (@const args)]))))

($:: ($number)
     (@number (lambda (num idx) num)))

($:: ($identifier)
     (@identifier (lambda (id idx) id)))

;; ===== helper ===== ;;
(define set-parse-error!
  (lambda (msg)
    (set! *shown-error* msg)))

(define set-empty-error!
  (lambda (tag)
    (set! *shown-error* (format "~a: tokens is empty~n" tag))))

(define set-not-token-error!
  (lambda (tag t)
    (set! *shown-error* (format "~a: '~a' is not a token~n" tag t))))

;; ===== test ===== ;;
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
                      (printf "parse: ~a~nrests: ~a~n" ast ts)))))
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
;(@test ($program) "let b = func (c) { let d = 1, e = func (f, g) { h(i, j) } l(m, n) }")
;(@test ($expr) "b")
;(@test ($expr-multi) "{ b(c) }")
;(parse "b(c)" (lambda (ast) (pretty-display ast)))
(parse
"
let def-var = func (param-var1, param-var2, param-var3) {
  let var0 = 1, var2 = var0, var3 = func (v1, v2) { add(v1, v2) }
  var3(var0, add(10, 20))
  var4(a)(b, c)
}
"
(lambda (ast)
  (pretty-display ast)))

