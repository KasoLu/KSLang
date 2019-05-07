#lang racket

(require racket/trace)
(require "utils.rkt")
(require "opc.rkt")
;(require "lexical.rkt")

;(struct Token [stashs parsed remain] #:transparent)

;(define $test
;  (lambda (p . strs)
;    (let ([show (lambda (v) (match v [(Token s p r) (printf "parsed: ~a, remain: ~a~n" (reverse p) r)]))])
;      (define loop
;        (lambda (str)
;          (printf "case:\"~a\"~n" str)
;          (p (Token '() '() (string->list str)) show show)
;          (printf "~n")))
;      (for-each loop strs))))

;(define $->x
;  (lambda (p func)
;    (@succ p
;      (lambda (v)
;        (match v
;          [(Token stashs parsed remain)
;           (let ([symbol (func (reverse stashs))])
;             (@const (Token '() (cons symbol parsed) remain)))])))))

;(define $->symbol
;  (lambda (p)
;    ($->x p chars->symbol)))

;(define $->number
;  (lambda (p)
;    ($->x p chars->number)))

;;; <'-' | '+'>? digit+ <'.' digit+>?
;(define $number
;  (let ([head (@? ($option "-" "+"))]
;        [body (@+ $digit)]
;        [tail (@? (@seq ($string ".") (@+ $digit)))])
;    (@seq head body tail)))

;(define $whitespace
;  ($item char-whitespace?))

;;; letter <letter | digit | "-" | "_" | ">" | "<">* <"?" | "!">?
;(define $identifier
;  (let ([head $letter]
;        [body (@* (@opt $letter $digit ($option "-" "_" ">" "<")))]
;        [tail (@? ($option "?" "!"))])
;    (@seq head body tail)))

;($test ($->symbol $lower) "a" "A")
;($test ($->symbol $upper) "a" "A")
;($test ($->symbol $digit) "a" "1")
;($test ($->symbol $letter) "" "a" "aa" "aA" "Aa" "aaa" "AAA")
;($test ($->symbol ($char #\a)) "" "a" "aa" "aA" "Aa" "aaa" "AAA")
;($test ($->symbol ($string "hello")) "" "hell" "Hello" "hello" "hello world")
;($test ($->number $number) "1" "+01230" "-00.1230" "1a2" "a12" "12a" "+a12" "00.a" "-0.1a" "00..0012")
;($test ($->symbol ($string "你好")) "你好不" "你不好")
;($test ($->symbol $whitespace) "a" " ab " "\n " "\t" " \t \n  ")
;($test ($->symbol $identifier) "a" "abc" "let" "a12" "string->list" "i_var" "list<-string" "1abc" "*string" "--->")
;($test $comment "abc" "123\n" "???" ".\n?" "#a\nabc" "#ab#\n#aa" "abc#cd")
;($test ($string "->") "abc->def" "->ab" "ab->" "->" "-->")
;($test (@and $digit $digit $digit) "123" "12a" "abc" "a12" "1a2")
;(pretty-display (kslang:scanner
;  "let def-var = func (param-var1, ~param-var2, *param-var3) {
;     let var0 = nil, var1 = expr1, var2 = var1, var3 = 1, var4 = true, var5 = false # let binding
;     func-expr(param-var1 = param-val1, param-var2, var3, var4, var5) # func-call
;     cond { cond-expr -> { expr-true } else -> { expr-else } } # cond-expr
;   }"))

