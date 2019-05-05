#lang racket

(provide (all-defined-out))
(require racket/trace)
(require "utils.rkt")
(require "parsec.rkt")
(require "lexical.rkt")

(define echo-failure
  (lambda (tag)
    (lambda (k-fail)
      (lambda (tokens ctx)
        (begin (if (null? tokens) 
                   (printf "$~a: tokens is empty~n" tag) 
                   (printf "$~a: '~a' is not satisfied~n" tag (car tokens)))
               (k-fail tokens ctx))))))

(define $decorate
  (lambda (tag parser)
    (@decorate parser identity (echo-failure tag))))

(define $test
  (lambda (parser . strs)
    (define loop
      (lambda (str)
        (printf "case:\"~a\"~n" str)
        (parser (string->list str) (context-make)
          (lambda (t tokens ctx) (printf "parsed: \"~a\", remain: ~a, context: ~a~n~n" (flatten t) tokens ctx))
          (lambda (tokens ctx) (printf "remain: ~a, context: ~a~n~n" tokens ctx)))))
    (for-each loop strs)))

; ---------- test -----------
;($test $lower "a" "A")
;($test $upper "a" "A")
;($test $digit "a" "1")
;($test $letter "" "a" "aa" "aA" "Aa" "aaa" "AAA")
;($test $number "1" "+01230" "-00.1230" "1a2" "a12" "12a" "+a12" "00.a" "-0.1a" "00..0012")
;($test ($string "hello") "" "hell" "Hello" "hello" "hello world")
;($test ($string "你好") "你好" "你不好")
;($test $whitespace "a" " ab " "\n " "\t" " \t \n  ")
;($test $identifier "a" "abc" "let" "a12" "string->list" "i_var" "list<-string" "1abc" "*string" "--->")
;($test $comment "abc" "123\n" "???" ".\n?" "#a\nabc" "#ab#\n#aa" "abc#cd")
;($test ($string "->") "abc->def" "->ab" "ab->" "->" "-->")
;(pretty-display (kslang:scanner 
;  "let def-var = func (param-var1, ~param-var2, *param-var3) {
;     let var0 = nil, var1 = expr1, var2 = var1, var3 = 1, var4 = true, var5 = false # let binding
;     func-expr(param-var1 = param-val1, param-var2, var3, var4, var5) # func-call
;     cond { cond-expr -> { expr-true } else -> { expr-else } } # cond-expr
;   }"))
