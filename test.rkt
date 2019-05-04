#lang racket

(provide (all-defined-out))
(require "utils.rkt")
(require "parsec.rkt")

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
        (printf "case: ~a~n" str)
        (parser (string->list str) (context-make)
          (lambda (t tokens ctx) (printf "parsed: ~a, remain: ~a, context: ~a~n~n" (flatten t) tokens ctx))
          (lambda (tokens ctx) (printf "remain: ~a, context: ~a~n~n" tokens ctx)))))
    (for-each loop strs)))

(define $range
  (lambda (start end)
    (@token (lambda (x) (and (char<=? start x) (char<=? x end))))))

(define $option
  (lambda opts
    (@token (lambda (x) (memq x opts)))))

(define $char
  (lambda (c)
    (@token (lambda (x) (char=? c x)))))

(define $lower
  ($range #\a #\z))

(define $upper
  ($range #\A #\Z))

(define $digit
  ($range #\0 #\9))

(define $letter
  (@or $lower $upper))

(define $token->x
  (lambda (parser func)
    (@decorate parser
      (lambda (k-succ)
        (lambda (t tokens ctx)
          (k-succ (func t) tokens ctx)))
      identity)))

(define $token->number
  (lambda (parser)
    ($token->x parser (lambda (t) (string->number (list->string (flatten t)))))))

(define $token->symbol
  (lambda (parser)
    ($token->x parser (lambda (t) (string->symbol (list->string (flatten t)))))))

(define $token->skip
  (lambda (parser)
    ($token->x parser (lambda (t) '()))))

(define $string
  (lambda (str)
    (apply @and (map $char (string->list str)))))

;; (- | +)? digit+ (. digit+)?
(define $number
  (let ([sign (@? (@or ($char #\-) ($char #\+)))]
        [integral (@+ $digit)]
        [factional (@? (@and ($char #\.) (@+ $digit)))])
    ($token->number (@and sign integral factional))))

;; symbol ::= '-' | '_' | '>' | '<'
;; postfix ::= '?' | '!' | "..."
;; identifier ::= letter (letter | symbol | digit)* (signal)?
(define $identifier
  (let ([symbol ($option #\- #\_ #\> #\<)] [signal (@or ($option #\? #\!))])
    ($token->symbol (@and $letter (@* (@or $letter symbol $digit)) (@? signal)))))

;; ('\s' | '\t' | '\n' | '\v' | '\r' | '\f')+
(define $whitespace
  ($token->skip (@+ (@token char-whitespace?))))

;; comment ::= '#' (!'\n')*
(define $comment
  ($token->skip (@and ($char #\#) (@* (@not ($char #\newline))))))

(define $any
  ($token->symbol (@token (lambda (x) #t))))

(define $option-symbol
  (lambda sbls
    ($token->symbol (apply @or (map $string sbls)))))

;; token ::= (identifier | number | comment | whitespace | "->")*
(define $scanner
  (@* (@or $whitespace $comment $identifier $number ($option-symbol "->") $any)))

(define $test-single
  (lambda (parser str)
    (parser (string->list str) (context-make)
      (lambda (t tokens ctx)
        `((parsed: ,(flatten t)) (remain: ,tokens)))
      (lambda (tokens ctx)
        `((remain: ,tokens))))))

; ---------- test -----------
;($test $lower "a" "A")
;($test $upper "a" "A")
;($test $digit "a" "1")
;($test $letter "" "a" "aa" "aA" "Aa" "aaa" "AAA")
;($test $number "1" "+01230" "-00.1230" "1a2" "a12" "12a" "+a12" "00.a" "-0.1a" "00..0012")
;($test ($string "hello") "" "hell" "Hello" "hello" "hello world")
;($test $bool "t" "true" "True" "fas" "false" "False" "truefalse")
;($test ($string "你好") "你好" "你不好")
;($test $whitespace "a" " ab " "\n " "\t" " \t \n  ")
;($test $identifier "a" "abc" "let" "a12" "string->list" "i_var" "list<-string" "1abc" "*string" "--->")
;($test $comment "abc" "123\n" "???" ".\n?" "#a\nabc" "#ab#\n#aa" "abc#cd")
;($test ($string "->") "abc->def" "->ab" "ab->" "->" "-->")
