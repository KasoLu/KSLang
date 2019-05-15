#lang racket

(require "utils.rkt")
(require "parsec.rkt")

(provide scan (struct-out Token) (struct-out Index))

;; ===== struct ===== ;;
(struct Token [val index] #:transparent)

(struct Stream [chars index] #:transparent)
(struct Index [line cursor] #:transparent)

;; ===== func ===== ;;
(define scan
  (lambda (text cont)
    (init-parse-error!)
    (init-shown-error!)
    (($tokens) 
      (Stream (string->list text) (Index 1 1))
      (lambda (t stm)
        (if (not t)
          (error 'scan *shown-error*)
          (cont t))))))

;; ===== data ===== ;
(define *comment-start* "#")

(define *parse-error* (void))
(define (init-parse-error!)
  (set! *parse-error* '()))

(define *shown-error* (void))
(define (init-shown-error!)
  (set! *shown-error* ""))

;; ===== func parser ===== ;;
(@:: (@pred pred)
     (lambda (stm cont)
       (match stm
         [(Stream chs idx)
          (cond [(null? chs)
                 (cont #f stm)]
                [(pred (car chs)) 
                 (cont (car chs) (Stream (cdr chs) (index-inc idx (car chs))))]
                [else 
                 (set! *parse-error* (cons chs idx))
                 (cont #f stm)])])))

(@:: (@. c)
     (@pred (lambda (t) (char=? c t))))

(@:: (@~ str)
     (apply @cat (map @. (string->list str))))

(@:: (@group tag p func)
     (lambda (stm cont)
       (p stm
         (lambda (t stm*)
           (cond [(and (not t) (null? (Stream-chars stm*)))
                  (cont #f stm)]
                 [(not t)
                  (generate-parse-error tag)
                  (cont #f stm)]
                 [else
                  (cont (Token (func t) (Stream-index stm)) stm*)])))))

;; ===== parser ===== ;;
($:: ($digit)
     (@pred char-numeric?))

($:: ($letter)
     (@pred char-alphabetic?))

($:: ($whitespace)
     (@skip (@pred char-whitespace?)))

($:: ($comment)
     (@skip (@cat (@~ *comment-start*)
                  (@* (@pred char-not-newline?)))))

;; number ::= ("-"|"+")? <digit>+ ("." <digit>+)?
($:: ($number)
     (@group 'number
       (@cat (@? (@opt (@~ "-") (@~ "+")))
             (@+ ($digit))
             (@? (@cat (@~ ".") (@+ ($digit)))))
       chars->number))

;; identifier ::= <letter> (<letter>|<digit>|"-"|"_"|">"|"<")* ("?"|"!")?
($:: ($identifier)
     (@group 'identifier
       (@cat ($letter)
             (@* (@opt ($letter) ($digit) (@~ "-") (@~ "_") (@~ ">") (@~ "<")))
             (@? (@opt (@~ "?") (@~ "!"))))
       list->string))

;; symbol ::= "->"
($:: ($symbol)
     (@group 'symbol
       (@opt (@~ "->"))
       list->string))

;; any ::= <any>
($:: ($any)
     (@group 'any
       (@pred (lambda (t) #t))
       (pipe list list->string)))

;; tokens ::= (<whitespace>|<comment>|<identifier>|<number>|<symbol>|<any>)*
($:: ($tokens)
     (@* (@opt ($whitespace)
               ($comment)
               ($identifier)
               ($number)
               ($symbol)
               ($any))))

;; ===== helper ===== ;;
(define index-inc
  (lambda (idx c)
    (match idx
      [(Index line cursor)
       (if (char=? c #\newline)
         (Index (add1 line) 1)
         (Index line (add1 cursor)))])))

(define char-not-newline?
  (lambda (c)
    (not (char=? c #\newline))))

(define generate-parse-error
  (lambda (tag)
    (match *parse-error*
      [(cons chs idx)
       (set! *shown-error* 
         (format "error - ~a[~a:~a]: '~a' is not satisfied~n" tag (Index-line idx) (Index-cursor idx) (car chs)))]
      [else
       (set! *shown-error*
         (format "error - ~a: tokens is empty~n" tag))])))

;; ===== test ===== ;;
(define-syntax @test
  (syntax-rules ()
    [(_ p strs ...)
     (for-each
       (lambda (str) 
         (init-parse-error!)
         (init-shown-error!)
         (printf "cases: \"~a\" -> ~a~n" str (quote p))
         (p (Stream (string->list str) (Index 1 1))
            (lambda (t stm) 
              (if (not t)
                (display *shown-error*)
                (printf "parse: ~a~nrests: ~a~n" t (Stream-chars stm))
                )))
         (printf "~n"))
       (list strs ...))]))

;(@test ($digit) "" "1" "a")
;(@test ($letter) "" "1" "a")
;(@test ($whitespace) "" " " "\t" "\n" "a")
;(@test ($comment) "" "a#" "#a" "#\na" "##")
;(@test ($number) "" "a" "1" "+0.1" "-00.0230" "00123.1230" "-+0.1" "+12..0")
;(@test ($identifier) "" "a" "1" "var1" "-var" "_var" "v1ar->a" "v--->>aa" "cond!" "else?" "test?!")
;(@test ($symbol) "" "-" ">" "->" "-->" "->>")
;(@test ($any) "" "-" ">" "->" "-->" "->>")
;(@test ($identifier) "1")
;(scan
;"let def-var = func (param-var1, ~param-var2, *param-var3) {
;  let var0 = nil, var1 = expr1, var2 = var1, var3 = 1, var4 = true, var5 = false # let binding
;  func-expr(param-var1 = param-val1, param-var2, var3, var4, var5) # func-call
;  cond { cond-expr -> { expr-true } else -> { expr-else } } # cond-expr
;}"
;  (lambda (ts)
;    (pretty-display ts)))

