#lang racket

(require "utils.rkt")
(require "parsec.rkt")

(provide scan (struct-out Token) (struct-out Index))

(struct Token   [index val]   #:transparent)
(struct Stream  [index chars] #:transparent)
(struct Index   [line cursor] #:transparent)

(define scan
  (lambda (text cont)
    (($tokens)
     (Stream (Index 1 1) (string->list text))
     (lambda (v stm) (cont v))
     (lambda (e stm) 
       (match stm
         [(Stream (Index line cursor) chs)
          (error e "[~a:~a]: scan error" line cursor)])))))

;--------- parser ---------;
($:: ($digit)
     (@pred char-numeric?))

($:: ($whitespace)
     (@skip (@pred char-whitespace?)))

($:: ($comment)
     (@skip (@seq (@~ "#") (@* (@pred (negate (curry char=? #\newline)))))))

($:: ($number)
     (@group 'number (pipe flatten chars->number)
       (@seq (@? (@opt (@~ "-") (@~ "+")))
             (@+ ($digit))
             (@? (@seq (@~ ".") (@+ ($digit)))))))

($:: ($identifier)
     ($:: ($letter)
          (@pred char-alphabetic?))
     (@group 'identifier (pipe flatten list->string)
       (@seq ($letter)
             (@* (@opt ($letter) ($digit) (@~ "-") (@~ "_") (@~ ">") (@~ "<")))
             (@? (@opt (@~ "?") (@~ "!"))))))

($:: ($symbol)
     (@group 'symbol list->string
       (@opt (@~ "->"))))

($:: ($any)
     (@group 'any (pipe list list->string)
       (@pred (lambda (t) #t))))

($:: ($tokens)
     (@* (@opt ($whitespace)
               ($comment)
               ($identifier)
               ($number)
               ($symbol)
               ($any))))

;----------- func parser ----------;
(@:: (@pred pred)
     (lambda (stm k-succ k-fail)
       (match stm
         [(Stream idx (list (? pred ch) rs ...))
          (k-succ ch (Stream (index-inc idx ch) rs))]
         [_ (k-fail #f stm)])))

(@:: (@- c)
     (@pred (curry char=? c)))

(@:: (@~ str)
     (apply @seq (map @- (string->list str))))

(@:: (@group tag func p)
     (lambda (stm k-succ k-fail)
       (p stm 
          (lambda (v rs) (k-succ (Token (Stream-index stm) (func v)) rs))
          (lambda (e rs) (k-fail tag rs)))))

;-------- helper ---------;
(define index-inc
  (lambda (idx c)
    (match idx
      [(Index line cursor)
       (if (char=? c #\newline)
         (Index (add1 line) 1)
         (Index line (add1 cursor)))])))

;--------- test ----------;
(define-syntax @test
  (syntax-rules ()
    [(_ p strs ...)
     (for-each
       (lambda (str) 
         (printf "cases: \"~a\" -> ~a~n" str (quote p))
         (p (Stream (string->list str) (Index 1 1))
            (lambda (v stm)
              (printf "parse: ~s~nrests: ~s~n" v (Stream-chars stm)))
            (lambda (e stm)
              (printf "error: ~s~nrests: ~s~n" e (Stream-chars stm))))
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
;(scan
;"let def-var = func (param-var1, ~param-var2, *param-var3) {
;  let var0 = nil, var1 = expr1, var2 = var1, var3 = 1, var4 = true, var5 = false # let binding
;  func-expr(param-var1 = param-val1, param-var2, var3, var4, var5) # func-call
;  cond { cond-expr -> { expr-true } else -> { expr-else } } # cond-expr
;}"
;(lambda (v) (pretty-display v)))
