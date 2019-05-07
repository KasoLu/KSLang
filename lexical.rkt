#lang racket

;(provide kslang:scanner (struct-out Lex-token) context-get)
(provide (all-defined-out))

(require racket/trace)
(require "utils.rkt")
(require "opc.rkt")

;; ----- struct -----
(struct Value [index stashs tokens rests errors] #:transparent)
(struct Index [line cursor] #:transparent)
(struct Stash [char index] #:transparent)
(struct Token [val index] #:transparent)

;; ----- helper -----
(define value-make
  (lambda (text)
    (Value (Index 1 1) '() '() (string->list text) '())))

(define index-next
  (lambda (index char)
    (if (char=? char #\newline) 
      (Index (add1 (Index-line index)) 1)
      (Index (Index-line index) (add1 (Index-cursor index))))))

(define char-pred
  (lambda (pred)
    (lambda (value)
      (let ([rests (Value-rests value)])
        (and (pair? rests) (pred (car rests)))))))

(define char-next
  (lambda (value)
    (match value
      [(Value index stashs tokens rests errors)
       (let ([curr-char (car rests)])
         (Value (index-next index curr-char) (cons (Stash curr-char index) stashs) tokens (cdr rests) errors))])))

(define token-compound
  (lambda (value compound-func)
    (match value
      [(Value index stashs tokens rests errors)
       (let ([token (stashs->token (reverse stashs) compound-func)])
         (if (Token? token)
           (Value index '() (cons token tokens) rests errors)
           (Value index '() tokens rests errors)))])))

(define stashs->token
  (lambda (stashs token-make-func)
    (if (null? stashs)
      (void)
      (match (car stashs)
        [(Stash _ index)
         (token-make-func (list->string (map Stash-char stashs)) index)]))))

(define content->skip
  (lambda (content index) '()))

(define content->number 
  (lambda (content index) (Token (string->number content) index)))

(define content->symbol
  (lambda (content index) (Token (string->symbol content) index)))

(define error-make
  (lambda (value tag)
    (match value
      [(Value index stashs tokens rests errors)
       (let ([new-error
               (if (null? rests)
                 (format "~a: rests is empty~n" tag)
                 (format "[~a:~a] '~a' is not satified" (Index-line index) (Index-cursor index) (car rests)))])
         (Value index stashs tokens rests (cons new-error errors)))])))

;; ----- parser -----
(define $item
  (lambda (pred)
    (@succ (@pred (char-pred pred)) (pipe char-next @const))))

(define $range
  (lambda (start end)
    ($item (lambda (x) (and (char<=? start x) (char<=? x end))))))

(define $string
  (lambda (str)
    (let ([$char (lambda (c) ($item (lambda (x) (char=? x c))))])
      (apply @seq (map $char (string->list str))))))

(define $option
  (lambda strs
    (apply @opt (map $string strs))))

(define $compound
  (lambda (parser tag compound-func)
    (@decor parser identity
      (lambda (k-succ) (lambda (v) (k-succ (token-compound v compound-func))))
      (lambda (k-fail) (lambda (v) (k-fail (error-make v tag)))))))

(define $not
  (lambda (parser)
    (lambda (val k-succ k-fail)
      (parser val 
        (lambda (v) (k-fail v))
        (lambda (v) (if (null? (Value-rests v)) (k-fail v) (k-succ (char-next v))))))))

;; ----- lexical -----
(define $letter
  (let ([$lower ($range #\a #\z)] [$upper ($range #\A #\Z)])
    (@opt $lower $upper)))

(define $digit
  ($range #\0 #\9))

;whitespace ::= <whitespace>
(define $whitespace
  ($compound ($item char-whitespace?) '$whitespace content->skip))

;comment ::= '#' <!'\n'>*
(define $comment
  ($compound (@seq ($string "#") (@* ($not ($string "\n")))) '$comment content->skip))

;number ::= <'-' | '+'>? digit+ <'.' digit+>?
(define $number
  (let ([head (@? ($option "-" "+"))]
        [body (@+ $digit)]
        [tail (@? (@seq ($string ".") (@+ $digit)))])
    ($compound (@seq head body tail) '$number content->number)))

;identifier ::= letter <letter | digit | '-' | '_' | '<' | '>'>* <'?' | '!'>?
(define $identifier
  (let ([head $letter]
        [body (@* (@opt $letter $digit ($option "-" "_" "<" ">")))]
        [tail (@? ($option "?" "!"))])
    ($compound (@seq head body tail) '$identifier content->symbol)))

;symbol ::= "->"
(define $symbol
  ($compound ($option "->") '$symbol content->symbol))

;any ::= <char>
(define $any
  ($compound ($item (lambda (x) #t)) '$any content->symbol))

;tokens ::= <whitespace | comment | number | identifier | symbol | any>*
(define $tokens
  (@* (@opt $whitespace $comment $number $identifier $symbol $any)))

;; ----- test -----
(define $->number
  (lambda (parser)
    ($compound parser '$->number content->number)))

(define $->symbol
  (lambda (parser)
    ($compound parser '$->symbol content->symbol)))

(define $test-print-tokens
  (lambda (value)
    (let ([tokens (reverse (Value-tokens value))] [rests (Value-rests value)])
      (printf "tokens: ~a~nrests: ~a~n" tokens rests))))

(define $test-print-errors
  (lambda (value)
    (let ([errors (Value-errors value)] [rests (Value-rests value)])
      (printf "errors: ~a~nrests: ~a~n" errors rests))))

(define $test
  (lambda (parser . strs)
    (define (loop str)
      (printf "case: \"~a\"~n" str)
      (parser (value-make str) $test-print-tokens $test-print-errors)
      (printf "~n"))
    (for-each loop strs)))

;($test ($->symbol $letter) "a" "A")
;($test ($->number $digit) "a" "1")
;($test $number "1" "+01230" "-00.1230" "1a2" "a12" "12a" "+a12" "00.a" "-0.1a" "00..0012")
;($test ($->symbol ($string "你好")) "你好不" "你不好")
