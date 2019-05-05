#lang racket

(provide (all-defined-out))
(require racket/trace)
(require "utils.rkt")
(require "parsec.rkt")

;; ----- context -----
(define context-make
  (lambda () '()))

(define context-get
  (lambda (key ctx)
    (let ([found (assv key ctx)])
      (if found
        (cadr found)
        (error 'context-get "'~a' not found in ~a" key ctx)))))

(define context-extend
  (lambda (key val ctx)
    (cons (list key val) ctx)))

(define context-init
  (lambda (ctx)
    (context-extend 'line 1
      (context-extend 'column 1
        (context-extend 'parser 'start
          (context-make))))))

(define context-inc-line-column
  (lambda (t ctx)
    (let ([line (context-get 'line ctx)] [column (context-get 'column ctx)])
      (if (char=? t #\newline)
        (context-extend 'line (add1 line) (context-extend 'column 1 ctx))
        (context-extend 'column (add1 column) ctx)))))

;; ----- struct -----
(struct Identifier [val line column] #:transparent)
(struct Number [val line column] #:transparent)
(struct Symbol [val line column] #:transparent)

;; ----- helper -----
(define pipe (lambda (val . funcs) (foldl (lambda (f res) (f res)) val funcs)))
(define token->skip (lambda (t line column) '()))
(define token->identifier (lambda (t line column) (Identifier (pipe t flatten list->string string->symbol) line column)))
(define token->number (lambda (t line column) (Number (pipe t flatten list->string string->number) line column)))
(define token->symbol (lambda (t line column) (Symbol (pipe t flatten list->string string->symbol) line column)))
(define report-scan-error
  (lambda (tokens tag line column)
    (if (null? tokens)
      (error 'scanner "tokens is empty")
      (error 'scanner "[~a:~a] ~a: '~a' is not satified" line column tag (car tokens)))))

;; ----- compound parser -----
(define $string
  (lambda (str)
    (let ([$char (lambda (c) (@token (lambda (x) (char=? x c))))])
      (apply @and (map $char (string->list str))))))

(define $range
  (lambda (start end)
    (@token (lambda (x) (and (char<=? start x) (char<=? x end))))))

(define $option
  (lambda strs
    (apply @or (map $string strs))))

(define $letter
  (let ([$lower ($range #\a #\z)] [$upper ($range #\A #\Z)])
    (@or $lower $upper)))

(define $digit
  ($range #\0 #\9))

(define $final
  (lambda (parser tag func)
    (lambda (tokens ctx k-ctx k-succ k-fail)
      (parser tokens (context-extend 'parser tag ctx) k-ctx
        (lambda (t tokens* ctx*)
          (let ([line (context-get 'line ctx)] [column (context-get 'column ctx)])
            (k-succ (func t line column) tokens* ctx*)))
        k-fail))))

;; ----- syntax -----
(define $whitespace ;;= <whitespace>
  ($final (@token char-whitespace?) '$whitespace token->skip))

(define $comment ;;= '#' <!'\n'>*
  ($final (@and ($string "#") (@* (@not ($string "\n")))) '$comment token->skip))

(define $identifier ;;= letter <letter | digit | symbol>* <'?' | '!'>?
  (let ([head $letter] 
        [body (@* (@or $letter $digit ($option "-" "_" "<" ">")))] 
        [tail (@? ($option "?" "!"))])
    ($final (@and head body tail) '$identifier token->identifier)))

(define $number ;;= <'-' | '+'>? digit+ <'.' digit+>?
  (let ([head (@? ($option "-" "+"))]
        [body (@+ $digit)]
        [tail (@? (@and ($string ".") (@+ $digit)))])
    ($final (@and head body tail) '$number token->number)))

(define $symbol ;;= "->"
  ($final ($option "->") '$symbol token->symbol))

(define $any ;;= char
  ($final (@token (lambda (x) #t)) '$any token->symbol))

(define $tokens ;;= <whitespace | comment | identifier | number | symbol | any>*
  (let ([parser (@* (@or $whitespace $comment $identifier $number $symbol $any))])
    ($final parser '$token (lambda (t line column) (flatten t)))))

(define kslang:scanner
  (lambda (text)
    ($tokens (string->list text) (context-init (context-make))
      (lambda (k-succ) (lambda (t tokens ctx) (k-succ t tokens (context-inc-line-column t ctx))))
      (lambda (t tokens ctx) t)
      (lambda (tokens ctx)
        (let ([line (context-get 'line ctx)] 
              [column (context-get 'column ctx)] 
              [parser (context-get 'parser ctx)])
          (report-scan-error tokens parser line column))))))

