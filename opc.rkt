#lang racket

(provide (all-defined-out))
(require "utils.rkt")

(define @truth
  (lambda ()
    (lambda (val k-succ k-fail)
      (k-succ val))))

(define @error
  (lambda ()
    (lambda (val k-succ k-fail)
      (k-fail val))))

(define @const
  (lambda (v)
    (lambda (val k-succ k-fail)
      (k-succ v))))

(define @pred
  (lambda (pred)
    (lambda (val k-succ k-fail)
      (if (pred val)
        (k-succ val)
        (k-fail val)))))

(define @seq
  (lambda ops
    (let ([seq-bind (lambda (op acc) (@succ op (lambda (val) acc)))])
      (let ([seq-op (foldr seq-bind (@truth) ops)])
        (lambda (val k-succ k-fail)
          (seq-op val k-succ (lambda (val*) (k-fail val))))))))

(define @opt
  (lambda ops
    (let ([opt-bind (lambda (op acc) (@fail op (lambda (val) acc)))])
      (foldr opt-bind (@error) ops))))

(define @?
  (lambda (op)
    (@opt op (@truth))))

(define @*
  (lambda (op)
    (@opt (@+ op) (@truth))))

(define @+
  (lambda (op)
    (@seq op (lambda $ (apply (@* op) $)))))

(define @succ
  (lambda (op bind)
    (lambda (val k-succ k-fail)
      (let ([d-succ (lambda (val) ((bind val) val k-succ k-fail))])
        (op val d-succ k-fail)))))

(define @fail
  (lambda (op bind)
    (lambda (val k-succ k-fail)
      (let ([d-fail (lambda (val) ((bind val) val k-succ k-fail))])
        (op val k-succ d-fail)))))

(define @decor
  (lambda (op d-val d-succ d-fail)
    (lambda (val k-succ k-fail)
      (op (d-val val) (d-succ k-succ) (d-fail k-fail)))))

