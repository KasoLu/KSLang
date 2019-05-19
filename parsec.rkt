#lang racket

(provide (all-defined-out))
(require "utils.rkt")

(define-syntax @::
  (syntax-rules ()
    [(_ kw body ...)
     (define kw (lambda $ (apply (let () body ...) $)))]))

(define-syntax $::
  (syntax-rules ()
    [(_ (name) body ...)
     (define (name)
       (lambda (vs k-succ k-fail)
         (let ([val (store-get *store* (quote name) vs)])
           (cond [(not val)
                  (store-set! *store* (quote name) vs (void))
                  ((let () body ...) vs 
                   (lambda $ (store-set! *store* (quote name) vs $)  (apply k-succ $))
                   (lambda $ (store-set! *store* (quote name) vs #f) (apply k-fail $)))]
                 [(void? val)
                  (report-left-recursive-error (quote name) vs)]
                 [else
                  (apply k-succ val)]))))]))

(@:: (@const x)
     (lambda (vs k-succ k-fail)
       (k-succ x vs)))

(@:: (@truth)
     (@const '()))

(@:: (@error)
     (lambda (vs k-succ k-fail)
       (k-fail #f vs)))

(@:: (@succ p func)
     (lambda (vs k-succ k-fail)
       (p vs (lambda (v rs) ((func v) rs k-succ k-fail)) k-fail)))

(@:: (@fail p func)
     (lambda (vs k-succ k-fail)
       (p vs k-succ (lambda (e rs) ((func e rs) vs k-succ k-fail)))))

(@:: (@skip p)
     (@succ p (lambda (v) (@const '()))))

(@:: (@and p q func)
     (@succ p (lambda (pv) (@succ q (lambda (qv) (@const (func pv qv)))))))

(@:: (@seq . ps)
     (let ([bind (lambda (p a) (@and p a cons))])
       (foldr bind (@const '()) ps)))

(@:: (@cat . ps)
     (let ([bind (lambda (p a) (@and p a pair-cons))])
       (foldr bind (@const '()) ps)))

(@:: (@opt . ps)
     (let ([bind (lambda (p a) (@fail p (lambda (e rs) a)))])
       (foldr bind (@error) ps)))

(@:: (@? p)
     (@opt p (@truth)))

(@:: (@* p)
     (@opt (@+ p) (@truth)))

(@:: (@+ p)
     (@and p (@* p) pair-cons))

(@:: (@.* p sep)
     (@? (@.+ p sep)))

(@:: (@.+ p sep)
     (@and p (@* (@cat sep p))
       (lambda (v vs)
         (foldl (lambda (v a) (append a v)) (list v) vs))))

;--------- data ---------;
(define *store* (make-hasheq))

(define store-get
  (lambda (store key1 key2)
    (cond [(hash-ref store key1 #f) => 
           (lambda (sub-store) (hash-ref sub-store key2 #f))]
          [else #f])))

(define store-set!
  (lambda (store key1 key2 val)
    (let ([sub-store (hash-ref store key1 #f)])
      (unless sub-store
        (set! sub-store (make-hasheq))
        (hash-set! store key1 sub-store))
      (hash-set! sub-store key2 val))))

(define report-left-recursive-error
  (lambda (tag vs)
    (error tag "report-left-recursive-error: ~a~n~a~n" vs *store*)))

(define pair-cons
  (lambda (p q)
    (if (null? p) q (cons p q))))

;------- test -------;
(define-syntax @test
  (syntax-rules ()
    [(_ p strs ...)
     (for-each
       (lambda (str) 
         (printf "cases: \"~a\" -> ~a~n" str (quote p))
         (p (string->list str)
            (lambda (v rs) (printf "parse: ~s~nrests: ~s~n" v rs))
            (lambda (e rs) (printf "error: ~s~nrests: ~s~n" e rs)))
         (printf "~n"))
       (list strs ...))]))

(@:: (@pred pred)
     (lambda (vs k-succ k-fail)
       (if (and (pair? vs) (pred (car vs)))
         (k-succ (car vs) (cdr vs))
         (k-fail 'pred vs))))

;(@test (@const 10) "" "a" "12" "A23")
;(@test (@truth) "" "a" "12" "A23")
;(@test (@error) "" "a" "12" "A23")
;(@test (@pred char-alphabetic?) "" "a" "12" "A23")
;(@test (@succ (@truth) (lambda (v) (@const 10))) "" "a" "12" "A23")
;(@test (@fail (@error) (lambda (t rs) (@const 10))) "" "a" "12" "A23")
;(@test (@seq (@pred char-alphabetic?) (@pred char-numeric?)) "" "a" "12" "A23" "ab34" "a3b4")
;(@test (@opt (@const 10) (@const 20)) "" "a" "12" "A23")
;(@test (@? (@pred char-alphabetic?)) "" "a" "12" "A23")
;(@test (@* (@pred char-alphabetic?)) "" "a" "12" "A23" "ab34" "a3b4" "abc")
;(@test (@+ (@pred char-alphabetic?)) "" "a" "12" "A23" "ab34" "a3b4" "abc")
;(@test (@.* (@pred char-alphabetic?) (@pred char-numeric?)) "" "a" "1a2" "A23" "ab34" "a3b4" "a3b4c")
;(@test (@.+ (@pred char-alphabetic?) (@pred char-numeric?)) "" "a" "1a2" "A23" "ab34" "a3b4" "a3b4c")

