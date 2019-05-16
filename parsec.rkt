#lang racket

(provide (all-defined-out))
(require "utils.rkt")

;; ===== syntax ===== ;;
(define-syntax @::
  (syntax-rules ()
    [(_ kw body)
     (define kw (lambda $ (apply body $)))]))

(define-syntax $::
  (syntax-rules ()
    [(_ (name) body)
     (define (name)
       (lambda (vs cont)
         (let ([table (store-get *store* (quote name) vs)])
           (if (not table)
             (begin
               (set! table (table-make))
               (store-set! *store* (quote name) vs table)
               (table-set-conts! table cont)
               (body vs 
                 (lambda v
                   (unless (member v (table-get-vals table))
                     (table-set-vals! table v)
                     (for-each (lambda (k) (apply k v)) (table-get-conts table))))))
             (begin
               (table-set-conts! table cont)
               (for-each (lambda (v) (apply cont v)) (table-get-vals table)))))))]))

;; ===== meta parser ===== ;;
(@:: (@const x)
     (lambda (vals cont)
       (cont x vals)))

(@:: (@truth)
     (@const '()))

(@:: (@error)
     (lambda (vals cont)
       (cont #f vals)))

(@:: (@succ p func)
     (lambda (vals cont)
       (p vals 
         (lambda (val vals*)
           (if (not val)
             (cont #f vals)
             ((func val) vals* cont))))))

(@:: (@fail p func)
     (lambda (vals cont)
       (p vals
         (lambda (val vals*)
           (if (not val)
             ((func) vals cont)
             (cont val vals*))))))

(@:: (@cat . ps)
     (@merge ps
       (lambda (v vs) (if (list? v) (append v vs) (cons v vs)))))

(@:: (@seq . ps)
     (@merge ps
       (lambda (v vs) (cons v vs))))

(@:: (@opt . ps)
     (let ([loop (lambda (p a) (@fail p (lambda () a)))])
       (foldr loop (@error) ps)))

(@:: (@alt . ps)
     (lambda (vs cont)
       (for-each (lambda (p) (p vs cont)) ps)))

(@:: (@? p)
     (@opt p (@truth)))

(@:: (@* p)
     (@opt (@+ p) (@truth)))

(@:: (@+ p)
     (@cat p (@* p)))

;; ===== func parser ===== ;;
(@:: (@skip p)
     (@succ p (lambda (t) (@const '()))))

(@:: (@flat p)
     (@succ p 
       (lambda (v)
         (if (list? v)
           (@const (filter (lambda (x) (not (null? x))) v))
           (@const (list v))))))

(@:: (@merge ps func)
     (let* ([loop-v (lambda (v vs) (@const (func v vs)))]
            [loop-a (lambda (a v)  (@succ a (lambda (vs) (loop-v v vs))))]
            [loop-p (lambda (p a)  (@succ p (lambda (v) (loop-a a v))))]
            [cat-p (foldr loop-p (@const '()) ps)])
       (lambda (vals cont)
         (cat-p vals (lambda (val vals*) (if (not val) (cont #f vals) (cont val vals*)))))))

;; ===== helper ===== ;;
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

(define table-make
  (lambda () (mcons '() '())))

(define table-get-conts
  (lambda (tbl) (mcar tbl)))

(define table-set-conts!
  (lambda (tbl cont) (set-mcar! tbl (cons cont (table-get-conts tbl)))))

(define table-get-vals
  (lambda (tbl) (mcdr tbl)))

(define table-set-vals!
  (lambda (tbl val) (set-mcdr! tbl (cons val (table-get-vals tbl)))))

;; ===== test ===== ;;
(define-syntax @test
  (syntax-rules ()
    [(_ p strs ...)
     (for-each
       (lambda (str) 
         (printf "cases: \"~a\" -> ~a~n" str (quote p))
         (p (string->list str)
            (lambda (val vals) (printf "parse: ~a~nrests: ~a~n" val vals)))
         (printf "~n"))
       (list strs ...))]))

;(@:: (@pred pred)
;     (lambda (vals cont)
;       (if (and (pair? vals) (pred (car vals)))
;         (cont (car vals) (cdr vals))
;         (cont #f vals))))

;($:: ($digit)
;     (@pred char-numeric?))

;($:: ($expr)
;     (@opt ($expr) ($digit)))

;(@test (@const 10) "" "a" "12" "A23")
;(@test (@truth) "" "a" "12" "A23")
;(@test (@error) "" "a" "12" "A23")
;(@test (@pred char-alphabetic?) "" "a" "12" "A23")
;(@test (@succ (@truth) (lambda (t) (@const 10))) "" "a" "12" "A23")
;(@test (@fail (@error) (lambda () (@const 10))) "" "a" "12" "A23")
;(@test (@seq (@const 10) (@const 20)) "" "a" "12" "A23")
;(@test (@opt (@const 10) (@const 20)) "" "a" "12" "A23")
;(@test (@? (@pred char-alphabetic?)) "" "a" "12" "A23")
;(@test (@* (@pred char-alphabetic?)) "" "a" "12" "A23" "ab34" "a3b4")
;(@test (@+ (@pred char-alphabetic?)) "" "a" "12" "A23" "ab34" "a3b4")
;(@test (@flat (@* (@pred char-alphabetic?))) "" "a" "12" "A23" "ab34" "a3b4")
;(@test (@seq (@const 10) (@skip (@const 20)) (@const 30)) "" "a" "12" "A23" "ab34" "a3b4")
;(@test ($expr) "10")

