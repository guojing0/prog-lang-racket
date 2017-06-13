#lang plai-typed

;;; warm-up

(define-type MisspelledAnimal
  [caml (humps : number)]
  [yacc (height : number)])

(define ma1 : MisspelledAnimal (caml 3))
(define ma2 : MisspelledAnimal (yacc 1.1))

;; pattern matching
(define (good? [ma : MisspelledAnimal]) : boolean
  (type-case MisspelledAnimal ma
    [caml (c) (>= c 2)]
    [yacc (y) (> y 2.1)]))

;; selector instead of pattern matcher
(define (second-good? [ma : MisspelledAnimal]) : boolean
  (cond
    [(caml? ma) (>= (caml-humps ma) 2)]
    [(yacc? ma) (> (yacc-height ma) 2.1)]))

;;; data types

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [uminusS (e : ArithS)])

;;; parser

(define (parse [s : s-expression]) : ArithC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusC (parse (second sl)) (parse (third sl)))]
         [(*) (multC (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid list input")]))

;;; interpreter

(define (desugar [as : ArithS]) : ArithC
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l)
                          (multC (numC -1) (desugar r)))]
    [uminusS (e) (multC (numC -1) (desugar e))]))

(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))

;; tests

(test (good? ma1) #t)
(test (good? ma2) #f)

(test (second-good? ma1) #t)
(test (second-good? ma2) #f)

(test (interp (multC (plusC (numC 2) (numC 8))
                     (multC (numC 2.5) (numC 4))))
      100)

(test (interp (desugar
               (multS (multS (numS 5) (numS 2))
                      (bminusS (numS 20) (numS 15)))))
      50)

(test (interp (desugar
               (plusS (multS (multS (numS 5) (numS 2))
                             (bminusS (numS 20) (numS 15)))
                      (uminusS (numS 50)))))
      0)


