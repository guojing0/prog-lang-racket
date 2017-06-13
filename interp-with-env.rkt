#lang plai-typed

;;; init env

(define-type Binding
  [bind (name : symbol) (val : number)])
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

;;; data types

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

;;; helper functions

(define (lookup [exp : symbol] [env : Env]) : number
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? exp (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup exp (rest env))])]))

(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

;;; interpreter

(define (interp [expr : ExprC]
                [env : Env]
                [fds : (listof FunDefC)]) : number
  (type-case ExprC expr
    [numC (n) n]
    [idC (n) (lookup n env)]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (fdC-body fd)
                          (extend-env (bind (fdC-arg fd)
                                            (interp a env fds))
                                      mt-env)
                          fds))]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]))

;;; tests

(test (interp (plusC (appC 'square (numC 10))
                     (appC 'double (numC 200)))
              mt-env
              (list (fdC 'double 'x (multC (numC 2) (idC 'x)))
                    (fdC 'square 'x (multC (idC 'x) (idC 'x)))))
      500)
