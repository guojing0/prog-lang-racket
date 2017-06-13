#lang plai-typed

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

(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in]
    [idC (s) (cond
               [(symbol=? s for) what]
               [else in])]
    [appC (f a) (appC f (subst what for a))]
    [plusC (l r) (plusC (subst what for l)
                        (subst what for r))]
    [multC (l r) (multC (subst what for l)
                        (subst what for r))]))

(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

;;; interpreter

(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [idC (_) (error 'interp "should not get here!")]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (subst a
                                 (fdC-arg fd)
                                 (fdC-body fd))
                          fds))]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]))

;;; tests

(test (interp (appC 'square (numC 10))
              (list (fdC 'square 'x (multC (idC 'x) (idC 'x)))))
      100)

(test (interp (appC 'double
                    (numC (interp (appC 'square (numC 10))
                                  (list (fdC 'square 'x (multC (idC 'x) (idC 'x)))))))
              (list (fdC 'double 'x (multC (numC 2) (idC 'x)))))
      200)
