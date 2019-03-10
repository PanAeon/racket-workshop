#lang racket
(require racket/trace)
(require rackunit)

#|
   useful -- display
|#
(define primitives
  (list (cons '+ +)
        (cons '* *)
        (cons '- -)
        (cons '/ /)
  )
  )

(define (lookup env s)
  (match env
    ['() (error "value " s "not found")]
    [(cons (cons s1 v1) rest) #:when (eq? s s1) v1]
    [(cons zz rest) (lookup rest s)]
   ))


(define (extend-environment env names values)
  (append (map cons names values) env )) ; FIXME: doesn't delete duplicate value


(define (eval-sequence env terms)
  (match terms
    [(list exp)  (eval-exp env exp)]
 
    [(list (list 'define name exp) rest ...)  (begin
       (define (env1) (extend-environment env (list name) (list (eval-exp env exp))))
       (eval-sequence (env1) rest)
     )]
 
    [(list trm rest ...) (begin (eval-exp env trm) (eval-sequence env rest))]))

;(trace eval-sequence)

(define (make-function env parameters body)
  (λ arguments; eval arguments, and return lambda, wait, hmm, could have reused prev one
    (begin
       (define (f paramName arg e) (extend-environment e (list paramName) (list (eval-exp e arg))))
       (define (env1) (foldl f env parameters arguments))
       (eval-sequence (env1) body)
     )
    ))

(define (eval-exp env exp)
  (match exp
    [(? number?) exp]
    ;
    ;
    [(list 'begin terms ...) (eval-sequence env terms)]
    
  


    [(list 'λ parameters body ...) (make-function env parameters body)]


    [(list 'lambda parameters body ...) (make-function env parameters body)]

    [(list s args ...) #:when (symbol? s) (apply (lookup env s) (map (λ (x) (eval-exp env x)) args))] ; APPLY symbol case

    [_ #:when (symbol? exp)  (lookup env exp)]

    [(list expr) (apply (eval-exp env  expr) '[])] ; APPLY single case

    [(cons s args) (apply (eval-exp env s) (map (λ (x) (eval-exp env x)) args))] ; APPLY multiple case

    #| [(cons '+  args) (apply + (map eval-exp args))]

    [(list '* args ...) (apply * (map eval-exp args))]

    [(list '- args ...) (apply - (map eval-exp args))]

    [(list '/ args ...) (apply / (map eval-exp args))]
   |#
    [_ (error 'wat (~a exp))]
    )
  )

    (check-equal?
     (lookup (list (cons 'a 1)
                   (cons 'b 2))
             'a)
     1)

    (check-equal?
     (lookup (list (cons 'a 1)
                   (cons 'b 2))
             'b)
     2)

    (check-equal?
     (lookup (list (cons 'a 0)
                   (cons 'a 1)
                   (cons 'b 2))
             'a)
     0)

    (check-exn
     exn:fail?
     (λ ()
       (lookup (list (cons 'a 1)
                     (cons 'b 2))
               'c)))

(define (evaluate input)
  (eval-exp primitives input)
  )

(define (repl)
  (printf "> ")
  (define input (read))
  (unless (eof-object? input )
    (define output (evaluate input))
    (printf "~a~n" output)
    (repl)
    )
  )
 
;   :.:.:.:.:.::.:.:.:.:.: TEST Lookup :.:.:.:.:.::.:.:.:.:.::.:.:.:.:.::.:.:.:.:.::.:.:.:.:.:
    (check-equal?
     (lookup (list (cons 'a 1)
                   (cons 'b 2))
             'a)
     1)

    (check-equal?
     (lookup (list (cons 'a 1)
                   (cons 'b 2))
             'b)
     2)

    (check-equal?
     (lookup (list (cons 'a 0)
                   (cons 'a 1)
                   (cons 'b 2))
             'a)
     0)

    (check-exn
     exn:fail?
     (λ ()
       (lookup (list (cons 'a 1)
                     (cons 'b 2))
               'c)))

;   :.:.:.:.:.::.:.:.:.:.: TEST ENV :.:.:.:.:.::.:.:.:.:.::.:.:.:.:.::.:.:.:.:.::.:.:.:.:.:
    (check-equal?
     (extend-environment (list (cons 'd 2) (cons 'e 1))
                         (list 'a 'b 'c)
                         (list 5 4 3))
     (list (cons 'a 5) (cons 'b 4) (cons 'c 3) (cons 'd 2) (cons 'e 1)))

    (check-equal?
     (evaluate
      '(begin
         (define a 2)
         (define b 3)
         (+ a b)))
     5)

    (check-equal?
     (evaluate
      '(begin
         (define a 2)
         (define a 3)
         (+ a a)))
     6)

;   :.:.:.:.:.::.:.:.:.:.: TEST λ :.:.:.:.:.::.:.:.:.:.::.:.:.:.:.::.:.:.:.:.::.:.:.:.:.:

     (check-equal?
     (evaluate '((λ () (+ 2 3))))
     5)

    (check-equal?
     (evaluate '((lambda (x y) (+ x y)) 3 4))
     7)

    (check-equal?
     (evaluate
      '((lambda ()
          (define a 2)
          (define b 3)
          (+ a b))))
     5)

    (check-equal?
     (evaluate
      '((lambda ()
          (define a 2)
          (define b (lambda (c) (define a 5) (+ a c)))
          (b a))))
     7)