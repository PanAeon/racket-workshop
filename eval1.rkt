#lang racket
(require racket/trace)
(require rackunit)

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
       (define (env1) (extend-environment env name (list (eval-exp env exp))))
       (eval-sequence (env1) rest)
     )]
 
    [(list trm rest ...) (begin (eval-exp env trm) (eval-sequence env rest))]))

;(trace eval-sequence)

(define (make-function env parameters body)
  (λ arguments; eval arguments, and return lambda, wait, hmm, could have reused prev one
    (begin
       (define (f paramName arg e) (extend-environment e paramName (list (eval-exp e arg))))
       (define (env1) (foldl f env parameters arguments))
       (eval-sequence (env1) body)
     )
    ))

(define (eval-exp env exp)
  (match exp
    [(? number?) exp]
    ;
    ; FIXME: where's apply??
    ;
    [(list 'begin terms ...) (eval-sequence env terms)]

    
    [(list 'λ parameters body ...) (make-function env parameters body)]
    
    [(list s args ...) #:when (symbol? s) (apply (lookup env s) (map (λ (x) (eval-exp env x)) args))]

    [_ #:when (symbol? exp)  (lookup env exp)]


    ;[(list 'λ parameters body ...) (make-function env parameters body)]


    [(list 'lambda parameters body ...) (make-function env parameters body)]

    #| [(cons '+  args) (apply + (map eval-exp args))]

    [(list '* args ...) (apply * (map eval-exp args))]

    [(list '- args ...) (apply - (map eval-exp args))]

    [(list '/ args ...) (apply / (map eval-exp args))]
   |#
    [_ (error 'wat (~a exp))]
    )
  )


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



    #|(check-equal?
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
     6)|#