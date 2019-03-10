#lang racket
(require racket/trace)
(require rackunit)

#|
   useful -- display
|#

(define (primitive function)
  (λ (continue . args)
    (continue (apply function args))))

(define (my-plus continue . args)
  (continue (apply + args)))
;(trace my-plus)

(define primitives
  (list (cons '+ (primitive +))
        (cons '* (primitive *))
        (cons '- (primitive -))
        (cons '/ (primitive /))
        (cons '= (primitive =))
        (cons '> (primitive >))
        (cons '>= (primitive >=))
        (cons '< (primitive <))
        (cons '<= (primitive <=))
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


(define (eval-sequence env continue terms)
  (match terms
    [(list exp)  (eval-exp env continue exp)]
 
    [(list (list 'define name exp) rest ...) (eval-exp env (λ (e1)
        (begin
          (define (env1) (extend-environment env (list name) (list e1)))
          (eval-sequence (env1) continue rest)
        )
      ) exp)
       
     ]
 
    [(list trm rest ...) (eval-exp env (λ (ignored) (eval-sequence env rest)) trm) ])
)

;(trace eval-sequence)


(define (make-function env parameters body)
  (λ (continue . arguments)
    (begin
       (define (funScope) (extend-environment env parameters arguments)) 
       (eval-sequence (funScope) continue body)
     )
    )
)




(define (eval-application env continue fun args) ; victory, this will push args to the end of the list and evaluate the list at the end
  (eval-exp
     env
     (λ (evaluated-function)
       (let ([evaluated_args
              (cdr ((foldr (λ(a r) (eval-exp env (λ (e1) (λ (x) (cons x (r e1))  ) ) a)) (λ (x) (list x)) args) '[]))
             ])
         (apply evaluated-function (cons continue evaluated_args))
       )
     )
     fun
  )
)

(define (fold-test fun args)
  (define (arg)
    (λ (x) (λ(c) (curry c x))
    )
  )
  (define (args1)
    (reverse (map (arg) args))
  )
  
  (((foldr compose identity (args1)) fun))     
  )
;(trace eval-application)

(define (eval-if env continue exp then else)
  (eval-exp env (λ (e) (if e (eval-exp env continue then) (eval-exp env continue else)))
            exp)
)

#|
TODO: and, or (REWRITE)
[(list 'and a b)
 (define rewritten-exp (list 'if your code here))
 your code also here]
|#

(define (eval-exp env continue  exp)
  (match exp
    [(? number?) (continue exp)]

    [(? boolean?) (continue exp)]

    [(list 'begin terms ...) (eval-sequence env continue terms)]

    [(list 'λ parameters body ...)  (continue (make-function env parameters body))]

    [(list 'lambda parameters body ...)  (continue (make-function env parameters body))]

    [(list 'if exp then else) (eval-if env continue exp then else)]

    [(? symbol?)   (continue (lookup env exp))]

    [(list fun args ...) (eval-application env continue fun args)]
    
    [_ (error 'wat (~a exp))]
    )
  )
;(trace eval-exp)
  
(define (evaluate input)
  (eval-exp primitives identity input)
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

;   :.:.:.:.:.::.:.:.:.:.: TEST Conditions :.:.:.:.:.::.:.:.:.:.::.:.:.:.:.::.:.:.:.:.::.:.:.:.:.:
    (check-equal?
     (evaluate '(if #f (+ 1 2) (+ 2 3)))
     5)

    (check-equal?
     (evaluate '(if (> 8 4) (+ 1 2) 0))
     3)

    (check-equal?
     (evaluate '((λ (a b)
                   (if (> a (+ b b)) (- a b) (+ a b)))
                 9 1))
     8)

    (check-equal?
     (evaluate '((λ (a b)
                   (if (> a (+ b b)) (- a b) (+ a b)))
                 9 5))
     14)

; :.:.:.:.:.::.:.:.:.:.: TEST Continuations :.:.:.:.:.::.:.:.:.:.::.:.:.:.:.::.:.:.:.:.::.:.:.:.:.:

    (check-equal?
     (eval-require primitives
                   (λ (x f) #t)
                   (λ () #f)
                   '(< 3 6))
     #t)

    (check-equal?
     (eval-require primitives
                   (λ (x f) #t)
                   (λ () #f)
                   '(> 3 6))
     #f)

    (check-equal?
     (evaluate
      '(begin
         (define a (amb 1 (- 5 3) 6 8))
         (require (> a 5))
         a))
     6)

    (check-exn
     exn:fail?
     (λ()
       (evaluate
        '(begin
           (define a (amb 1 2 3))
           (require (> a 5))
           a))))

    (check-equal?
     (evaluate
      '(begin
         (define a (amb 1 3 5 7))
         (define b (amb 2 4 3 6))
         (require (= (+ a b) 9))
         (list a b)))
     '(3 6))

    (check-equal?
     (evaluate*
      '(begin
         (define a (amb 1 (- 5 3) 6 8))
         (require (> a 5))
         a))
     '(6 8))

    (check-equal?
     (evaluate*
      '(begin
         (define a (amb 1 3 5 7))
         (define b (amb 2 4 3 6))
         (require (= (+ a b) 9))
         (list a b)))
     '((3 6) (5 4) (7 2)))