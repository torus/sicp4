(use gauche.test)

;; 4.1.1

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define apply-in-underlying-scheme apply) ;; 4.1.4

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;; 4.1.2

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;; cond

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;; Ex 4.2
(define false #f)
(define true #t)
;; (eval '(define x 3) '())

;; Ex 4.3
(define *method-table* '())
(define (method-existing? tag)
  (not (eq? #f (assq tag *method-table*))))
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((and (pair? exp) (symbol? (car exp)) (method-existing? (car exp)))
         ((get (car exp)) exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (put! tag proc)
  (set! *method-table* (acons tag proc *method-table*)))
(define (get tag)
  (cdr (assq tag *method-table*)))

(put! 'quote (lambda (exp env) (text-of-quotation exp)))
(put! 'set! eval-assignment)
(put! 'define eval-definition)
(put! 'if eval-if)
(put! 'lambda (lambda (exp env)
                (make-procedure (lambda-parameters exp)
                                (lambda-body exp)
                                env)))
(put! 'begin (lambda (exp env)
               (eval-sequence (begin-actions exp) env)))
(put! 'cond (lambda (exp env)
              (eval (cond->if exp) env)))

;; Ex 4.4
;; ('and head . tail)
(define (eval-and exp env)
  (eval (and->if exp) env))

(define (and->if exps)
  (if (null? exps)
      'true
      (let ((first (car exps))
            (rest (cdr exps)))
        (if (null? rest)
            first
            (make-if first
                     (and->if rest)
                     'false)))))

(define (or->if exps)
  (if (null? exps)
      'false
      (let ((first (car exps))
            (rest (cdr exps)))
        (if (null? rest)
            first
            (make-if first
                     first
                     (or->if rest))))))

(put! 'and (lambda (exp env)
              (eval (and->if exp) env)))

(put! 'or (lambda (exp env)
              (eval (or->if exp) env)))

;; Ex 4.5
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (cond-body first)
                     (expand-clauses rest))))))
(define (cond-body clause)
  (if (tagged-list? (cond-actions clause) '=>)
      (list (make-lambda '(a) (list (list (caddr clause) 'a)))
            (cond-predicate clause))
      (sequence->exp (cond-actions clause))))

;; Ex 4.6
(define (let->combination exp)
  (let ((vars (cadr exp))
        (body (cddr exp)))
    (cons
     (make-lambda (map car vars) body)
     (map cadr vars))))
(put! 'let (lambda (exp env)
             (eval (let->combination exp) env)))

;; Ex 4.7
(define (let*->nested-lets exp)
  (if (null? (cadr exp))
      (cddr exp)
      (cons 'let
            (cons (list (caadr exp))
                  (if (null? (cdadr exp))
                      (cddr exp)
                      (list (let*->nested-lets (cons 'let*
                                                     (cons (cdadr exp)
                                                           (cddr exp))))))))))
(put! 'let* (lambda (exp env)
              (eval (let*->nested-lets exp) env)))

;; Ex 4.8
;; (let <bindings> <body>)       -- normal let
;; (let <var> <bindings> <body>) -- named let
(define (normal-let->combination exp)
  (let ((vars (cadr exp))
        (body (cddr exp)))
    (cons
     (make-lambda (map car vars) body)
     (map cadr vars))))

(define (named-let->combination exp)
  (let ((name (cadr exp))
        (vars (caddr exp))
        (body (cdddr exp)))
    (list (make-lambda '(iter) (list (cons 'iter (cons 'iter (map cadr vars)))))
          (make-lambda
           (cons 'ext (map car vars))
           (list (cons 'let
                       (cons (list
                        (list name
                              (make-lambda (map car vars)
                                           (list (cons 'ext
                                                       (cons 'ext
                                                             (map car vars)))))
                              ))
                       body)))))))

;; (let loop ((i 10) (dest 0))
;;   (if (= i 0)
;;       dest
;;       (loop (- i 1) (+ dest i))))

;; ((lambda (iter)
;;    (iter iter 10 0))
;;  (lambda (loop-ext i dest)
;;    (let ((loop (lambda (i dest) (loop-ext loop-ext i dest))))
;;      (if (= i 0)
;;          dest
;;          (loop (- i 1) (+ dest i))))))

(define (let->combination exp)
  (if (pair? (cadr exp))
      (normal-let->combination exp)
      (named-let->combination exp)))

;; 4.1.3

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;; Procedure

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

;; Environment

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (test-environment-procedures)
  (let* ((env1 (extend-environment (list 'a 'b) (list 1 2) the-empty-environment))
         (env2 (extend-environment (list 'c) (list 3) env1))
         (env3 (extend-environment (list 'a) (list 4) env2)))
    (test* "lookup-variable-value - first frame"
           4 (lookup-variable-value 'a env3))
    (test* "lookup-variable-value - deeper frame"
           2 (lookup-variable-value 'b env3))

    (test* "lookup-variable-value - error"
           (test-error) (lookup-variable-value 'x env3))

    (set-variable-value! 'b 5 env3)
    (test* "set-variable-value!" 5 (lookup-variable-value 'b env3))

    (test* "set-variable-value! - error" (test-error) (lookup-variable-value 'y env3))

    (define-variable! 'd 6 env3)
    (test* "define-variable!" 6 (lookup-variable-value 'd env3))
    (define-variable! 'a 7 env3)
    (test* "define-variable! - override" 7 (lookup-variable-value 'a env3))
    )
)

(test-environment-procedures)

;; Ex 4.11
(test-section "Ex 4.11")

(define (make-frame variables values)
  (map cons variables values))

(define (frame-variables frame) (map car frame))

(define (frame-values frame) (map cdr frame))

(define (add-binding-to-frame! var val frame)
  (let ((tail (cons (car frame) (cdr frame))))
    (set-car! frame (cons var val))
    (set-cdr! frame tail)))

(test* "make-frame" '((a . 1) (b . 2) (c . 3)) (make-frame '(a b c) '(1 2 3)))
(test* "frame-values" '(1 2 3) (frame-values (make-frame '(a b c) '(1 2 3))))
(test* "frame-variables" '(a b c)
       (frame-variables (make-frame '(a b c) '(1 2 3))))
(let ((frame (make-frame '(a b c) '(1 2 3))))
  (add-binding-to-frame! 'x 4 frame)
  (test* "add-binding-to-frame!" '((x . 4) (a . 1) (b . 2) (c . 3)) frame)
  )

;; (test-environment-procedures) => fail

;; Ex 4.12
(test-section "Ex 4.12")

(define (scan-frame pairs var found not-found)
  (cond ((null? pairs)
         (not-found))
        ((eq? var (caar pairs))
         (found (car pairs)))
        (else (scan-frame (cdr pairs) var found not-found))))

(define (env-loop var env success fail)
  (if (eq? env the-empty-environment)
      (fail)
      (let ((frame (first-frame env)))
        (scan-frame frame
                    var
                    success
                    (lambda ()
                      (env-loop var (enclosing-environment env) success fail))
                    ))))

(define (lookup-variable-value var env)
  (env-loop var env (^[pair] (cdr pair)) (^[] (error "Unbound variable" var))))

(define (set-variable-value! var val env)
  (env-loop var env
            (^[pair] (set-cdr! pair val))
            (^[] (error "Unbound variable -- SET!" var))))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (scan-frame frame var
                (^[pair] (set-cdr! pair val))
                (^[] (add-binding-to-frame! var val frame)))
    ))

(test-environment-procedures)

;; Ex 4.13
(define (unbound! var env)
  (let ((frame (first-frame env)))
    (scan-frame frame var
                (^[pair] (set-car! pair ()))
                (^[] (error "Unbound variable -- UNBOUND")))
    ))

(let* ((env1 (extend-environment (list 'a 'b) (list 1 2) the-empty-environment))
       (env2 (extend-environment (list 'c) (list 3) env1)))
  (unbound! 'c env2)
  (test* "unbound!" (test-error) (lookup-variable-value 'c env2))
  (test* "unbound! - unbound" (test-error) (unbound! 'a env2))
  )

(test-end)


;; 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-name)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

;; (define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        ;; ...
        (list 'debug print)
        ))

(define (primitive-procedure-name)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")



(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(let ((the-global-environment (setup-environment)))
  (eval '(define a 1) the-global-environment)
  (eval '(define (append x y)
           (if (null? x)
               y
               (cons (car x)
                     (append (cdr x) y))))
        the-global-environment)
  (eval '(append '(a b c) '(d e f)) the-global-environment)
  )

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     ))
      (display object)))

(define (start-driver-loop)
  (define the-global-environment (setup-environment))
  (define (driver-loop)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (let ((output (eval input the-global-environment)))
        (announce-output output-prompt)
        (user-print output)))
    (driver-loop))
  (driver-loop))
