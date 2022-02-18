#lang racket

(require macro-debugger/stepper)
(require (for-syntax racket/list))

;; syntax objects attach syntactical context to sexps

(define s1 (syntax "hello world"))

(define s2 (syntax (foo bar 1 2 3)))

(define s3 #'(foo bar 1 2 3)) ; #' is syntactic sugar for `syntax`

(define s4 #'(if (< 1 2) "true" "false"))

#; (values
    (syntax-source s3)
    (syntax-line s3)
    (syntax-column s3)
    (syntax->datum s3))

;; `eval-syntax` evaluates syntax objects, like `eval` for sexps

#; (eval-syntax s4)


;; `datum->syntax` lets us create syntax objects from sexps (and context)

#; (datum->syntax #f '(println "hello world"))


;; `define-syntax` defines a syntax transformer, aka macro

(define-syntax always-say-hi
  (lambda (stx)
    #'(println "hello")))


(define-syntax quote-myself
  (lambda (stx)
    (datum->syntax stx `(quote ,stx))))


;; we can use `expand/step` (and others) to help us see the expansion of a macro

#; (expand/step #'(quote-myself ha ha ha))

(define-syntax (reversed stx)
  (let ([sexp (syntax->datum stx)])
    (datum->syntax stx (reverse (cdr sexp)))))
    

#; (expand/step #'(reversed 1 2 3 +))

(define-syntax (my-if stx)
  (let ([sexp (syntax->datum stx)])
    (datum->syntax stx
                   `(cond [,(second sexp),(third sexp)]
                          [else ,(fourth sexp)]))))
          


;; `syntax-case` provides us with pattern matching and takes a template
(define-syntax (my-if-2 stx)
  (syntax-case stx ()
    [(_ test exp1 exp2)  #'(cond [test exp1]
                                 [else exp2])]))


;; `define-syntax-rule` lets use more easily define syntax transformers
(define-syntax-rule (my-if-3 test exp1 exp2)
  (cond [test exp1]
        [else exp2]))

(define-syntax-rule (loop n body)
  (let rec ([i 0])
    (when (< i n)
      body
      (rec (+ i 1)))))

;;(for-loop i 10 (println i)

(define-syntax-rule (for-loop var n body)
  (let rec ([var 0])
    (when (< var n)
      body
      (rec (+ var 1)))))

(define-syntax-rule(aif test exp1 exp2)
  (let ([it test])
    (if it
        exp1
        exp2)))
(define-syntax (aif-2 stx)
  (let ([sexp (syntax->datum stx)])
    (datum->syntax stx
                   `(let ([it ,(second sexp)])
                      (if it
                          ,(third sexp)
                          ,(fourth sexp))))))

        

#; (if (long-computation)
    (do-something-with it)
    (else))