#lang racket

(require racket/trace) ; for dynamic function call traces
(require 2htdp/image) ; for a bit of fun with pictures

(define *car-wheel* (circle 10 "solid" "blue"))

(define *car-body* (beside/align "bottom"
                    (square 20 "solid" "black")
                    (square 35 "solid" "black")
                    (square 20 "solid" "black")))

(define *car* (let ([wheels (beside *car-wheel*
                                   (rectangle 20 0 "solid" "white")
                                   *car-wheel*)])
                (overlay/offset wheels
                                0 -15
                                *car-body*)))
#|-----------------------------------------------------------------------------
;; Function definitions

- `lambda`: creates an anonymous function
- `define` supports a special syntax for binding variables to functions
-----------------------------------------------------------------------------|#

(define foo(lambda (x y) (+ x y)))

(define (bar x y)
  (* x y)
  (/ x y)
  (+ x y))


#|-----------------------------------------------------------------------------
;; Some more special forms

- `begin`: sequences multiple sexps; evaluates to the result of the last
- `if`: if-then-else
- `when`: if-then
- `cond`: multi-way conditional
-----------------------------------------------------------------------------|#

#; (begin body ...)

#; (if test-expr then-expr else-expr)

#; (when test-expr body ...)

#; (cond [expr1 body ...]
         [expr2 body ...]
         [else body ...])


#|-----------------------------------------------------------------------------
;; Equality tests

- `=` for numbers
- `eq?` for pointer-based equality
- `equal?` for value-based equality
-----------------------------------------------------------------------------|#

(define lst '(a b c))

#|-----------------------------------------------------------------------------
;; Recursion
-----------------------------------------------------------------------------|#

;0 1 1 2 3 5 8 13 21
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))(fib (- n 2)))))
(trace fib)

(define (sum-to n)
  (if (= n 0)
      0
      (+ n (sum-to (- n 1)))))
(trace sum-to)

(define (sum-to-2 n s)
  (if (= n 0)
      s
      (sum-to-2 (- n 1) (+ n s))))
(trace sum-to-2)

(define (sum-to-acc n sum)
  (if (= n 0)
      sum
     (sum-to-acc (- n 1) (+ sum n))))
(trace sum-to-acc)


(define(sum-to-acc-2 n)
  (let rec ([i n]
            [acc 0])
    (if (= i 0)
        acc
        (rec (- i 1)(+ acc i)))))
     
(define (loop n sexp)
  (when (> n 0)
    (eval sexp)
    (loop (- n 1) sexp)))

#|-----------------------------------------------------------------------------
;; On lists

- recursion over lists is an example of "structural recursion"
-----------------------------------------------------------------------------|#

(define (length lst)
  (if (empty? lst)
      0
      (+ 1 (length (rest lst)))))
(trace length)

(define (repeat n x)
  (if (= n 0)
      '()
      (cons x (repeat (- n 1) x))))
(trace repeat)
(define (reverse lst)
  (trace-let rec ([lst lst]
            [acc '()])
  (if (empty? lst)
      acc
      (rec (rest lst)(cons (first lst) acc)))))

(define (range n)
  (let rec ([i 0]
            [acc '()])
    (if (= i n)
        (reverse acc)
        (rec (+ i 1) (cons i acc)))))
#|-----------------------------------------------------------------------------
;; Higher-order functions (HOFs)

HOFs either take a function as an argument or return a function.

Some useful built-in HOFs and related functions:
- `eval`: evaluates a sexp
- `apply`: apply a function to a list of arguments
- `curry`: returns a version of a function that can be partially applied
- `compose`: returns a function that is the composition of two other functions
-----------------------------------------------------------------------------|#



#|-----------------------------------------------------------------------------
;; Lexical scope

- A free variable is bound to a value *in the environment where it is defined*, 
  regardless of when it is used
-----------------------------------------------------------------------------|#
