
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Question 1
;; Takes in 3 arguments, retruns list of all numbers from
;; the second argument to the third argument, incrementing
;; by the first argument every time
(define (sequence spacing low high)
  (if (> low high)
      '()
      (cons low (sequence spacing (+ low spacing) high))))

;; Question 2
;; Takes a string list and a suffix
;; append the suffix to each element in the list
(define (string-append-map xs suffix)
  (map (lambda(str) (string-append str suffix)) xs))

;; Question 3
;; Takes a list and a number
;; if the number is negative or the list is empty
;; raise error, else find the element at the index
;; of the number % list length
(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (null? xs)
          (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))

;; Question 4
;; Takes a stream and a number n
;; returns a list holding first n values generated from the stream
(define (stream-for-k-steps s k)
  (if (= k 0)
      '()
      (cons (car (s)) (stream-for-k-steps (cdr (s)) (- k 1)))))

;; Question 5
;; Generates a stream like a natural number stream but
;; all numbers divisible by 6 are negated
(define funny-number-stream
  (local {
          (define (funny-stream-generator i)
          (lambda() (if (= 0 (remainder i 6))
                        (cons (- 0 i) (funny-stream-generator (+ 1 i)))
                        (cons i (funny-stream-generator (+ 1 i))))))
          }
  (funny-stream-generator 1)))

;; Question 6
;; A stream that prints "dan" and "dog"
(define dan-then-dog
  (local { (define dan (lambda() (cons "dan.jpg" (lambda() (dog)))))
           (define dog (lambda() (cons "dog.jpg" (lambda() (dan)))))
          }
    dan))

;; Question 7
;; Takes a stream and constructs a stream of pairs composed of
;; 1 & elements from the original stream
(define (stream-add-one s)
  (lambda() (cons (cons 1 (car (s))) (stream-add-one (cdr (s))))))

;; Question 8
;; Takes two list and uses the combine the elements from the lists
;; repeaterly to generate a stream
(define (cycle-lists xs ys)
  (local { (define (cycle xs ys n)
             (lambda() (cons (cons (list-nth-mod xs n)
                                   (list-nth-mod ys n))
                             (cycle xs ys (+ n 1)))))
          }
    (cycle xs ys 0)))

;; Question 9
;; Takes a value and a vector
;; Searches and returns the first pair with first element equal to
;; the input value. If no pair is found, returns #f
(define (vector-assoc v vec)
  (local { (define (assoc-help vec n)
             (if (= n (vector-length vec))
                 #f
                 (let ([temp (vector-ref vec n)])
                   (if (and (pair? temp) (equal? (car temp) v))
                       temp
                       (assoc-help vec (+ n 1))))))
          }
    (assoc-help vec 0)))

;; Question 10
;; Implements an improved version of assoc
;; before searching from the list, checks from
;; a vector of recent call results to see if
;; there is a match. If not, do normal assoc
(define (caching-assoc xs n)
  (let ([v (make-vector n #f)][ind 0])
    (local { (define (inner-caching-assoc x)
               (let ([temp (vector-assoc x v)])
                 (if (boolean? temp)
                     (let ([res (assoc x xs)]) (vector-set! v (remainder ind n) res) (set! ind (+ ind 1)) res)
                     temp)))
           }
    inner-caching-assoc)))

;; Question 11
;; Defines a macro that takes two expressions
;; that produce numbers
;; Evaluates the first expression once and
;; evaluates the second expression at least once and repeatedly
;; until its result is less or equal to
;; the result from the firs expression
(define-syntax while-greater
  (syntax-rules (do)
  [(while-greater e1 do e2)
   (let ([x e1])
     (local { (define (inner-recursion e)
               ( if (> (force e) x)
                   (inner-recursion (delay e2))
                   #t))
            }
     (inner-recursion (delay e2))))]))
