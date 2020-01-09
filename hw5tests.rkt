#lang racket

(require "hw5.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

; Note we have provided [only] 3 tests, but you can't run them until do some of the assignment.
; You will want more tests.

(require rackunit)

(racketlist->mupllist '(1 1 4 5 1 4 ("mur" "kmr" "szk") 1 9 1 9 8 1 0))
(mupllist->racketlist (racketlist->mupllist '(1 1 4 5 1 4 ("mur" "kmr" "szk") 1 9 1 9 8 1 0)))

(eval-exp (ifmunit (int 5) (int 1) (int 0)))
(eval-exp (ifmunit (munit) (int 1) (int 0)))
(eval-exp (call (call mupl-filter (fun "func" "i" (isgreater (var "i") (int 3)))) (apair (int 1) (apair (int 5) (apair (int 2) (munit))))))
(eval-exp (call (call mupl-filter (fun "func" "i" (isgreater (int 3) (var "i")))) (apair (int 1) (apair (int 5) (apair (int 2) (munit))))))
(eval-exp (ifeq (int 3) (int 3) (int 4) (int 5)))
(eval-exp (ifeq (int 3) (int 5) (int 4) (int 5)))
(eval-exp (mlet "x" (int 3) (call (fun #f "i" (isgreater (var "i") (var "x"))) (int 5))))

(define tests
  (test-suite
   "Homework 5 Tests"

   (check-equal? (eval-exp (add (int 2) (int 2))) (int 4) "add simple test")

   (check-exn (lambda (x) (string=? (exn-message x) "MUPL addition applied to non-number"))
              (lambda () (eval-exp (add (int 2) (munit))))
              "add bad argument")

   (check-equal? (mupllist->racketlist
                  (eval-exp (call (call mupl-all-gt (int 9))
                                  (racketlist->mupllist 
                                   (list (int 10) (int 9) (int 15))))))
                 (list (int 10) (int 15))
                 "provided combined test using problems 1, 2, and 4")
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
