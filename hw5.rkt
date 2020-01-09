;; CSE341, Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct isgreater (e1 e2)    #:transparent) ;; if e1 > e2 then 1 else 0
(struct ifnz (e1 e2 e3) #:transparent) ;; if not zero e1 then e2 else e3
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair   (e1 e2) #:transparent) ;; make a new pair
(struct first   (e)     #:transparent) ;; get first part of a pair
(struct second  (e)     #:transparent) ;; get second part of a pair
(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then 1 else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)
;; Take in a racket list of MUPL values and
;; converts it to a MUPL list
(define (racketlist->mupllist rl)
  (if (null? rl)
      (munit)
      (apair (car rl) (racketlist->mupllist (cdr rl)))))

;; Takes in a MUPL list and converts
;; it to a racket list of MUPL values
(define (mupllist->racketlist ml)
  (if (munit? ml)
      '()
      (cons (apair-e1 ml) (mupllist->racketlist (apair-e2 ml)))))
;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.

;; Evaluates an expression under an enviroment
;; if the experssion is a value, evaluates it to itself
(define (eval-under-env e env)
  (cond [(var? e) ;; looks up a variable in the environment
         (envlookup env (var-string e))]
        [(int? e) e]
        [(munit? e) e]
        [(closure? e) e]
        [(apair? e) ;; evaluates the two branches of the pair
         (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        ;; evauates the two expressions to add
        ;; if any one is not an int, raises an error
        ;; else returns an int with num to be
        ;; the sum of the nums from the two expressions
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (int (+ (int-num v1) (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; evaluates the two expressions
        ;; if any one is not an int, raise an error
        ;; else return (int 1) if the num of the first
        ;; expression is larger than that of the second
        ;; returns (int 0) otherwise
        [(isgreater? e)
         (let ([v1 (eval-under-env (isgreater-e1 e) env)]
               [v2 (eval-under-env (isgreater-e2 e) env)])
           (cond [(not (and (int? v1) (int? v2))) (error "MUPL comparison applied to non-number")]
                 [(> (int-num v1) (int-num v2)) (int 1)]
                 [#t (int 0)]))]
        ;; evaluates the first expression
        ;; if it is not an int, raises an error
        ;; else if it is not 0, evaluates the second expression
        ;; evaluates the third expression otherwise
        [(ifnz? e)
         (let ([v1 (eval-under-env (ifnz-e1 e) env)])
           (cond [(not (int? v1)) (error "First expression not an int")]
                 [(not (= 0 (int-num v1))) (eval-under-env (ifnz-e2 e) env)]
                 [#t (eval-under-env (ifnz-e3 e) env)]))]
        ;; evaluates a function to a closure containing
        ;; the function and the current environment
        [(fun? e)
         (closure env e)]
        ;; creates a local binding and extends it to the current environment
        ;; evaluates the body within this new environment
        [(mlet? e)
         (let* ([ex-env (cons (mlet-var e) (eval-under-env (mlet-e e) env))]
                [new-env (cons ex-env env)])
          (eval-under-env (mlet-body e) new-env))]
        ;; calls a function
        ;; binds the function name to function body
        ;; binds the parameter name to the passed in argument
        ;; extends the enviroment from the function closure with the new bindings
        ;; evaluates the function body within this new environment
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)])
           (if (closure? v1)
               (let* ([old-env (closure-env v1)]
                      [func (closure-fun v1)]
                      [fun-name (fun-nameopt func)]
                      [para (cons (fun-formal func) (eval-under-env (call-actual e) env))])
                 (if (null? fun-name)
                     (let ([new-env (cons para old-env)])
                       (eval-under-env (fun-body func) new-env))
                     (let* ([ex-env (cons fun-name v1)]
                            [new-env (cons ex-env (cons para old-env))])
                       (eval-under-env (fun-body func) new-env))))
               (error "Calling a non-function")))]
        ;; evaluates the expression
        ;; if it is not a pair, raises an error
        ;; else, accessing its first branch
        [(first? e)
         (let ([temp (eval-under-env (first-e e) env)])
           (if (apair? temp)
               (apair-e1 temp)
               (error "Accessing the first field on a non-pair value")))]
        ;; evaluates the expression
        ;; if it is not a pair, raises an error
        ;; else, accessing the second branch
        [(second? e)
         (let ([temp (eval-under-env (second-e e) env)])
           (if (apair? temp)
               (apair-e2 temp)
               (error "Accessing the second field on a non-pair value")))]
        ;; evaluates the expression
        ;; if it is munit returns (int 1)
        ;; otherwise returns (int 0)
        [(ismunit? e)
         (if (munit? (eval-under-env (ismunit-e e) env))
             (int 1)
             (int 0))]
        ;; CHANGE add more cases here
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

;; takes 3 expressions
;; if the pfirst expression is munit
;; returns the second expression
;; otherwise returns the third expression
(define (ifmunit e1 e2 e3)
  (ifnz (isgreater (ismunit e1) (int 0))
        e2
        e3))

;; takes in a racket list and an expression
;; creates a series of locl bindings with elements from the list
;; and evaluates the expression with these bindings
(define (mlet* bs e2)
  (if (null? bs)
      e2
      (mlet (caar bs)
            (cdar bs)
            (mlet* (cdr bs) e2))))

;; takes in 4 expressions
;; raises an error if any one of the first 2 expressions
;; is not an int
;; if the first 2 expressions are equal integers, returns e3
;; otherwise returns e4
(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifnz (isgreater (var "_x") (var "_y"))
               e4
               (ifnz (isgreater (var "_y") (var "_x"))
                     e4
                     e3))))

;; Problem 4

;; takes in a function
;; returns another function that takes in a MUPL list
;; this new functions returns a MUPL list containing
;; elements from the passed in list that returns non-zero
;; int when passed into the function parameter
(define mupl-filter
  (fun "mupl-filter" "f"
       (fun #f "ls"
            (ifmunit (var "ls")
                     (munit)
                     (let* ([fst (first (var "ls"))]
                            [scd (second (var "ls"))])
                       (ifnz (call (var "f") fst)
                             (apair fst (call (var #f) scd))
                             (call (var #f) scd)))))))

;; takes in a MUPL integer
;; returns a function that takes in a MUPL int list
;; and returns a list containing elements
;; larger than the passed int
(define mupl-all-gt
  (mlet "filter" mupl-filter
        (fun "mupl-all-gt" "i"
             (call (var "filter") (fun #f "j"
                                       (isgreater (var "j") (var "i")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
