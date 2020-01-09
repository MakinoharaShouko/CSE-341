#lang racket

(require "hw4.rkt") 

;; A simple library for displaying a 2x3 grid of pictures: used
;; for fun in the tests below (look for "Tests Start Here").

(require (lib "graphics.rkt" "graphics"))

(open-graphics)

(define window-name "Programming Languages, Homework 4")
(define window-width 700)
(define window-height 500)
(define border-size 100)

(define approx-pic-width 200)
(define approx-pic-height 200)
(define pic-grid-width 3)
(define pic-grid-height 2)

(define (open-window)
  (open-viewport window-name window-width window-height))

(define (grid-posn-to-posn grid-posn)
  (when (>= grid-posn (* pic-grid-height pic-grid-width))
    (error "picture grid does not have that many positions"))
  (let ([row (quotient grid-posn pic-grid-width)]
        [col (remainder grid-posn pic-grid-width)])
    (make-posn (+ border-size (* approx-pic-width col))
               (+ border-size (* approx-pic-height row)))))

(define (place-picture window filename grid-posn)
  (let ([posn (grid-posn-to-posn grid-posn)])
    ((clear-solid-rectangle window) posn approx-pic-width approx-pic-height)
    ((draw-pixmap window) filename posn)))

(define (place-repeatedly window pause stream n)
  (when (> n 0)
    (let* ([next (stream)]
           [filename (cdar next)]
           [grid-posn (caar next)]
           [stream (cdr next)])
      (place-picture window filename grid-posn)
      (sleep pause)
      (place-repeatedly window pause stream (- n 1)))))

;; Tests Start Here

; These definitions will work only after you do some of the problems
; so you need to comment them out until you are ready.
; Add more tests as appropriate, of course.
(define nums (sequence 1 0 5))

(equal? (sequence 1 0 5) '(0 1 2 3 4 5))
(equal? (sequence 2 3 11) '(3 5 7 9 11))
(equal? (sequence 1 15 11) '())

(equal? (string-append-map '("hello" "happy" "world") "114514") '("hello114514" "happy114514" "world114514"))
(equal? (string-append-map '() "KMR") '())

(equal? (list-nth-mod '("mur" "kmr" "1919" "tadokoro") 2) "1919")

(equal? (stream-for-k-steps dan-then-dog 5) '("dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg" "dan.jpg"))

(equal? (stream-for-k-steps (stream-add-one funny-number-stream) 8) '((1 . 1) (1 . 2) (1 . 3) (1 . 4) (1 . 5) (1 . -6) (1 . 7) (1 . 8)))
(equal? (stream-for-k-steps (stream-add-one dan-then-dog) 5) '((1 . "dan.jpg") (1 . "dog.jpg") (1 . "dan.jpg") (1 . "dog.jpg") (1 . "dan.jpg")))

(equal? (stream-for-k-steps (cycle-lists '(1 2 3) '("a" "b")) 5) '((1 . "a") (2 . "b") (3 . "a") (1 . "b") (2 . "a")))

(equal? (vector-assoc "mur" #((10 45) 4 5 35 ("mur" 114514) ("mur" "kmr"))) '("mur" 114514))
(equal? (vector-assoc "mur" #((10 45) 4 5 35 ("mur" 114514 1919810) ("mur" "kmr"))) '("mur" 114514 1919810))

(define files (string-append-map 
               (list "dan" "dog" "curry" "dog2") 
               ".jpg"))

(define funny-test (stream-for-k-steps funny-number-stream 16))

; a zero-argument function: call (one-visual-test) to open the graphics window, etc.
(define (one-visual-test)
  (place-repeatedly (open-window) 0.5 (cycle-lists nums files) 27))

; similar to previous but uses only two files and one position on the grid
(define (visual-one-only)
  (place-repeatedly (open-window) 0.5 (stream-add-one dan-then-dog) 28))
