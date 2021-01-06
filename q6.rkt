;Alexander Kuhn
;ID 101023154
#lang racket
(define (square x)
  (* x x))

;My functions

(define (cube x)
  (* x (* x x)))

(define (cube-good-enough? guess x)
    (< (abs (- (cube guess) x)) 0.001))

(define (cube-numerator x y)
    (+ (/ x (square y)) (* y 2)))

(define (improve-cube x guess)
    (/ (cube-numerator x guess) 3))

(define (cbrt-iteration procedure guess x)
    (if (procedure guess x)
        guess
        (cbrt-iteration procedure (improve-cube x guess) x)))

(define (cbrt procedure x)
    (cbrt-iteration procedure 1.0 x))

;other good-enoughs
(define (cube-good-enough1? guess x)
    (< (abs (- (cube guess) x)) 0.00001))

(define (cube-good-enough2? guess x)
    (< (abs (- (cube guess) x)) 0.0000001))

(define (cube-good-enough3? guess x)
    (< (abs (- (cube guess) x)) 0.0000000000001))

;non-functional cube iteration
(define (new-if predicate consequent alternate)
   (cond (predicate consequent)
         (else alternate)))

(define (new-cbrt-iteration procedure guess x)
    (new-if (procedure guess x)
      guess
      (new-cbrt-iteration procedure (improve-cube x guess) x)))

(define (new-cbrt procedure x)
    (new-cbrt-iteration procedure 1.0 x))
;We must use the default if here - using new-if results in an infinite loop.
;If is not a standard function, and is evaluated in a different order to prevent things like these from happening.
;But since new-if is an ordinary function, the language evaluates its parameters first - but one of the parameters is the cbrt-iteration function.
;This will be evaluated repeatedly, but it will never stop. So it keeps iterating until the program runs out of memory.

;testing standard cbrt (actual output may differ from computer to computer, but it'll be in the ballpark)
;Easy cases
(display "(cbrt cube-good-enough? 27)=> ")(newline)
   (display "Expected: 3.0000005410641766")(newline)
   (display "Actual: ")(cbrt cube-good-enough? 27)(newline)

(display "(cbrt cube-good-enough? 64)=> ")(newline)
   (display "Expected: 4.000017449510739")(newline)
   (display "Actual: ")(cbrt cube-good-enough? 64)(newline)

;Negatives
(display "(cbrt cube-good-enough? -125)=> ")(newline)
   (display "Expected: -5.000000000076353")(newline)
   (display "Actual: ")(cbrt cube-good-enough? -125)(newline)

(display "(cbrt cube-good-enough? -4096)=> ")(newline)
   (display "Expected: -16.00000000000379")(newline)
   (display "Actual: ")(cbrt cube-good-enough? -4096)(newline)

;Huge cubes
(display "(cbrt cube-good-enough? 125000000)=> ")(newline)
   (display "Expected: 500.00000000007793")(newline)
   (display "Actual: ")(cbrt cube-good-enough? 125000000)(newline)

(display "(cbrt cube-good-enough? -549755813888)=> ")(newline)
   (display "Expected: -8192.0")(newline)
   (display "Actual: ")(cbrt cube-good-enough? -549755813888)(newline)

;Alternate good-enough thresholds
(display "(cbrt cube-good-enough1? 27)=> ")(newline)
   (display "Expected: 3.0000000000000977")(newline)
   (display "Actual: ")(cbrt cube-good-enough1? 27)(newline)

(display "(cbrt cube-good-enough2? 27)=> ")(newline)
   (display "Expected: 3.0000000000000977")(newline)
   (display "Actual: ")(cbrt cube-good-enough2? 27)(newline)

(display "(cbrt cube-good-enough3? 27)=> ")(newline)
   (display "Expected: 3.0")(newline)
   (display "Actual: ")(cbrt cube-good-enough3? 27)(newline)

;You may uncomment the following to see the result of attempting to use new-if
;(display "(new-cbrt cube-good-enough? 8)=> ")(newline)
   ;(display "Expected: An asymptotic approach to 2")(newline)
   ;(display "Actual: ")(new-cbrt cube-good-enough? 8)(newline)