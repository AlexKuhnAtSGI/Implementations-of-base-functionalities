;Alexander Kuhn
;ID 101023154
#lang racket
;sum function left intact for purposes of comparison
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (inc x) (+ x 1))
(define (identity x) x)
(define (sum-integers a b)
	(sum identity a inc b))

(define (sum-iter a b)
  (define (sum-step curr goal total)
    (if (> curr goal) total
        (sum-step (+ curr 1) goal (+ total curr))))
  (sum-step a b 0))

;testing sum-iter
;easy cases
(display "(sum-iter 1 2)=> ")(newline)
   (display "Expected: 3")(newline)
   (display "Actual: ")(sum-iter 1 2)(newline)

(display "(sum-iter 1 10)=> ")(newline)
   (display "Expected: 55")(newline)
   (display "Actual: ")(sum-iter 1 10)(newline)

(display "(sum-iter 5 100)=> ")(newline)
   (display "Expected: 5040")(newline)
   (display "Actual: ")(sum-iter 5 100)(newline)

;more challenging (sum-iter will do these instantly, sum-integers will hang or run out of memory)
(display "(sum-iter 1 1000000)=> ")(newline)
   (display "Expected: 500000500000")(newline)
   (display "Actual: ")(sum-iter 1 1000000)(newline)

(display "(sum-iter 1 16777216)=> ")(newline)
   (display "Expected: 140737496743936")(newline)
   (display "Actual: ")(sum-iter 1 16777216)(newline)