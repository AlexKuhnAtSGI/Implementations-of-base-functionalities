;Alexander Kuhn
;ID 101023154
#lang racket
(define (pascals row col)
  (if (or (= col 0) (= col row)) 1
      (+ (pascals (- row 1) (- col 1)) (pascals (- row 1) col))
      ))

;Testing Pascal (provided tests)
(display "(pascals 0 0)=> ")(newline)
   (display "Expected: 1")(newline)
   (display "Actual: ")(pascals 0 0)(newline)

(display "(pascals 2 0)=> ")(newline)
   (display "Expected: 1")(newline)
   (display "Actual: ")(pascals 2 0)(newline)

(display "(pascals 2 1)=> ")(newline)
   (display "Expected: 2")(newline)
   (display "Actual: ")(pascals 2 1)(newline)

(display "(pascals 4 2)=> ")(newline)
   (display "Expected: 6")(newline)
   (display "Actual: ")(pascals 4 2)(newline)

;Bigger tests
(display "(pascals 6 3)=> ")(newline)
   (display "Expected: 20")(newline)
   (display "Actual: ")(pascals 6 3)(newline)

(display "(pascals 13 5)=> ")(newline)
   (display "Expected: 1287")(newline)
   (display "Actual: ")(pascals 13 5)(newline)

(display "(pascals 16 8)=> ")(newline)
   (display "Expected: 12870")(newline)
   (display "Actual: ")(pascals 16 8)(newline)