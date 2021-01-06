;Alexander Kuhn
;ID 101023154
#lang racket
(define (forAll x y)
  (define (trimResult l count)
    (if (= count 1) l
        (trimResult (cdr l) (- count 1))))
  (trimResult (cdr(append (build-list y values) (list y))) x))

(define (my_iterator procedure x l)
  (if (not (null? l))
  (my_iterator procedure (procedure x (car l)) (cdr l))
  x))

;Not as specified - tried to follow advice and design without the closure first, but ran out of time
;Mathematical operations work, but the lambda doesn't

(display "(my_iterator + 0 (forAll 1 5))=> ")(newline)
   (display "Expected: 15")(newline)
   (display "Actual: ")(my_iterator + 0 (forAll 1 5))(newline)

(display "(my_iterator * 1 (forAll 1 5))=> ")(newline)
   (display "Expected: 120")(newline)
   (display "Actual: ")(my_iterator * 1 (forAll 1 5))(newline)

(display "(my_iterator (lambda (x y) (display x)(display '' '')) '''' (forAll 1 5))=> ")(newline)
   (display "Expected: 1 2 3 4 5")(newline)
   (display "Actual: ")(my_iterator (lambda (x y) (display x)(display " ")) "" (forAll 1 5))(newline)

