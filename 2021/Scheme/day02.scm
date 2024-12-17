
(load "utils.scm")

(define symbols (read-input "../data/day02.in"))

(define (solve input)
  (let loop ((pos 0) (aim 0) (depth 0) (input input))
    (if (null? input)
      (cons (* pos aim) (* depth pos))
      (let ((value (cadr input))
            (rest (cddr input)))
        (case (car input)
          ((up) (loop pos (- aim value) depth rest))
          ((down) (loop pos (+ aim value) depth rest))
          ((forward) (loop (+ pos value) aim (+ depth (* aim value)) rest)))))))

(define (main) (solve symbols))

(main)
