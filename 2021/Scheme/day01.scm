
(load "utils.scm")

(define lines (read-input "../data/day01.in"))

(define ((solve input) dist)
  (length (filter id (map (lambda (x y) (> x y)) (drop dist input) input))))

(define (main)
  (let ((f (solve lines)))
    (cons (f 1) (f 3))))

(main)
