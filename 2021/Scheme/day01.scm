
(load "lib.scm")

(define lines (read-lines "../data/day01.in"))

(define ((solve input) dist)
  (length (filter id (map (lambda (x y) (> x y)) (drop dist input) input))))

(let ((f (solve lines)))
  (cons (f 1) (f 3)))
