(load "utils.scm")

(define lines (read-input "../data/day01.in"))

(define ((solve input) dist)
  (->> input
       (map > (drop dist input))
       (filter id)
       length))

(define (main)
  (let ((f (solve lines)))
    (cons (f 1) (f 3))))

(main)
