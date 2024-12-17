
(load "utils.scm")

(define (parse filename) 
  (let ((line (car (read-line-string filename))))
    (map (compose string->number list->string) (split-on #\, (string->list line)))))

(define (run f val items)
  (foldl (lambda (acc new) (+ acc (f (abs (- new val))))) 0 items))

(define (gauss n)
  (floor (/ (* n (+ 1 n)) 2)))

(define (main)
  (let* ((input (sort (parse "../data/day07.in")))
         (len (length input))
         (total (sum input))
         (median (car (drop (floor (/ len 2)) input)))
         (mean (/ total len)))
    (cons (run id median input)
          (min (run gauss mean input)
               (run gauss (+ 1 mean) input)))))

(main)

;; (348996 . 98231647)
