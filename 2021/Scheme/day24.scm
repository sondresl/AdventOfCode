(load "utils.scm")

(define (parse filename) 
  (define (inner items)
    (if (null? items)
      '()
      (case (car items)
        ((inp) (cons (take 2 items) (inner (drop 2 items))))
        (else (cons (take 3 items) (inner (drop 3 items)))))))
  (define (refine items)
    (list (caddr (nth 4 items)) (caddr (nth 5 items)) (caddr (nth 15 items))))
  (map refine (chunks 18 (inner (read-input filename)))))

(define items (parse "../data/day24.in"))

(define a car)
(define b cadr)
(define c caddr)

(define (next nums vals)
  (call-with-current-continuation
    (lambda (ret)
      (define (inner res vals z)
        (if (and (null? vals) (zero? z))
          (ret res)
          (let ((guesses (if (= 26 (a (car vals)))
                           (filter (lambda (x) (and (< x 10) (< 0 x))) (list (+ (b (car vals)) (modulo z 26))))
                           nums)))
            (for-each (lambda (w)
                        (inner (+ (* 10 res) w)
                               (cdr vals)
                               (if (= w (+ (b (car vals)) (modulo z 26)))
                                 (floor (/ z (a (car vals))))
                                 (+ w (c (car vals)) (* 26 (floor (/ z (a (car vals)))))))))
                      guesses))))
      (inner 0 vals 0))))

(let ((input (parse "../data/day24.in")))
  (cons (next '(9 8 7 6 5 4 3 2 1) input)
        (next '(1 2 3 4 5 6 7 8 9) input)))
