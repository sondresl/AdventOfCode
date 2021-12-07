

(load "utils.scm")

(define (parse filename) 
  (let ((line (car (read-line-string filename))))
    (map (compose string->number list->string) (split-on #\, (string->list line)))))

(define (mk-vec input)
  (let ((vec (make-vector 10 0)))
    (for-each (lambda (x)
                (vector-set! vec x (+ 1 (vector-ref vec x))))
              input)
    vec))


(define (run-vec turns vec)
  (let loop ((turn 0))
    (if (= turn turns)
      (sum (vector->list vec))
      (begin
        (vector-set! vec 7  (+ (vector-ref vec 0) 
                               (vector-ref vec 7)))
        (vector-set! vec 9 (vector-ref vec 0))
        (for-each (lambda (ix) (vector-set! vec ix (vector-ref vec (+ 1 ix)))) (range 0 8))
        (vector-set! vec 9 0)
        (loop (+ turn 1))))))

(define (main)
  (let ((vec (mk-vec (parse "../data/day06.in"))))
    (cons (run-vec 80 vec)
          (run-vec (- 256 80) vec)))) ; Vec has been mutated.

(main)

;; (350917 . 1592918715629)
