
(define ((point-map x y start-element) input)
  (let ((data (make-vector (* (+ 1 x) (+ 1 y)) start-element)))
    (let loop ((input input))
      (if (null? input)
        data
        (let* ((x0 (caar input))
               (y0 (cdar input))
               (ix (+ x0 (* y0 (+ x 1))))
               (curr (vector-ref data ix)))
          (vector-set! data ix (+ curr 1))
          (loop (cdr input)))))))
    
