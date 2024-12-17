

(load "utils.scm")

(define (point-map size input)
  (let ((data (make-vector size 0)))
    (let loop ((input input) (ix 0))
      (if (null? input)
        data
        (begin
          (vector-set! data ix (car input))
          (loop (cdr input) (inc ix)))))))

(define (vector-map! f vec)
  (let ((len (vector-length vec)))
    (for-each (lambda (n) 
                (vector-set! vec n (f (vector-ref vec n))))
              (range 0 (dec len)))))

(define (neighbours n)
  (define (index x y)
    (+ (* y 10) x))
  (let ((x (modulo n 10))
        (y (floor (/ n 10))))
    (->> (list (cons (dec x) (dec y))
               (cons (dec x) y)
               (cons (dec x) (inc y))
               (cons x (dec y))
               (cons x (inc y))
               (cons (inc x) (dec y))
               (cons (inc x) y)
               (cons (inc x) (inc y)))
         (filter (lambda (p)
                   (and (not (negative? (car p)))
                        (not (negative? (cdr p)))
                        (< (car p) 10)
                        (< (cdr p) 10))))
         (map (uncurry index)))))

(define (indices f vec)
  (->> (vector->list vec)
       (map cons (range 0 99))
       (filter (compose f cdr))
       (map car)))

(define (mk-vec filename)
  (->> (read-line-string filename)
       (map string->list)
       (apply append)
       (map (lambda (x) (- (char->integer x) 48)))
       (point-map 100)))

(define (flash! n vec)
  (vector-set! vec n 0)
  (let ((ns (neighbours n)))
    (for-each (lambda (x) 
                (let ((curr (vector-ref vec x))) 
                  (vector-set! vec x (if (zero? curr) curr (inc curr)))))
              ns)))

(define (step vec)
  (let ((flash-count 0))
    (vector-map! inc vec) ;; Mutate the vector
    (let loop ((to-flash (indices (partial < 9) vec)))
      (if (null? to-flash)
        flash-count
        (begin
          (flash! (car to-flash) vec)
          (set! flash-count (inc flash-count))
          (loop (indices (partial < 9) vec)))))))

(define (run n vec)
  (map (lambda (x) (step vec))
       (range 1 n)))

(define (main)
  (let* ((vec (mk-vec "../data/day11.in"))
         (part1 (sum (run 100 vec)))
         (part2 (->> (all-until (partial = 100) (lambda (x) (step vec)) 0)
                     cdr
                     length
                     (+ 100))))
    (cons part1 part2)))

(main)
