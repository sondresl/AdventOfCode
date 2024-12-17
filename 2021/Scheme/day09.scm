
(load "utils.scm")

(define (point-map size input)
  (let ((data (make-vector size 0)))
    (let loop ((input input) (ix 0))
      (if (null? input)
        data
        (begin
          (vector-set! data ix (car input))
          (loop (cdr input) (inc ix)))))))

(define (parse filename) 
  (define (char->num ch)
    (- (char->integer ch) 48))
  (let ((nums (apply append 
                     (map (compose (fmap char->num) string->list) 
                          (read-line-string filename)))))
    (point-map (length nums)
               nums)))

(define ((indexfn vec) x y)
  (cond ((> x 99) 9)
        ((> y 99) 9)
        ((or (> 0 x) (> 0 y)) 9)
        (else (vector-ref vec (+ (* y 100) x)))))

(define (neighbours x y)
  (list (cons (+ x 1) y)
        (cons x (+ y 1))
        (cons (- x 1) y)
        (cons x (- y 1))))

(define ((find-neighbours index-fn) x y)
  (map (lambda (x) (index-fn (car x) (cdr x))) (neighbours x y)))

(define (low-points vec mx my)
  (let* ((index (indexfn vec))
         (neighs (find-neighbours index)))
    (over-indexes 
      (lambda (x y)
        (let* ((val (index x y))
               (nvals (neighs x y)))
          (if (all (lambda (x) (> x val)) nvals)
            (cons (cons x y) val)
            #f)))
      mx my)))

(define (bfs vec x y)
  (let* ((index (indexfn vec))
         (neighs (find-neighbours index))
         (val (index x y)))
    (define (inner queue seen)
      (if (null? queue) 
        '()
        (let* ((x (caar queue))
               (y (cdar queue))
               (ns (->> (neighbours x y)
                        (filter (lambda (x) (not (= 9 (index (car x) (cdr x))))))
                        (filter (lambda (x) (not (member x seen)))))))
          (cons (car queue)
                (inner (append (cdr queue) ns) (append seen ns))))))
    (inner (list (cons x y)) (list (cons x y)))))

(define (main)
  (let* ((vec (parse "../data/day09.in"))
         (lps (filter id (low-points vec 99 99))))
    (cons (sum (map (compose inc cdr) lps))
          (->> (map (lambda (x) (bfs vec (caar x) (cdar x))) lps)
               (map length)
               sort
               reverse
               (take 3)
               (foldr * 1)))))

(main)
