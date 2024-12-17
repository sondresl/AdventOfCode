
(define (range from to)
  (if (> from to)
      '()
      (cons from
            (range (+ 1 from) to))))

(define (range . args)
  (define (inner from to step)
    (if (> from to)
        '()
        (cons from
              (inner (+ step from)
                     to
                     step))))
  (case (length args)
    ((1) (inner 0 (car args) 1))
    ((2) (inner (car args) (cadr args) 1))
    ((3) (inner (car args) (cadr args) (caddr args)))))

(define (range . args)
  (define (inner from to step-f)
    (if (> from to)
        '()
        (cons from
              (inner (step-f from)
                     to
                     step-f))))
  (define (inc x) (+ x 1))
  (case (length args)
    ((1) (inner 1 (car args) inc))
    ((2) (inner (car args) (cadr args) inc))
    ((3) (inner (car args) (cadr args) (caddr args)))))


(define (take n items)
  (if (or (zero? n)
          (null? items))
      '()
      (cons (car items)
            (take (- n 1)
                  (cdr items)))))

(define (drop n items)
  (if (or (zero? n)
          (null? items))
      items
      (drop (- n 1)
            (cdr items))))

(define (init items)
  (if (null? (cdr items))
      '()
      (cons (car items)
            (init (cdr items)))))

(define (last items)
  (if (null? (cdr items))
      (car items)
      (last (cdr items))))

(define (nth n items)
  (car (drop n items)))

(define (my-append items1 items2)
  (if (null? items1)
      items2
      (cons (car items1)
            (my-append (cdr items1)
                       items2))))

(define (my-length items)
  (if (null? items)
      0
      (+ 1 (my-length (cdr items)))))

(define (my-reverse items)
  (define (inner new old)
    (if (null? old)
        new
        (inner (cons (car old) new)
               (cdr old))))
  (inner '() items))


(define (slice start end items)
  (drop start (take (+ 1 end) items)))

(define (second-last items)
  (last (init items)))


(define (palindrome? items)
  (equal? items (reverse items)))

(define (insert-at n item items)
  (append (take n items)
          (list item)
          (drop n items)))


(define (reduce proc def items)
  (if (null? items)
      def
      (proc (car items)
            (reduce proc def (cdr items)))))

(define (filter pred items)
  (cond ((null? items) '())
        ((pred (car items))
         (cons (car items)
               (filter pred (cdr items))))
        (else (filter pred (cdr items)))))

(define (count pred items)
  (length (filter pred items)))

(define (take-while pred items)
  (if (or (null? items)
          (not (pred (car items))))
      '()
      (cons (car items)
            (take-while pred (cdr items)))))

(define (drop-while pred items)
  (if (or (null? items)
          (not (pred (car items))))
      items
      (drop-while pred (cdr items))))

(define (group-by proc items)
  (if (null? items)
      '()
      (let* ((val (proc (car items)))
             (test (lambda (x) (eq? (proc x) val))))
        (cons (take-while test items)
              (group-by proc (drop-while test items))))))

(define (group items)
  (group-by (lambda (x) x) items))

(define (compress items)
  (map car (group items)))


(define (encode items)
  (map (lambda (x) (list (length x) (car x)))
       (group items)))

(define (replicate n element)
  (if (zero? n)
      '()
      (cons element (replicate (- n 1) element))))

(define (decode pairs)
  (apply append (map (lambda (x) (replicate (car x) (cadr x))) pairs)))


(define (filter-not pred items)
  (filter (lambda (x) (not (pred x))) items))



(define (partition pred items)
  (list (filter pred items)
        (filter-not pred items)))



(define (until pred proc seed)
  (if (pred seed)
      seed
      (until pred proc (proc seed))))

(define (all-until pred proc seed)
  (if (pred seed)
      (list seed)
      (cons seed
            (all-until pred proc (proc seed)))))


(define (fib-step pair)
  (cons (cdr pair)
        (+ (car pair)
           (cdr pair))))

(define (fib-bigger-than n)
  (car (until (lambda (x) (< n (car x)))
              fib-step
              '(1 . 1))))

(define (even-fibs n)
  (filter even?
          (map car (all-until (lambda (x) (< n (car x)))
                              fib-step
                              (cons 1 1)))))

(define (collatz-step n)
  (cond ((= 1 n) 1)
        ((even? n) (/ n 2))
        (else (+ (* 3 n) 1))))

(define (collatz n)
  (all-until (lambda (x) (= x 1))
             collatz-step
             n))

(define (collatz-lengths-count n m)
  (length
    (filter (lambda (x) (<= m (length x)))
            (map collatz (range 1 n)))))

(define-syntax ->>
  (syntax-rules ()
    ((_ var) var)
    ((_ var (f1 ...) f2 ...)
     (->> (f1 ... var) f2 ...))
    ((_ var f1 f2 ...)
     (->> (f1 var) f2 ...))))

(define (fmap f . items)
  (if (null? items)
      (lambda items
        (apply map f items))
      (apply map f items)))

(define (foldr f def lst)
  (if (null? lst)
      def
      (f (car lst) (foldr f def (cdr lst)))))

(define (foldl f acc lst)
  (if (null? lst)
      acc
      (foldl f (f acc (car lst)) (cdr lst))))

(define (scanl f init lst)
  (if (null? lst)
      (list init)
      (cons init
            (scanl f (f init (car lst)) (cdr lst)))))

(define (scanr f def lst)
  (if (null? lst)
      (list def)
      (let ((rest (scanr f def (cdr lst)))) ; Recursively make the rest of the list
        (cons (f (car lst) (car rest))
              rest))))

(define ((flip f) x y)
  (f y x))

(define (sum lst)
  (apply + lst))

(define (range . args)
  (define (iter from to step)
    (if (> from to)
        '()
        (cons from (iter (+ from step) to step))))
  (case (length args)
    ((1) (iter 0 (car args) 1))
    ((2) (iter (car args) (cadr args) 1))
    ((3) (iter (car args) (cadr args) (caddr args)))))

(define (filter pred lst)
  (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))

(define (complement f)
  (compose not f))

(define (filter-not pred lst)
  (filter (complement pred) lst))

(define (take n lst)
  (if (or (null? lst) (zero? n))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

(define (drop n lst)
  (if (or (null? lst) (zero? n))
      lst
      (drop (dec n) (cdr lst))))

(define (chunks n items)
  (if (null? items)
    '()
    (cons (take n items)
          (chunks n (drop n items)))))

(define (transpose items)
  (apply map list items))

(define (tails items)
  (if (null? items)
    '(())
    (cons items
          (tails (cdr items)))))

(define (sliding n items)
  (map reverse (transpose (reverse (take n (tails items))))))

(define (repeat lst n)
  (if (zero? n)
      '()
      (append lst (repeat lst (- n 1)))))

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (last lst)
  (if (null? (cdr lst)) 
    (car lst))
    (last (cdr lst)))

(define (id x) x)

(define (compose f g)
  (lambda args (f (apply g args))))

(define (comp f . funcs)
  (foldl compose f funcs))

(define partial
  (lambda (f . givens)
    (lambda args
      (apply f (append givens args)))))

(define (const x)
  (lambda args
    x))

(define (compareBy comp f start . args)
  (cdr (let ((init (cons (f start) start)))
         (foldl (lambda (old y)
                  (let ((new (cons (f y) y)))
                    (if (comp (car old) (car new))
                        new
                        old)))
                init args))))

(define (maximumBy f items)
  (apply (partial compareBy <) f (car items) (cdr items)))

(define (minimumBy f items)
  (apply (partial compareBy >) f (car items) (cdr items)))

(define (maximum items)
  (apply max items))

(define (sortBy comp items)
  (if (null? items)
      '()
      (let* ((pivot (car items))
             (left (filter (partial comp pivot) (cdr items)))
             (right (filter (complement (partial comp pivot)) (cdr items))))
        (append (sortBy comp left)
                (list pivot)
                (sortBy comp right)))))

(define sort (partial sortBy >))

(define (find f items)
  (if (null? items)
      #f
      (or (and (f (car items)) items)
          (find f (cdr items)))))

(define (take-while pred items)
  (if (or (null? items) (not (pred (car items))))
      '()
      (cons (car items)
            (take-while pred (cdr items)))))

(define (all-until pred proc seed)
  (if (not (pred seed))
      '()
      (cons seed
            (all-until pred proc (proc seed)))))

(define (binToInt items)
  (let ((ord (lambda (x) (- (char->integer x) 48))))
    (foldl (lambda (acc new) (+ (ord new) (* 2 acc))) 0 items)))

;; File input
(define (read-input filename)
  (define (inner port)
    (let ((val (read port)))
      (if (eof-object? val)
        '()
        (cons val (inner port)))))
  (let ((file (open-input-file filename)))
    (inner file)))

(define (read-line-string filename)
  (let ((file (open-input-file filename)))
    (define (line)
      (let ((val (read-char file)))
        (if (or (eof-object? val) (eq? #\newline val))
          '()
          (cons val (line)))))
    (define (inner-loop)
      (if (eof-object? (peek-char file))
        '()
        (cons (line) (inner-loop)))) ;; 'Abuse' mutable object
    (map list->string (inner-loop))))

;; debug
(define (print x)
  (display x)
  (newline))
