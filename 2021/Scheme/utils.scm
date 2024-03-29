
;; Threading macro

(define-syntax ->>
  (syntax-rules ()
    ((_ var) var)
    ((_ var (f1 ...) f2 ...)
     (->> (f1 ... var) f2 ...))
    ((_ var f1 f2 ...)
     (->> (f1 var) f2 ...))))

;; Basic list operatinos

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

(define (slice start end items)
  (drop start (take (+ 1 end) items)))

(define (insert-at n item items)
  (append (take n items)
          (list item)
          (drop n items)))

(define (split-on char items)
  (if (null? items)
    '()
    (let ((new (take-while (lambda (x) (not (eq? x char))) items))
          (rest (drop-while (lambda (x) (not (eq? x char))) items)))
      (cons new
            (split-on char (if (null? rest) rest (cdr rest)))))))

(define (words input)
  (->> (string->list input)
       (split-on #\space)
       (map list->string)))

;; Basic higher-order functions that should be built-in

;; curried map
(define (fmap f . items)
  (if (null? items)
      (lambda items
        (apply map f items))
      (apply map f items)))

(define (filter pred . items)
  (if (null? items)
    (lambda items
      (apply filter pred items))
    (let ((items (car items)))
      (cond ((null? items) '())
            ((pred (car items))
             (cons (car items)
                   (filter pred (cdr items))))
            (else (filter pred (cdr items)))))))

(define (foldr f def lst)
  (if (null? lst)
      def
      (f (car lst) (foldr f def (cdr lst)))))

(define (foldl f acc lst)
  (if (null? lst)
      acc
      (foldl f (f acc (car lst)) (cdr lst))))

(define (reduce proc items)
  (foldl prod (car items) (cdr items)))

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

(define (span f items)
  (cons (take-while f items)
        (drop-while f items)))

(define (break f items)
  (span (complement f) items))

(define partial
  (lambda (f . givens)
    (lambda args
      (apply f (append givens args)))))

(define (group-by proc items)
  (if (null? items)
      '()
      (let* ((val (proc (car items)))
             (test (lambda (x) (eq? (proc x) val))))
        (cons (take-while test items)
              (group-by proc (drop-while test items))))))

(define (group items)
  (group-by (lambda (x) x) items))

(define (compareBy comp f start . args)
  (cdr (let ((init (cons (f start) start)))
         (foldl (lambda (old y)
                  (let ((new (cons (f y) y)))
                    (if (comp (car old) (car new))
                        new
                        old)))
                init args))))

(define (sortBy comp items)
  (if (null? items)
      '()
      (let* ((pivot (car items))
             (left (filter (partial comp pivot) (cdr items)))
             (right (filter (complement (partial comp pivot)) (cdr items))))
        (append (sortBy comp left)
                (list pivot)
                (sortBy comp right)))))

(define (maximumBy f items)
  (apply (partial compareBy <) f (car items) (cdr items)))

(define (minimumBy f items)
  (apply (partial compareBy >) f (car items) (cdr items)))

(define (maximum items)
  (apply max items))

(define (minimum items)
  (apply min items))

(define sort (partial sortBy >))


(define (string-sort str)
  (list->string (sortBy (lambda (x y) (> (char->integer x) (char->integer y))) (string->list str))))

(define (find f items)
  (if (null? items)
      #f
      (or (and (f (car items)) items)
          (find f (cdr items)))))

(define (all f items)
  (cond ((null? items) #t)
        ((f (car items)) (all f (cdr items)))
        (else #f)))

;; Some useful functions, and some combinators

(define (id x) x)

(define (const x)
  (lambda args
    x))

(define ((flip f) x y)
  (f y x))

(define (repeat lst n)
  (if (zero? n)
      '()
      (append lst (repeat lst (- n 1)))))

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (compose f g)
  (lambda args (f (apply g args))))

(define (comp f . funcs)
  (foldl compose f funcs))

(define ((on f g) x y)
  (f (g x) (g y)))

(define ((<*> f g) x)
  (f x (g x)))

(define (ap2 f g h x)
    (f (g x) (h x)))

(define (curry f)
  (lambda (x y)
    (f (cons x y))))

(define (uncurry f)
  (lambda (x)
    (f (car x) (cdr x))))

;; Basic operations

(define (sum lst)
  (apply + lst))

;; Other helper functions

(define (range . args)
  (define (inner from to step-f)
    (if (> from to)
        '()
        (cons from
              (inner (step-f from)
                     to
                     step-f))))
  (case (length args)
    ((1) (inner 1 (car args) inc))
    ((2) (inner (car args) (cadr args) inc))
    ((3) (inner (car args) (cadr args) (caddr args)))))

(define (over-indexes f x y)
  (apply append
         (map (lambda (x)
                (map (lambda (y)
                       (f x y))
                     (range 0 y)))
              (range 0 x))))

(define (count pred items)
  (length (filter pred items)))

(define (replicate n element)
  (if (zero? n)
      '()
      (cons element (replicate (- n 1) element))))

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

(define (iterate-n n f x)
  (if (zero? n)
    x
    (iterate-n (dec n) f (f x))))

(define (complement f)
  (compose not f))

; (define (all-until pred proc seed)
;   (if (not (pred seed))
;       '()
;       (cons seed
;             (all-until pred proc (proc seed)))))

(define (list->integer n items)
  (let ((ord (lambda (x) (- (char->integer x) 48))))
    (foldl (lambda (acc new) (+ (ord new) (* n acc))) 0 items)))

(define (binToInt items)
  (list->integer 2 items))

(define (to-digit items)
  (foldl (lambda (acc new) (+ (* 10 acc) new)) 0 items))

(define (chars->number items)
  (list->integer 10 items))

(define (digit-or-dash c)
  (or (eq? c #\-)
      (member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))

;; Find all the numbers in a string
(define (all-nums items)
  (define (inner items)
    (if (null? items)
      '()
      (let* ((next (drop-while (complement digit-or-dash) items))
             (num (span digit-or-dash next)))
        (cons (list->integer 10 (car num))
              (inner (cdr num))))))
  (inner (string->list items)))


;; https://rosettacode.org/wiki/Permutations#Scheme
(define (permute l)
  (define (insert l n e)
    (if (= 0 n)
      (cons e l)
      (cons (car l) 
            (insert (cdr l) (- n 1) e))))
  (define (seq start end)
    (if (= start end)
      (list end)
      (cons start (seq (+ start 1) end))))
  (if (null? l)
    '(())
    (apply append (map (lambda (p)
                         (map (lambda (n)
                                (insert p n (car l)))
                              (seq 0 (length p))))
                       (permute (cdr l))))))

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
