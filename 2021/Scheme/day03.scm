
(load "utils.scm")

(define symbols (map string->list (read-line-string "../data/day03.in")))

(define (gamma-epsilon items)
  (define (inner item)
    (let ((ones (count (partial eq? #\1) item))
          (zeros (count (partial eq? #\0) item)))
      (if (>= ones zeros)
        (list #\1 #\0)
        (list #\0 #\1))))
  (transpose (map inner (transpose items))))

(define (oxygen-carbon f items)
  (define (inner items)
    (if (= 1 (length items))
      (car items)
      (let ((val (car (f (gamma-epsilon items)))))
        (cons val 
              (inner (map cdr 
                          (filter (lambda (item) (eq? val (car item)))
                                  items)))))))
  (inner items))

(define (run f items)
  (apply * (map binToInt (f items))))

(define (part2 items)
  (list (oxygen-carbon car items) (oxygen-carbon cadr items)))

(define (main)
  (cons (run gamma-epsilon symbols) (run part2 symbols)))

(main)
