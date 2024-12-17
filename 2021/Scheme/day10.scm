(load "utils.scm")

(define lines
  (->> (read-line-string "../data/day10.in")
       (map string->list)))

(define (score1 c)
  (case c
    ((#\)) 3)
    ((#\]) 57)
    ((#\}) 1197)
    ((#\>) 25137)))

(define (score2 c)
  (case c
    ((#\() 1)
    ((#\[) 2)
    ((#\{) 3)
    ((#\<) 4)))

(define (match c1 c2)
  (or (and (eq? c1 #\() (eq? c2 #\)))
      (and (eq? c1 #\{) (eq? c2 #\}))
      (and (eq? c1 #\[) (eq? c2 #\]))
      (and (eq? c1 #\<) (eq? c2 #\>))))

(define (run line)
  (define (inner stack line)
    (cond ((null? line) stack)
          ((member (car line) '(#\{ #\( #\< #\[))
           (inner (cons (car line) stack)
                  (cdr line)))
          ((match (car stack) (car line))
           (inner (cdr stack) (cdr line)))
          (else (score1 (car line)))))
  (inner '() line))

(define (middle lst)
  (nth (floor (/ (length lst) 2)) lst))
      
(let* ((res (map run lines))
       (part1 (sum (filter number? res)))
       (part2 (->> (filter pair? res)
                   (map (partial foldl (lambda (acc new)
                                         (+ (* acc 5) (score2 new)))
                                 0))
                   sort
                   middle)))
  (cons part1 part2))
