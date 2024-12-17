
(load "utils.scm")

;; Custom parsing
(define (parse filename)
  ;; nums as string "1,2,3,4,5"
  (define (parse-nums-string char nums)
    (let ((nums (string->list nums)))
      (map string->number (map list->string (split-on char nums)))))
  (let* ((lines (read-line-string filename))
         (nums (parse-nums-string #\, (car lines)))
         (boards (->> (cdr lines)
                      (filter (compose not (partial string=? "")))
                      (map (partial parse-nums-string #\space))
                      (map (partial filter id))
                      (chunks 5))))
    (cons nums boards)))

(define ((one-board nums) board)
  (define (clear-num n rows)
    (map (lambda (row) (filter (lambda (x) (not (eq? n x))) row)) rows))
  (define (inner count prev nums rows)
    (if (not (null? (filter null? rows)))
      (list count (sum (map sum (take 5 rows))) prev)
      (inner (+ count 1)
             (car nums)
             (cdr nums)
             (clear-num (car nums) rows))))
  (let ((allrows (append board (transpose board))))
    (inner 0 #f nums allrows)))

(define (main) 
  (let* ((input (parse "../data/day04.in"))
         (nums (car input))
         (boards (cdr input))
         (calcs (sortBy (on > car)
                        (map (one-board nums) boards)))
         (part1 (apply * (cdr (car calcs))))
         (part2 (apply * (cdr (last calcs)))))
    (cons part1 part2)))

(main)

;; (38594 . 21184)
