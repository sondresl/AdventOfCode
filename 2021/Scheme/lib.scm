
(load "utils.scm")

;; File input
(define (read-input filename)
  (define (inner port)
    (let ((val (read port)))
      (if (eof-object? val)
        '()
        (cons val (inner port)))))
  (let ((file (open-input-file filename)))
    (inner file)))

(define (read-lines filename)
  (let ((file (open-input-file filename)))
    file))

;; debug

(define (print x)
  (display x)
  (newline))
