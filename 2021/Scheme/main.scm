
(load "utils.scm")

(load "day01.scm")
(load "day02.scm")
(load "day02.scm")

(define (run-main str)
  (load str)
  (print (list str (main))))

(for-each run-main
          '("day01.scm"
            "day02.scm"
            "day03.scm"))
