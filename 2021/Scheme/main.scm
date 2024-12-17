
(load "utils.scm")

(define (run-main str)
  (load str)
  (print (list str (main))))

(for-each run-main
          '("day01.scm"
            "day02.scm"
            "day03.scm"
            "day04.scm"
            "day05.scm"
            "day06.scm"
            "day07.scm"
            "day08.scm"
            "day09.scm"
            "day10.scm"
            "day11.scm"))
