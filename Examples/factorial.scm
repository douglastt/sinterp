;; non tail-recursive
(define (fac1 n)
   (if (<= n 0) 1
       (* n (fac1 (- n 1)))))

;; tail recursive
(define (fac2 n)
   (define (f n acc)
           (if (<= n 0) acc
               (f (- n 1) (* n acc))))
   (f n 1))

;; $Id: factorial.scm,v 1.4 2016-09-06 15:07:38-07 - - $
