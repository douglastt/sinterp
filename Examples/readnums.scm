
;; $Id: readnums.scm,v 1.3 2018-11-14 14:42:46-08 - - $

;;
;; Read numbers from stdin, stopping at end of file.
;;

{define (readnumber)
        (let ((object (read)))
             (cond [(eof-object? object) object]
                   [(number? object) (+ object 0.0)]
                   [else (begin (printf "invalid number: ~a~n" object)
                                (readnumber))] )) }

{define (testinput)
        (let ((number (readnumber)))
             (if (eof-object? number)
                 (printf "*EOF* ~a~n" number)
                 (begin (printf "number = ~a~n" number)
                        (testinput)))) }

(testinput)

