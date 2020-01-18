#!racket
;; $Id: sbi.scm,v 1.19 2020-01-14 16:58:47-08 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))
(define *arg-list* (vector->list (current-command-line-arguments)))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (dump-stdin)
    (let ((token (read)))
         (printf "token=~a~n" token)
         (when (not (eq? token eof)) (dump-stdin))))


;; interp from here

(define NAN (/ 0.0 0.0))

(define *function-table* (make-hash))
(for-each
    (lambda (symfun) (hash-set! *function-table* (car symfun) (cadr symfun)))
    `(
        (+    ,+)
        (-    ,-)
        (*    ,*)
        (/    ,/)
        (^    ,expt)
        (sqrt ,sqrt)
	(=    ,=)
	(!=   ,(lambda (a1 a2) (not (= a1 a2))))
	(<    ,<)
	(>    ,>)
	(<=   ,<=)
	(>=   ,>=)
    ))

(define *variable-table* (make-hash))
(for-each
    (lambda (varval)
        (hash-set! *variable-table* (car varval) (cadr varval)))
    `(
        (pi    ,(acos -1))
        (e     ,(exp 1))
        (i     ,(sqrt -1))
        (one   1)
        (zero  0)
	(eof   0.0)
	(nan   ,(/ 0.0 0.0))
    ))

(define *array-table* (make-hash))
(define *label-table* (make-hash))

(define (dump-tables)
  (printf "*function-table*:\n")
  (hash-for-each
   *function-table*
   (lambda (key value) (printf "~s: ~s\n" key value) #t))
  (printf "*variable-table*:\n")
  (hash-for-each
   *variable-table*
   (lambda (key value) (printf "~s: ~s\n" key value) #t))
  (printf "*array-table*:\n")
  (hash-for-each
   *array-table*
   (lambda (key value) (printf "~s: ~s\n" key value) #t))
  (printf "*label-table*:\n")
  (hash-for-each
   *label-table*
   (lambda (key value) (printf "~s: ~s\n" key value) #t))
  (printf "dump tables finished\n"))

(define (remove-lino program)
;  (let (line (car program))
 ;   (if (empty? line)
	
  (map (lambda (line)
	 (if (empty? (cdr line))
	     (cdr line)
	     (cadr line)))
       program))
;  (printf "removing lino: ~v\n" program)
;  (if (empty? program)
;      '()
;      `(,(cdr (car program)) ,(remove-lino (cdr program)))
;      ))

;(define (eval-expr expr)
;    (cond ((number? expr) (+ expr 0.0))
;          ((symbol? expr) (hash-ref *variables* expr NAN))
;          ((pair? expr) 
;              (let ((func (hash-ref *functions* (car expr) NAN))
;                    (opnds (map eval-expr (cdr expr))))
;                   (if (null? func) NAN
;                       (apply func (map eval-expr opnds)))))
;           (else NAN)))


(define (interpret-expression expr)
  (cond ((number? expr) (+ expr 0.0))
	((boolean? expr) expr)
	((string? expr) expr)
	((symbol? expr) (hash-ref *variable-table* expr NAN))
	((pair? expr)
	 (let ((operator (hash-ref *function-table* (car expr) NAN))
	       (operands (map interpret-expression (cdr expr))))
	   (if (null? operator)
	       NAN
	       (apply operator operands))))
	(else NAN)))

(define (print-exprs exprs)
  (if (empty? exprs)
      (printf "\n")
      (begin
	(printf "~a" (car exprs))
	(print-exprs (cdr exprs)))))

(define (interpret-statement stmt)
  (cond ((null? stmt) '())
	(else
	 (let ((header (car stmt)))
	  (cond ((eqv? header 'goto) (cdr stmt))
		((eqv? header 'print) (print-exprs (map interpret-expression (cdr stmt))))
		(else (raise  header)))
	))))

(define (interpret-program program)
;  (printf "interpret-program at ~a...\n" (car program))
  (if (empty? program)
      (void)	; (dump-tables)
      (let ((stmt (car program)) 
	     (cont (cdr program)))
	(begin
	  ;(if (is-label-stmt stmt)
	  ;    (put-label stmt)
	  ;    (empty-statement))
	  (interpret-statement stmt)
	;  (printf "interpret a statement success\n")
	  (interpret-program cont)))))


;; interp end here

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~a~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (for-each (lambda (line) (printf "~a~n" line)) program)
;    (interpret-program (remove-lino program))
    (printf ")~n")
    (let ((program-nolino (remove-lino program)))
      (for-each (lambda (line) (printf "~a~n" line)) program-nolino)
      (interpret-program program-nolino))
    )

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (write-program-by-line sbprogfile program))))

(if (terminal-port? *stdin*)
    (main *arg-list*)
    (printf "sbi.scm: interactive mode~n"))

