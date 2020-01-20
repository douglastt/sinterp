;#!racket
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
	(sin  ,sin)
	(cos  ,cos)
	(tan  ,tan)
	(acos ,acos)
	(asin ,asin)
        (atan ,atan)
        (exp  ,exp)
        (log  ,log)
	(abs  ,abs)
	(ceiling ,ceiling)
	(floor ,floor)
	(round ,round)
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
   (lambda (key value) (printf "~v: ~v\n" key value) #t))
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
  (map (lambda (line)
	 (cond ((empty? (cdr line)) (cdr line))
	       ((empty? (cddr line))
		(if (symbol? (cadr line))  ; goto without folloing statement in that line
		    `(,(cadr line) ())
		    (cadr line)))
	       (else `(,(cadr line) ,(caddr line)))))
       program))

(define (interpret-expression expr)
  (cond ((number? expr) (+ expr 0.0))
	((boolean? expr) expr)
	((string? expr) expr)
	((symbol? expr) (hash-ref *variable-table* expr))
	((pair? expr)
	 (cond ((eqv? (car expr) 'asub)
		(let ((aray (hash-ref *array-table* (cadr expr))))
		  (vector-ref aray (inexact->exact (interpret-expression (caddr expr))))))
	       (else
		(let ((operator (hash-ref *function-table* (car expr)))
		      (operands (map interpret-expression (cdr expr))))
		  (if (null? operator)
		      NAN
		      (apply operator operands))))))
	(else (raise (format "~a is not a valid expr" expr)))))

(define (print-exprs exprs)
  (if (empty? exprs)
      (printf "\n")
      (begin
	(printf "~a" (car exprs))
	(print-exprs (cdr exprs)))))

(define (update-variable var val)
  (cond ((symbol? var) (hash-set! *variable-table* var val))
        ((pair? var) 
	 (if (eqv? (car var) 'asub)
	     (let ((aray (hash-ref *array-table* (cadr var)))
		   (indx (interpret-expression (caddr var))))
	       (vector-set! aray (inexact->exact indx) val))
	     (raise (format "does not know what is ~v" (car var)))))
	(else (raise (format "~v is not valid memory ref" var)))
	))

(define (make-new-array mem-ref)
  (let ((name (cadr mem-ref))
	(indx (interpret-expression (caddr mem-ref))))
    (begin
      (let ((aray (make-vector (inexact->exact indx) 0.0)))
	(hash-set! *array-table* name aray)))))

(define (read-vars vars)
  (cond ((empty? vars) (void))
	(else
	 (let ((head (car vars))
	       (tail (cdr vars)))
	   (begin
	     (let ((val (+ (read) 0.0)))
	       (hash-set! *variable-table* head val))
					;     (printf "adding ~v:~v to variable-table\n" head val))
	     (read-vars tail))))))
	       
(define (interpret-statement stmt)
  (cond ((null? stmt) '())
	(else
	 (let ((header (car stmt)))
	  (cond ((eqv? header 'goto) (cdr stmt))
		((eqv? header 'print)
		 (begin (print-exprs (map interpret-expression (cdr stmt)))
			'()))
		((eqv? header 'let)
		 (begin
		   (let ((var-bind (cadr stmt))
			 (var-val (interpret-expression (caddr stmt))))
		     (begin
		       (update-variable var-bind var-val)
		       '()))))
		((eqv? header 'dim)
		 (begin
		   (make-new-array (cadr stmt))
		   '()))
		((eqv? header 'if)
		 (let ((condition (interpret-expression (cadr stmt)))
		       (label (cddr stmt)))
		   (begin
		     (if condition label '()))))
		((eqv? header 'input)
		 (begin
		   (read-vars (cdr stmt))
		   '()))
		((symbol? header)
		 (interpret-statement (cadr stmt)))
		(else (raise (format "unkown statement ~v" stmt))))
	  ))))

(define (interpret-program program)
;  (printf "interpret-program at ~a...\n" program)
  (if (empty? program)
      (void)	; (dump-tables)
      (let ((stmt (car program)) 
	     (cont (cdr program)))
	(begin
	  (let ((label-opt (interpret-statement stmt)))
	    (begin
	      (if (null? label-opt)
		  (interpret-program cont)
		  (let ((program-counter (hash-ref *label-table* (car label-opt) NAN)))
		    (begin
		      (interpret-program program-counter))))))))))

(define (update-label-table program)
  (cond ((empty? program) (void))
	(else
	 (let ((header (car program))
	       (tail (cdr program)))
	   (begin
	     (cond ((or
		     (empty? header)
		     (eqv? (car header) 'print)
		     (eqv? (car header) 'let)
		     (eqv? (car header) 'goto)
		     (eqv? (car header) 'asub)
		     (eqv? (car header) 'if)
		     (eqv? (car header) 'input)
		     (eqv? (car header) 'dim)) (void))
		   (else
		    (let ((label (car header))
			  (stmt (if (empty? header) '() (cadr header))))
		      (begin
			(hash-set! *label-table* label (cons stmt tail))))))
	     (update-label-table tail))))))
 
;; interp end here

(define (write-program-by-line filename program)
;    (printf "==================================================~n")
;    (printf "~a: ~a~n" *run-file* filename)
;    (printf "==================================================~n")
;    (printf "(~n")
;    (for-each (lambda (line) (printf "~a~n" line)) program)
;    (interpret-program (remove-lino program))
;   (printf ")~n")
    ; (dump-tables)
    (let ((program-nolino (remove-lino program)))
;      (for-each (lambda (line) (printf "~v~n" line)) program-nolino)
      (begin
	(update-label-table program-nolino)
	(with-handlers ((exn:fail? (lambda (e) (printf "exception occurs: ~s\n" e))))
		       (interpret-program program-nolino))))
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

