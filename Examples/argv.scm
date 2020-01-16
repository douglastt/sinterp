;; $Id: argv.scm,v 1.4 2020-01-14 16:05:58-08 - - $

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (println arg)
    (printf "~a~n" arg))

(define *arg-list* (vector->list (current-command-line-arguments)))

(println *run-file*)
(for-each println *arg-list*)
