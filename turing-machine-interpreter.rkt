#lang racket

(provide tm-int tm-example)

(require "flowchart-interpreter.rkt")

(define tm-example '((0 if 0 goto 3) (1 right) (2 goto 0) (3 write 1)))

(define tm-int
  '((read Q Right)
    (init (:= Qtail Q) (:= Left (list)) (goto loop))
    (loop (if (equal? Qtail (list)) end exec))
    (exec (:= cmd (car Qtail))
          (:= Qtail (cdr Qtail))
          (:= op (cadr cmd))
          (goto match1))
    (match1 (if (equal? op 'right) exec-right match2))
    (match2 (if (equal? op 'left) exec-left match3))
    (match3 (if (equal? op 'write) exec-write match4))
    (match4 (if (equal? op 'goto) exec-goto match5))
    (match5 (if (equal? op 'if) exec-if err))

    (exec-right (:= Left (cons (car Right) Left))
                (:= Right (cdr Right))
                (goto loop))
    (exec-left (:= Right (cons (car Left) Right))
               (:= Left (cdr Left))
               (goto loop))
    (exec-write (:= Right (cons (caddr cmd) (cdr Right)))
                (goto loop))
    (exec-goto (:= Qtail (memf (λ (bmd) (equal? (car bmd) (caddr cmd))) Q))
                (goto loop))
    (exec-if (if (equal? (caddr cmd) (car Right)) jmp loop))
    (jmp (:= Qtail (memf (λ (bmd) (equal? (cadddr (cdr cmd)) (car bmd))) Q))
         (goto loop))
    
    (err (return `unexpected ,cmd))
    (end (return Right))
    )
  )
