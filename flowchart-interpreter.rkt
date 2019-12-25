#lang racket

(provide fc-int find_name)

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define find_name
  '((read name namelist valuelist)
    (search (if (equal? name (car namelist)) found cont))
    (cont (:= valuelist (cdr valuelist))
          (:= namelist (cdr namelist))
          (goto search))
    (found (return (car valuelist)))
    ))

(define (int-instr ctx line tail)
  (match line
    [(list ':= name expr)
     (namespace-set-variable-value! name (eval expr ctx) #f ctx)
     (int-instr ctx (car tail) (cdr tail))]
    [(list 'goto label)
     (list 'tolabel label ctx)]
    [(list 'if cond tru fls)
     (list 'tolabel (if (eval cond ctx) tru fls) ctx)]
    [(list 'return expr)
     (list 'ret (eval expr ctx))])
)

(define (int-block ctx program [label #f])
  (if (not label)
      (int-block ctx program (caar program))
      (let* ([block (findf (λ (b) (equal? (car b) label)) program)]
            [rec (int-instr ctx (cadr block) (cddr block))])
        (match rec
          [(list 'tolabel label ctx) (int-block ctx program label)]
          [(list 'ret ret) ret])
        )
      )
  )
  
(define (fc-int program data)
  (define ns (make-base-namespace))
  (map (λ (p) (namespace-set-variable-value! (car p) (cadr p) #f ns))
       (map list (cdar program) data))
  (int-block ns (cdr program))
  )

(display "fc-int(find_name) ")
(equal?
 (fc-int find_name '[c [a b c d] [1 2 3 4]]) 3)