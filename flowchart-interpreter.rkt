#lang racket

(provide int find_name eval-ctx)

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

(define (eval-ctx ctx expr)
  ; (println ctx)
  ; (println expr)
  (let ([ctx
         (map (λ (p) (list (car p) `',(cadr p))) ctx)])
    (eval `(let ,ctx ,expr) ns))
  )

(define (int-instr ctx line tail)
  (match line
    [(list ':= name expr)
     (let* ([val  (eval-ctx ctx expr)]
            [ctx  (filter-not (λ (el) (equal? (car el) name)) ctx)]
            [ctx  (cons (list name val) ctx)])
       (int-instr ctx (car tail) (cdr tail)))]
    [(list 'goto label)
     (list 'tolabel label ctx)]
    [(list 'if cond tru fls)
     (list 'tolabel (if (eval-ctx ctx cond) tru fls) ctx)]
    [(list 'return expr)
     (list 'ret (eval-ctx ctx expr))])
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
  
(define (int program data)
  ;(println (cdar program))
  ;(println data)
  (let* ([ctx (map list (cdar program) data)]
         [prog (cdr program)])
    (int-block ctx prog)
    )
  )