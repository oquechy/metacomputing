#lang racket

(require "flowchart-interpreter.rkt")

(provide mix-read
         clear-ctx
         live-vars
         reduce
         project
         is-static
         lookup
         gen-label
         dyn-labels
         idx-cnt-update
         unmarked
         eval-ctx
         assert)

(define (eval-ctx ctx expr)
  (define ns (make-base-namespace))
  (map (λ (p) (namespace-set-variable-value! (car p) (cadr p) #f ns)) ctx)
  (eval expr ns))

(define (unmarked live-v marked lst)
  (filter (λ (p) (not (member (list (car p)
                            (project (car p)
                                     (cadr p)
                                     live-v))
                      marked))) lst))

(define (st-vars expr division)
  (match expr
    [(list tail ...)
     (flatten (map (λ (e) (st-vars e division)) tail))]
    [sym
     (if (member sym (car division)) (list sym) '())]))

(define (assert actual expected [err "assertion failed"])
  (if (equal? actual expected)
      #t
      (error (~a err ": " actual " != " expected))))

(display "st-vars ")
(assert (st-vars '(:= x (+ 2 3 foo bar (tan cotan)))
                 '[[foo tan] [cotan bar]])
        '(foo tan)
        "st-vars failed")

(define (lookup pp program)
  (cdr (findf (λ (b) (equal? (car b) pp)) program)))

(define (external-vars block division)
  (let [(vars
         (foldl (λ (instr vars)
                  (let* [(external (car vars))
                         (assigned (cadr vars))
                         (parse (match instr
                                  [(list ':= X expr)
                                   (list expr (list X))]
                                  [(list 'if expr _ _)
                                   (list expr '())]
                                  [(list 'return expr)
                                   (list expr '())]
                                  [_ '(() ())]))
                         (expr (car parse))
                         (asn (cadr parse))
                         (stat (st-vars expr division))]
                    (list (append external
                                  (remove* assigned stat))
                          (append assigned asn))
                    ))
                '(() ())
                block))]
    (list (car vars) (remove* (car vars) (cadr vars))))
  )

(display "external-vars ")
(assert (external-vars '(label (:= T (+ X 1))
                               (:= Y (+ Y T))
                               (:= Q (+ 2 Z))
                               (:= Z (+ Q (1 S)))
                               (if (U) t f))
                       '[[X Y Q R S U] [Z T]])
        '((X Y S U) (T Q Z)))

(define (fuse b live-vars program)
  (let* [(jmp
         (match (last (lookup (car b) program))
           [(list 'if _ t f) (list t f)]
           [(list 'goto l)   (list l)]
           [(list 'return _) (list)]))
         (new-live
          (flatten (map (λ (j) (car (lookup j live-vars))) jmp)))]
    (remove* (caddr b) (remove-duplicates (append (cadr b) new-live)))))

(display "fuse ")
(assert (fuse '(label (Y) (X))
              '((label (Y) (X)) (lt (T Y) (Z)) (lf (S X) (Q)))
              '((label (:= X Y) (if #t lt lf))))
        '(Y T S)
        "fuse failed")

(define (live-vars program division)
  (define (upd live-vars)
    (let [(upd-vars
           (for/list ([b live-vars])
             (list (car b) (fuse b live-vars program) (caddr b))))]
      (if (equal? upd-vars live-vars) upd-vars (upd upd-vars))))
  (let* [(vars
         (map (λ (b)
                (cons (car b)
                      (external-vars b division)))
              (cdr program)))
        (live-v (upd vars))]
    (map (λ (t) (list (car t) (sort (cadr t) symbol<?))) live-v)))

(display "live-vars ")
(assert (live-vars find_name '[[name namelist] [valuelist]])
        '((search (name namelist))
          (cont (name namelist))
          (found ()))
        "live-vars failed")

(define (dyn-labels program division)
  (remove-duplicates
   (cons (caadr program)
        (flatten (filter-map (λ (b)
                      (match (last b)
                        [(list 'if expr t f)
                         (and (not (is-static expr division)) (list t f))]
                        [_ #f]))
                    program)))))

(define (mix-read read division)
  (remove* (car division) read))

(define (reduce expr vs)
  (define (reduce-in-ctx expr)
    (reduce expr vs))
  (with-handlers
      ([exn:fail? (λ (_) (if (list? expr) (map reduce-in-ctx expr) expr))])
    `',(eval-ctx vs expr)
    ))

(define (is-static expr division)
  (define (is-static-dep expr num) 
    (match expr
      [(list 'quote _ ...)
       #t]
      [(list 'quasiquote tail ...)
       (andmap (λ (expr) (is-static-dep expr (+ num 1))) tail)]
      [(list 'unquote tail ...)
       (andmap (λ (expr) (is-static-dep expr (- num 1))) tail)]
      [(list lst ...)
       (andmap (λ (expr) (is-static-dep expr num)) lst)]
      [sym
       (or (> num 0) (not (member sym (cadr division))))]
      ))
  (is-static-dep expr 0)
  )

(define (project label vs live-v)
  (filter (λ (p) (member (car p) (car (lookup label live-v)))) vs))

(display "project ")
(assert (project 'l
                 '((x 1) (y 2) (z 3))
                 '((l (x z))))
        '((x 1) (z 3))
        "project failed")

(define (idx-cnt-update label vs idx cnt live-v)
  (let [(proj (project label vs live-v))]
    (if (hash-has-key? idx proj)
        (list idx cnt)
        (list (hash-set idx proj cnt) (+ cnt 1)))))

(display "idx-cnt-update ")
(assert (idx-cnt-update 'l
                        '((x 1) (y 2) (z 3))
                        (hash)
                        0
                        '((l (x z))))
        (list (hash '((x 1) (z 3)) 0) 1)
        "idx-cnt-update failed")


(define (gen-label label vs idx live-v)
  (string->symbol (~a label (hash-ref idx (project label vs live-v)))))

(define (clear-ctx ctx X)
  (filter (λ (el) (not (equal? (car el) X))) ctx))