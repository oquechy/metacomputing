#lang racket

(require "mix-utils.rkt")
(require "flowchart-interpreter.rkt")
(require "flowchart-flowchart-interpreter.rkt")
(require "turing-machine-interpreter.rkt")
(require "tricky-mix.rkt")

(display "mix(find_name) ")

(define fn
  (fc-int tricky-mix `[,find_name
                       [[name namelist] [valuelist]]
                       [(name c) (namelist [a b c d])]]))
(assert fn
        `((read valuelist)
          (search0
           (:= valuelist (',cdr valuelist))
           (:= valuelist (',cdr valuelist))
           (return (',car valuelist))))
        "failed")

(display "mix(tm-int(tm-example)) ")

(define target1
  (fc-int tricky-mix `[,tm-int
                       [[Q Qtail op cmd] [Right]]
                       [(Q ,tm-example) (Qtail '()) (op '()) (cmd '())]]))

(assert (fc-int target1 '[[1 1 1 0 0 1]])
        '[1 0 1]
        "failed")


(display "mix(mix(tm-int)) ")

(define comp
  (fc-int tricky-mix `[,tricky-mix
                       [[program division tail bb command X
                                 expr ppt ppf pp-nxt pp-cur dyn-l live-v]
                        [vs0 vs pvs marked pending pp code
                             residual debug top label cnt idx val idx-cnt
                             pptvs ppfvs]]
                       [(program ,tm-int)
                        (division [[Q Qtail op cmd] [Right]])
                        (tail ())
                        (bb ())
                        (command ())
                        (X ())
                        (expr ())
                        (ppt ())
                        (ppf ())
                        (pp-nxt ())
                        (pp-cur ())
                        (dyn-l ())
                        (live-v ())
                        ]])
  )

(assert (fc-int (fc-int comp `[[(Q ,tm-example)
                                (Qtail ())
                                (op ())
                                (cmd ())]])
                '[[1 1 1 0 0 1]])
        '[1 0 1]
        "failed")

(display "mix(fc-fc-int(find_name)) ")

(define fc-target1
  (fc-int tricky-mix `[,fc-fc-int
                        [[program prog label block line
                                  tail op expr name]
                         [data ctx value]]
                        [(program ,find_name)
                         (prog '())
                         (label '())
                         (block '())
                         (line '())
                         (tail '())
                         (op '())
                         (expr '())
                         (name '())
                         ]]))

(assert (fc-int fc-target1 '[[c [a b c d] [1 2 3 4]]])
        3
        "failed")

(display "mix(mix(fc-fc-int)) ")

(define fc-comp
  (fc-int tricky-mix `[,tricky-mix
             [[program division tail bb command X
                       expr ppt ppf pp-nxt pp-cur dyn-l live-v]
              [vs0 vs pvs marked pending pp code
                   residual debug top label cnt idx val idx-cnt
                   pptvs ppfvs]]
             [(program ,fc-fc-int)
              (division [[program prog label block line
                                  tail op expr name]
                         [data ctx value]])
              (tail ())
              (bb ())
              (command ())
              (X ())
              (expr ())
              (ppt ())
              (ppf ())
              (pp-nxt ())
              (pp-cur ())
              (dyn-l ())
              (live-v ())
              ]])
)

(assert (fc-int (fc-int fc-comp `[[(program ,find_name)
                           (prog '())
                           (label '())
                           (block '())
                           (line '())
                           (tail '())
                           (op '())
                           (expr '())
                           (name '())]])
                '[[c [a b c d] [1 2 3 4]]])
        3
        "failed")

(define cogen
  (fc-int tricky-mix `[,tricky-mix
                        [[program division tail bb command X
                                  expr ppt ppf pp-nxt pp-cur dyn-l live-v]
                         [vs0 vs pvs marked pending pp code
                              residual debug top label cnt idx val idx-cnt
                              pptvs ppfvs]]
                        [(program ,tricky-mix)
                         (division [[program division tail bb command X
                                             expr ppt ppf pp-nxt pp-cur
                                             dyn-l live-v]
                                    [vs0 vs pvs marked pending pp code
                                         residual debug top label cnt
                                         idx val idx-cnt
                                         pptvs ppfvs]])
                         (tail ())
                         (bb ())
                         (command ())
                         (X ())
                         (expr ())
                         (ppt ())
                         (ppf ())
                         (pp-nxt ())
                         (pp-cur ())
                         (dyn-l ())
                         (live-v ())
                         ]])
  )

(display "mix(mix(mix)) (tm-int) ")

(define comp-gen
  (fc-int cogen `[[(program ,tm-int)
                   (division [[Q Qtail op cmd] [Right]])
                   (tail ())
                   (bb ())
                   (command ())
                   (X ())
                   (expr ())
                   (ppt ())
                   (ppf ())
                   (pp-nxt ())
                   (pp-cur ())
                   (dyn-l ())
                   (live-v ())]])
  )

(assert (fc-int cogen `[[(program ,tm-int)
                         (division [[Q Qtail op cmd] [Right]])
                         (tail ())
                         (bb ())
                         (command ())
                         (X ())
                         (expr ())
                         (ppt ())
                         (ppf ())
                         (pp-nxt ())
                         (pp-cur ())
                         (dyn-l ())
                         (live-v ())]])
        comp
        "failed")

(display "mix(mix(mix)) (fc-fc-int) ")


(assert (fc-int cogen `[[(program ,fc-fc-int)
                   (division [[program prog label block line
                                       tail op expr name]
                              [data ctx value]])
                   (tail ())
                   (bb ())
                   (command ())
                   (X ())
                   (expr ())
                   (ppt ())
                   (ppf ())
                   (pp-nxt ())
                   (pp-cur ())
                   (dyn-l ())
                   (live-v ())]])
        fc-comp
        "failed")

