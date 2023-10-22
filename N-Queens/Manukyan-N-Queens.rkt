#lang racket
;--------------------------------------
;Tigran Manukyan
;Class #16842       11am - 12:15pm 
;N-Queens
;--------------------------------------


;--------------------------------------
; Definition of nc
;   paramters: v is current row
;              p is all previous rows for current solution
;   value: calls nc-h to check if theres row or
;               diagonal conflic between v and all members of p
;   helper functions: nc-h
;--------------------------------------
(define (nc v p)
  (nc-h v p 1)
  )


;--------------------------------------
; Definition of nc-h
;   paramters: v is current row
;              p is previous rows for current solution
;              d is distance between v and car of p
;   value: check if theres row or
;              diagonal conflic between v and all members of p
;   helper functions: nc-1
;--------------------------------------
(define (nc-h v p d)
  (cond
    ((empty? p) #t)
    (else (and
           (nc-1 v (car p) d)
           (nc-h v (cdr p) (+ d 1))
           )
          )
    )
  )


;--------------------------------------
; Definition of nc-1
;   paramters: a is first row value
;              b is second row value
;              d is distance between a and b
;   value: check if theres row or
;              diagonal conflic between v and all members of p
;--------------------------------------
(define (nc-1 a b d)
  (and
   (not (= a b))
   (not (= d (abs (- a b))))
   )
  )




;--------------------------------------
; Definition of solve
;   paramters: n is number of queens
;   value: returns all solutions of N-Queens problem
;   helper functions: solve-h
;--------------------------------------
(define (solve n) (solve-h n 1 '(0) '(0 1) '()))


;--------------------------------------
; Definition of solve-h
;   paramters: n is number of queens
;              c is current collom
;              qp is current sollution up to curront row
;              nr is next row to check for each column
;              sv is list of solutions
;   value: finds all solutions of N-Queens problem,
;                calls itself reqursivally
;                storing solutions in sv,
;                returns sv when no more solutions can be found
;--------------------------------------
(define (solve-h n c qp nr sv)
  (cond
    ((and (= c 0) (= n (car nr))) sv)
    ((= c n) (solve-h
              n
              (- c 1)
              (cdr qp)
              (cdr nr)
              (cons qp sv)
              )
             )
    ((= n (car nr)) (solve-h
                     n
                     (- c 1)
                     (cdr qp)
                     (cdr nr)
                     sv
                     )
                    )
    ((nc (car nr) qp) (solve-h
                       n
                       (+ c 1)
                       (cons (car nr) qp)
                       (cons 0 (cons (+ 1 (car nr)) (cdr nr)))
                       sv
                       )
                      )
    (else (solve-h
           n
           c
           qp
           (cons (+ 1 (car nr)) (cdr nr))
           sv
           )
          )
    )
  )