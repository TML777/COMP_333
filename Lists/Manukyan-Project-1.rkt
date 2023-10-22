#lang racket

;--------------------------------------
;Tigran Manukyan
;Class #16842       11am - 12:15pm 
;Project 1
;--------------------------------------



;--------------------------------------
; Definition of rotate-left-1
;   paramters: x
;              x is list
;   value: rotates the elements of x
;          one to the left
;--------------------------------------
(define (rotate-left-1 x)
  (cond
    ((empty? x) '())
    (else (append (cdr x) (list (car x))))
    )
  )
    

;--------------------------------------
; Definition of rotate-left-n
;   paramters: x n
;              x is list
;              n integer >= 0
;   value: rotates the elements of x
;          n times to the left
;--------------------------------------
(define (rotate-left-n x n)
  (cond
    ((= n 0) x)
    (else (rotate-left-n (rotate-left-1 x) (- n 1)))
    )
  )


;--------------------------------------
; Definition of count-items
;   paramters: x
;              x is list
;   value: returns number of 
;          elements in list x
;--------------------------------------
(define (count-items x)
  (cond
    ((empty? x) 0)
    (else (+ 1 (count-items (cdr x))))
    )
  )


;--------------------------------------
; Definition of list-item-n
;   paramters: x n
;              x is list
;              n is int between 0 and x's length
;   value: returns the nth element 
;          in the list x
;--------------------------------------
(define (list-item-n x n)
  (cond
    ((= n 0) (car x))
    (else (list-item-n (cdr x) (- n 1)))
    )
  )


;--------------------------------------
; Definition of list-minus-item-n
;   paramters: x n
;              x is list
;              n is int between 0 and x's length
;   value: takes out the nth element 
;          from list x
;--------------------------------------
(define (list-minus-item-n x n)
  (cond
    ((= n 0) (cdr x))
    (else (append (list (car x)) (list-minus-item-n (cdr x) (- n 1))))
    )
  )


;--------------------------------------
; Definition of rotate-right-1
;   paramters: x
;              x is list
;   value: rotates the elements of x
;          one to the right
;--------------------------------------
(define (rotate-right-1 x)
  (cond
    ((empty? x) x)
    ((empty? (cdr x)) x)
    (else
     (define tempList (rotate-right-1(cdr x)))
     (append (list (car tempList)) (append (list (car x)) (cdr tempList)))
     )
    )
  )


;--------------------------------------
; Definition of reverse-list x
;   paramters: x
;              x is list
;   value: reverses the 
;          elements of list x
;--------------------------------------
(define (reverse-list x)
  (cond
    ((empty? x) x)
    (else (append (reverse-list (cdr x)) (list (car x))))
    )
  )


;--------------------------------------
; Definition of cons-to-all a x
;   paramters: a x
;              a an element
;              x is list of lists
;   value: uses map to cons a to all
;          elements in list x
;          (x is a list of lists)
;--------------------------------------
(define (cons-to-all a x)
  (map (lambda (y) (cons a y)) x)
  )


;--------------------------------------
; Definition of permute x
;   paramters: x
;              x is list
;   value: generates all 
;          perutations of list x
;          as a list of lists
;--------------------------------------
(define (permute x)
  (cond
   ((empty? x) x)
   ((empty? (cdr x)) (list x))
   ((empty? (cddr x)) (list x (reverse-list x)))
   (else (ph-2 x (- (count-items x) 1)))
   )
  )


;--------------------------------------
; Definition of ph-1 x n
;   paramters: x n
;             x is list
;             n is int between 0 and x's length
;   value: helper function for permute,
;          calls cons-to-all for nth element and 
;          permutation of x minus nth element
;--------------------------------------
(define (ph-1 x n)
  (cons-to-all
   (list-item-n x n)
   (permute (list-minus-item-n x n))
   )
  )


;--------------------------------------
; Definition of ph-2 x n
;   paramters: x n
;             x is list
;             n is int between 0 and x's length
;   value: helper function for permute,
;          calls ph-1 for elements of x from 
;          0 to n and appends the lists together 
;--------------------------------------
(define (ph-2 x n)
  (cond
    ((= n 0) (ph-1 x n))
    (else (append (ph-1 x n) (ph-2 x (- n 1))))
    )
  )



;test cases
#|
(rotate-left-1 '())                             ;'()
(rotate-left-1 '(a))                            ;'(a)
(rotate-left-1 '(a b c))                        ;'(b c a)
(rotate-left-n '(a b c) 0)                      ;'(a b c)
(rotate-left-n '(a b c d e) 2)                  ;'(c d e a b)
(rotate-left-n '(a b c d e) 5)                  ;'(a b c d e)
(count-items '())                               ;0
(count-items '(a))                              ;1
(count-items '(a b c d e))                      ;5
(list-item-n '(a b c d e) 0)                    ;'a
(list-item-n '(a b c d e) 4)                    ;'e
(list-item-n '(a b c d e) 1)                    ;'b
(list-minus-item-n '(a b c d e) 0)              ;'(b c d e)
(list-minus-item-n '(a b c d e) 1)              ;'(a c d e)
(list-minus-item-n '(a b c d e) 2)              ;'(a b d e)
(list-minus-item-n '(a b c d e) 4)              ;'(a b c d)
(rotate-right-1 '(a b c d e))                   ;'(e a b c d)
(rotate-right-1 '(a))                           ;'(a)
(rotate-right-1 '(a b))                         ;'(b a)
(rotate-right-1 '(a b c d e f g))               ;'(g a b c d e f)
(reverse-list '(a))                             ;'(a)
(reverse-list '(a b))                           ;'(b a)
(reverse-list '(a b c d e))                     ;'(e d c b a)
(cons-to-all 'a '((b c) (d e) (f g)))           ;'((a b c) (a d e) (a f g))

(permute '(a b))                                ;'((a b) (b a))
(permute '(a b c))                              ;'((c a b) (c b a) (b a c) (b c a) (a b c) (a c b))
(permute '(a b c d))

;'((d c a b) (d c b a) (d b a c) (d b c a) (d a b c)
; (d a c b) (c d a b) (c d b a) (c b a d) (c b d a)
; (c a b d) (c a d b) (b d a c) (b d c a) (b c a d)
; (b c d a) (b a c d) (b a d c) (a d b c) (a d c b)
; (a c b d) (a c d b) (a b c d) (a b d c))
|#