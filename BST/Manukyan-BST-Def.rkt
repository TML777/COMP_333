#lang racket
;--------------------------------------
;Tigran Manukyan
;Class #16842       11am - 12:15pm 
;BST Definitions
;--------------------------------------


(provide bst)
(provide node)
(provide node-value)
(provide traverse)
(provide nl-to-vl)
(provide find)
(provide insert)
(provide insert-from-list)
(provide random-list)
(provide delete)
(provide get-iop-path)
(provide nl-to-vcl)
(provide nl-to-all)
(provide delete-from-list)


;--------------------------------------
; node: struct to represent node in BST
; value: int to represent value of node;
; count: int to represent count of nodes with value
; left: ref to left node
; right: ref to right node
; mutable and transparent
;--------------------------------------
(struct node (value count left right) #:mutable #:transparent)

;--------------------------------------
; node: struct to represent node in BST
; root: ref to node at head or null if empty
; mutable and transparent
;--------------------------------------
(struct bst (root) #:mutable #:transparent)


;--------------------------------------
; Definition of traverse
;   paramters: t tree root
;   value: returns traversed list of
;              nodes in order
;   helper functions: traverse-node n
;--------------------------------------
(define (traverse t)
  (cond
    ((empty? (bst-root t)) null)
    (else (traverse-node(bst-root t)))
    )
  )

;--------------------------------------
; Definition of traverse-node
;   paramters: n is node in tree
;   value: helper function of traverse t
;              returns traversed list of
;              nodes in order
;--------------------------------------
(define (traverse-node n)
  (cond
    ((empty? n) null)
    (else (append (traverse-node (node-left n))
                  (list n)
                  (traverse-node (node-right n)))
          )
    )
  )


;--------------------------------------
; Definition of nl-to-vl
;   paramters: x list of nodes
;   value: returns list of values from
;              list of nodes 
;--------------------------------------
(define (nl-to-vl x)
  (cond
    ((empty? x) null)
    (else (cons (node-value (car x)) (nl-to-vl (cdr x))))
    )
  )


;--------------------------------------
; Definition of find
;   paramters: t tree root
;              v value to find
;   value: returns a list of nodes with
;            with value found(or place of insert value)
;            at the head of list, and all ancestors nodes
;            following with root at the end
;   helper functions: find-node n v
;--------------------------------------
(define (find t v)
  (cond
    ((empty? (bst-root t)) null)
    (else (find-node (bst-root t) v))
    )
  )



;--------------------------------------
; Definition of find
;   paramters: n node in tree
;              v value to be found
;   value: helper funtion of find t v
;            returns a list of nodes with
;            with value found(or place of insert value)
;            at the head of list, and all ancestors nodes
;            following 
;--------------------------------------
(define (find-node n v)
  (cond
    ((empty? n) '())
    ((= v (node-value n)) (list n))
    ((< v (node-value n)) (append (find-node (node-left n) v) (list n)))
    (else (append (find-node (node-right n) v) (list n)))
    )
  )


;--------------------------------------
; Definition of insert
;   paramters: t tree root
;              v value to be inserted
;   value: inserts node into 
;              tree
;   helper functions: insert-node v path
;--------------------------------------
(define (insert t v)
  (cond
    ((empty? (bst-root t)) (set-bst-root! t (node v 1 null null)))
    (else (insert-node v (find t v)))
    )
  )


;--------------------------------------
; Definition of insert-node
;   paramters: v value to be inserted
;              path path to correct place to insert node
;   value: helper function for insert
;          inserts node into the correct 
;              position according to the head of path
;--------------------------------------
(define (insert-node v path)
  (cond
    ((< v (node-value (car path))) (set-node-left! (car path) (node v 1 null null)))
    ((> v (node-value (car path))) (set-node-right! (car path) (node v 1 null null)))
    (else (set-node-count! (car path) (+ 1 (node-count (car path)))))
    )
  )
  



;--------------------------------------
; Definition of insert-from-list
;   paramters: t tree root
;              y list of values
;   value: used for testing
;            inserts list of values y into
;            into tree t
;--------------------------------------
(define (insert-from-list t y)
  (map (lambda (x) (insert t x) (displayln (nl-to-vl (traverse t)))) y)
  )



;--------------------------------------
; Definition of random-value
;   paramters: r max random value
;   value: returns random value
;             from 0 to r
;--------------------------------------
(define (random-value r)
  (inexact->exact (floor (* r (random))))
  )

;--------------------------------------
; Definition of random-list
;   paramters: n number of values
;              r max random value
;   value: returns list of n random values
;             from 0 to r
;--------------------------------------
(define (random-list n r)
  (cond
    ((= n 0) '())
    (else (cons (random-value r) (random-list (- n 1) r)))
    )
  )



;--------------------------------------
; Definition of delete
;   paramters: t tree root
;              v value to be deleted
;   value: deletes v from tree t
;              if v is in tree
;           !!!!
;           if v is tree root, then
;           t might be modified
;           !!!!
;   helper functions: delete-node path
;                     delete-node-2 path
;                     child-count n
;--------------------------------------
(define (delete t v)
  (let ((path (find t v)))
    (cond
      ((empty? (bst-root t)) void)
      ((not (= (node-value (car path)) v)) void)
      ((> (node-count (car path)) 1) (set-node-count! (car path) (- (node-count (car path)) 1)))
      ((equal? (car path) (bst-root t))
       (let ((count (child-count (car path))))
         (cond
           ((= count 0) (set-bst-root! t null))
           ((= count 1)
            (cond
              ((equal? (node-right (car path)) null) (set-bst-root! t (node-left (car path))))
              (else (set-bst-root! t (node-right (car path))))
              )
            )
           (else (delete-node path))
           )
         )
       )
      (else (delete-node path))
      )
    )
  )


;--------------------------------------
; Definition of delete-node
;   paramters: path list of nodes
;                   with node to be deleted in fromt
;   value: helper function of delete
;          calls delete-node-0 if number of children = 0
;          calls delete-node-1 if number of children = 1
;          calls delete-node-2 if number of children = 2
;   helper functions: delete-node-0 path
;                     delete-node-1 path
;                     delete-node-2 path
;                     child-count n
;--------------------------------------
(define (delete-node path)
  (let ((count (child-count (car path))))
    (cond
      ((= count 0) (delete-node-0 path))
      ((= count 1) (delete-node-1 path))
      (else (delete-node-2 path))
      )
    )
  )

;--------------------------------------
; Definition of delete-node-0
;   paramters: path list of nodes
;                    with node to be deleted in fromt
;   value: helper function of delete-node
;          deletes node modying its parent
;--------------------------------------
(define (delete-node-0 path)
  (cond
    ((equal? (node-left (cadr path)) (car path)) (set-node-left! (cadr path) null))
    (else (set-node-right! (cadr path) null))
    )
  )


;--------------------------------------
; Definition of delete-node-1
;   paramters: path list of nodes
;                   with node to be deleted in fromt
;   value: helper function of delete-node
;          deletes node modying its parent
;--------------------------------------
(define (delete-node-1 path)
  (let ((n (car path))(p (cadr path)))
    (cond
      ((equal? (node-right p) null)
       (cond
         ((equal? (node-right n) null) (set-node-left! p (node-left n)))
         (else (set-node-left! p (node-right n)))
         )
       )
      ((equal? (node-left p) null)
       (cond
         ((equal? (node-right n) null) (set-node-right! p (node-left n)))
         (else (set-node-right! p (node-right n)))
         )
       )
      (else
       (cond
         ((equal? (node-value n) (node-value (node-left p)))
          (cond
            ((equal? (node-right n) null) (set-node-left! p (node-left n)))
            (else (set-node-left! p (node-right n)))
            )
          )
         (else
          (cond
            ((equal? (node-right n) null) (set-node-right! p (node-left n)))
            (else (set-node-right! p (node-right n)))
            )
          )
         )
       )
      )
    )
  )
            
        


;--------------------------------------
; Definition of delete-node-2
;   paramters: path list of nodes
;                   with node to be deleted in fromt
;   value: helper function of delete and delete-node
;          deletes node by switching it with head of
;             iopath then calling (delete-node iopath)
;             iopath = get-iop-path(car path)
;   helper functions: get-iop-path n
;                     delete-node path
;--------------------------------------
(define (delete-node-2 path)
  (let ((ioppath (get-iop-path (car path))))
    (set-node-value! (car path) (node-value (car ioppath)))
    (set-node-count! (car path) (node-count (car ioppath)))
    (set-node-count! (car ioppath) 1)
    (delete-node ioppath)
    )
  )
  
;--------------------------------------
; Definition of child-count
;   paramters: n is a node
;   value: helper function of delete and delete-node
;          returns 0 if n has 0 children
;          returns 1 if n has 1 children
;          returns 2 if n has 2 children
;--------------------------------------
(define (child-count n)
  (cond
    ((and (equal? (node-left n) null) (equal? (node-right n) null)) 0)
    ((and (not (equal? (node-left n) null)) (not (equal? (node-right n) null))) 2)
    (else 1)
    )
  )


;--------------------------------------
; Definition of get-iop-path
;   paramters: n is a node
;   value: helper function of delete-node-2
;          returns right most node of tree of
;               node-left of node
;   helper functions: get-iop-path-right path
;--------------------------------------
(define (get-iop-path n)
  (get-iop-path-right (list (node-left n) n))
  )

;--------------------------------------
; Definition of get-iop-path-right
;   paramters: path is list of nodes
;   value: helper function of get-iop-path
;          adds node-right of car path
;               at the head of path
;               then calls itself using path
;          base case is if node-right is null
;--------------------------------------
(define (get-iop-path-right path)
  (cond
    ((equal? (node-right (car path)) null) path)
    (else (get-iop-path-right (append (list (node-right (car path))) path)))
    )
  )



;----------------------------------------------------------------------------
;---------------------------TEST HELPER FUNCTIONS----------------------------
;----------------------------------------------------------------------------


;--------------------------------------
; Definition of get-child-node-values
;   paramters: n node
;   value: used for testing
;           returns list of values
;           of child nodes of n
;           if child is null "X" symbol is used
;--------------------------------------
(define (get-child-node-values n)
  (cond
    ((and(empty? (node-left n))(empty? (node-right n))) (list 'X 'X))
    ((and(empty? (node-left n))(not(empty? (node-right n)))) (list 'X (node-value (node-right n))))
    ((and(not(empty? (node-left n)))(empty? (node-right n))) (list (node-value (node-left n)) 'X))
    (else (list (node-value(node-left n))(node-value(node-right n))))
    )
  )

;--------------------------------------
; Definition of get-node-fields
;   paramters: n node
;   value: used for testing
;           creates a list of all field values
;           of node n
;--------------------------------------
(define (get-node-fields n)
  (cond
    ((empty? n) "X")
    (else (append (list (node-value n) (node-count n)) (get-child-node-values n)))
    )
  )

;--------------------------------------
; Definition of nl-to-vcl
;   paramters: x list of nodes
;   value: used for testing
;             returns list of the node value
;             and node count feild
;--------------------------------------
(define (nl-to-vcl x)
  (cond
    ((empty? x) (list 'X))
    (else (append (list (list (node-value (car x)) (node-count (car x)))) (nl-to-vcl (cdr x))))
    )
  )


;--------------------------------------
; Definition of nl-to-all
;   paramters: x list of nodes
;   value: used for testing
;             returns list of all the field values
;             of each node in list x
;--------------------------------------
(define (nl-to-all x)
  (cond
    ((empty? x) (list 'X))
    (else (append (list (get-node-fields (car x))) (nl-to-all (cdr x))))
    )
  )

;--------------------------------------
; Definition of delete-from-list
;   paramters: t tree root
;              y list of values
;   value: used for testing
;            deletes list of values y from
;            tree t
;--------------------------------------
(define (delete-from-list t y)
  (map (lambda (x) (delete t x) (displayln (nl-to-vl (traverse t)))) y)
  )






;----------------------------------------------------------------------------
;---------------------------------TEST CASES---------------------------------
;----------------------------------------------------------------------------
#|
(define n10 (node 70 1 null null))
(define n09 (node 65 1 null n10))
(define n08 (node 30 1 null null))
(define n07 (node 87 1 null null))
(define n06 (node 60 1 null n09))
(define n05 (node 37 1 n08 null))
(define n04 (node 12 1 null null))
(define n03 (node 75 1 n06 n07))
(define n02 (node 25 1 n04 n05))
(define n01 (node 50 1 n02 n03))

(define b01 (bst n01))
|#



#|
(define b02 (bst null))
(insert b02 5)
(insert b02 2)
(insert b02 8)

(delete b02 5)
(nl-to-vl (traverse b02))
|#



#|
;Test Case 1 (Required)
(define btree (bst null))                          ; create new bst named btree initialized to empty 
btree                                              ; display its contents
(define rlist (random-list 15 1000))               ; create a random list rlist
rlist                                              ; display its contents
(insert-from-list btree rlist)                     ; insert all values from rlist into btree
|#


#|
;Test Case 2 (Required)
(define btree-2 (bst null))
btree-2
(define list-2 '(50 25 75 12 37 30 45 60 80 12 60))
list-2
(insert-from-list btree-2 list-2)
(nl-to-vcl (traverse btree-2))
|#



#|
;Test Case 3 (Optional)
(define tree-3 (bst null))
tree-3
(define list-3 (random-list 10 1000))
list-3
(insert-from-list tree-3 list-3)
|#



#|
;--------------------------------------
;Required Test Case #1: (get-iop-path)
;--------------------------------------
(define q (bst null))                              ; build tree
(define qlist '(50 25 75 70 80 72 71 74 73))       
(insert-from-list q qlist)

(define n75 (car (find q 75)))                     ; find node 75
(node-value n75)

(nl-to-vl (get-iop-path n75))                      ; find path from 75 to iop
|#




#|
;--------------------------------------
;Required Test Case #2: Delete Subcases
;--------------------------------------

(define bst-1 (bst null))

; --- delete cases that change root
; --- delete case 0.1
(displayln "") (displayln "delete case 0.1")
(set! bst-1 (bst null))
(insert-from-list bst-1 '(50))
(nl-to-all (traverse bst-1))     
(delete bst-1 50)
(nl-to-all (traverse bst-1))     

; --- delete case 1.1
(displayln "") (displayln "delete case 1.1")
(set! bst-1 (bst null))
(insert-from-list bst-1 '(50 25))
(nl-to-all (traverse bst-1)) 
(delete bst-1 50)
(nl-to-all (traverse bst-1))   

; --- delete case 1.2
(displayln "") (displayln "delete case 1.2")
(set! bst-1 (bst null))
(insert-from-list bst-1 '(50 75))
(nl-to-all (traverse bst-1))    
(delete bst-1 50)
(nl-to-all (traverse bst-1))

; --- delete cases that don't change root
; --- delete case 0.2
(displayln "") (displayln "delete case 0.2")
(set! bst-1 (bst null))
(insert-from-list bst-1 '(50 25))
(nl-to-all (traverse bst-1)) 
(delete bst-1 25)
(nl-to-all (traverse bst-1)) 

; --- delete case 0.3
(displayln "") (displayln "delete case 0.3")
(set! bst-1 (bst null))
(insert-from-list bst-1 '(50 75))
(nl-to-all (traverse bst-1))  
(delete bst-1 75)
(nl-to-all (traverse bst-1))      

; --- delete case 1.3
(displayln "") (displayln "delete case 1.3")
(set! bst-1 (bst null))
(insert-from-list bst-1 '(50 25 12))
(nl-to-all (traverse bst-1))     
(delete bst-1 25)
(nl-to-all (traverse bst-1))

; --- delete case 1.4
(displayln "") (displayln "delete case 1.4")
(set! bst-1 (bst null))
(insert-from-list bst-1 '(50 25 35))
(nl-to-all (traverse bst-1))
(delete bst-1 25)
(nl-to-all (traverse bst-1))

; --- delete case 1.5
(displayln "") (displayln "delete case 1.5")
(set! bst-1 (bst null))
(insert-from-list bst-1 '(50 75 60))
(nl-to-all (traverse bst-1))
(delete bst-1 75)
(nl-to-all (traverse bst-1))

; --- delete case 1.6
(displayln "") (displayln "delete case 1.6")
(set! bst-1 (bst null))
(insert-from-list bst-1 '(50 75 85))
(nl-to-all (traverse bst-1))
(delete bst-1 75)
(nl-to-all (traverse bst-1))

; --- delete case 2.1 #1
(displayln "") (displayln "delete case 2.1 #1")
(set! bst-1 (bst null))
(insert-from-list bst-1 '(50 25 75 12 30))
(nl-to-all (traverse bst-1))
(delete bst-1 25)
(nl-to-all (traverse bst-1))

; --- delete case 2.1 #2
(displayln "") (displayln "delete case 2.1 #2")
(set! bst-1 (bst null))
(insert-from-list bst-1 '(50 25 75 12 30 10))
(nl-to-all (traverse bst-1))
(delete bst-1 25)
(nl-to-all (traverse bst-1))

; --- delete case 2.2
(displayln "") (displayln "delete case 2.2")
(set! bst-1 (bst null))
(insert-from-list bst-1 '(50 25 75 12 30 10 13 14))
(nl-to-all (traverse bst-1))
(delete bst-1 25)
(nl-to-all (traverse bst-1))
|#



#|
;--------------------------------------
;Required Test Case #3: Combined Insert/Delete Cases
;--------------------------------------
(define bst-1 (bst null))                    ; initially empty bst

(define list-1 (random-list 10 1000))        ; list of 10 random numbers
(displayln list-1)

(insert-from-list bst-1 list-1)              ; insert numbers from list into bst
(nl-to-vl (traverse bst-1))                  ; snapshot of tree

(delete-from-list bst-1 list-1)              ; delete numbers from list from bst
(nl-to-vl (traverse bst-1))                  ; snapshot of tree
|#