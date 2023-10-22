#lang racket
;--------------------------------------
;Tigran Manukyan
;Class #16842       11am - 12:15pm 
;BST Test Cases
;--------------------------------------


(require "Manukyan-BST-Def.rkt")


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