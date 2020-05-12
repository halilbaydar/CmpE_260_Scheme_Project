#lang scheme
; 2017400297


;You can replace #f's with your function definitions and define more helper functions as you need to use this template.

; Solver function
(define (temp liste) (list(cons (car liste) (cons (cadr liste) (list '() (caddr liste))))'()))
(define (TENTS-SOLUTION liste) (define row_list (car liste)) (define col_list(cadr liste)) (define tree_listesi(caddr liste)) (define table_size(list (length (car liste)) (length (cadr liste))))
(solve tree_listesi (list(cons row_list (cons col_list (list '() (caddr liste))))'()) table_size)) 

(define (solve tree_list all_possible_list table_size)   (cond
                                                         ((null? all_possible_list) #f)
                                                         ((if (null? (car all_possible_list)) (solve tree_list (cdr all_possible_list) table_size)
          (if (and(eq? (it_it_all_zero(caar all_possible_list)) #t) (eq? (it_it_all_zero(cadar all_possible_list)) #t)) ('())
              (solve tree_list (list (add_list(eliminate_neighbor_with_tree_and_tent(caar (cdddr (car all_possible_list))) tree_list (caddar all_possible_list) table_size)
          (caar all_possible_list) (cadar all_possible_list) (caddar all_possible_list) (cadddr (car all_possible_list))) 
          (cdr all_possible_list)) table_size))))))

(define (add_list indexler row_list col_list tent_list tree_list) (cond
                                                        ((null? indexler )'() ) 
                                                        ((if (and (eq? (is_it_available_swap row_list (caar indexler)) #t) (eq? (is_it_available_swap col_list (cadar indexler)) #t)) 
                                                        (cons (list (SWAP-NTH row_list (caar indexler)) (SWAP-NTH col_list (cadar indexler))) (list tent_list (list(car indexler) tree_list)))
                                                        (add_list (cdr indexler)  row_list col_list tent_list tree_list)) (add_list (cdr indexler)  row_list col_list tent_list tree_list))
                                                        ))

(define (is_it_available_swap list n)  (if (eq? n 1)  (if (not (eq? (car list) 0)) #t #f) 
                                           (is_it_available_swap (cdr list) (- n 1))))

(define (it_it_all_zero liste) (if (null? liste) #t (if (eq? (car liste) 0) (it_it_all_zero (cdr liste)) #f)))

(define (SWAP-NTH list n) (if (= n 1)  (if (not (= (car list) 0)) (cons (-(car list) 1) (cdr list)) '()) (cons (car list) (SWAP-NTH (cdr list) (- n 1)))))
(define (REPLACE-NTH list n item) (if (= n 1)  (cons item (cdr list)) (cons (car list) (REPLACE-NTH (cdr list) (- n 1) item))))

; Helper functions
(define RETURN-FIRST-NOT-FALSE (lambda (liste)( if (null? liste) #f(if (> (car liste) 5)(* (car liste) (car liste) )( RETURN-FIRST-NOT-FALSE (cdr liste))))))
(define ADJACENT(lambda (liste1 liste2) (define x1 (car liste1)) (define y1(cadr liste1)) (define x2(car liste2)) (define y2(cadr liste2)) 
                   (if (and (eq? x1 x2)  (eq? (+ y1 1) y2)) #t (if (and (eq? x1 x2)  (eq? (- y1  1) y2)) #t(if (and (eq? y1 y2)  (eq? (+ x1 1) x2)) #t(if (and (eq? y1 y2)  (eq? (- x1  1) x2)) #t
                   (if (and (eq? (- x1 1) x2)  (eq? (+ y1  1) y2)) #t
                   (if (and (eq? (- x1 1) x2)  (eq? (- y1 1) y2)) #t(if (and (eq? (+ x1 1) x2)  (eq? (- y1 1) y2)) #t(if (and (eq? (+ x1 1) x2)  (eq? (+ y1 1) y2)) #t(if (and (eq? x1 x2) (eq? y1 y2)) #t #f)))))))))))

(define ADJACENT-WITH-LIST (lambda (kordinat list1) (if (or (null? list1) (null? kordinat)) #f ( if(ADJACENT kordinat (car list1))  #t (ADJACENT-WITH-LIST kordinat (cdr list1)))))) ;tent

(define NEIGHBOR-LIST (lambda (list1)(define x1 (car list1)) (define y1 (cadr list1))(define liste2 '())(cons(list (- x1 1) y1)  (cons (list (+ x1 1) y1)  (cons (list x1 (- y1 1)) (cons (list x1 (+ y1 1)) liste2)))))) ;tree

(define (append list1 list2) (if (null? list1) list2(cons (car list1) (append (cdr list1) list2))))

(define n-th (lambda (liste n) (if (eq? 1 n) (car liste) (n-th (cdr liste) (- n 1)))))

(define (insert-n list item n) (if (= n 0) (cons item list) (cons (car list) (insert-n (cdr list) item (- n 1)))))

(define (ele liste1 table_size) (cond
                                         ((null? liste1) '())
                                         ((or(> (car(car liste1)) (car table_size))(> (cadr(car liste1)) (cadr table_size)) )  (ele (cdr liste1) table_size))
                                         (else (cons (car liste1) (ele (cdr liste1) table_size)))))

(define eliminate_neighbor_with_tree_and_tent (lambda (kordinat tree_list tent_list table_size) (eliminate_deighbor_with_tent(eliminate_duplicate(ele (NEIGHBOR-LIST kordinat) table_size) tree_list ) tent_list)))

(define (eliminate_duplicate komsu_liste_in_table tree_list) (cond
                                                 ((null? komsu_liste_in_table) '())
                                                 ((if (member (car komsu_liste_in_table) tree_list) (eliminate_duplicate (cdr komsu_liste_in_table) tree_list) (cons (car komsu_liste_in_table)
                                                                                                                                                                     (eliminate_duplicate (cdr komsu_liste_in_table) tree_list)) ))))
(define same(lambda (item tree_list) (if (null? tree_list) #f (if(eq? item (car tree_list)) #t (same item (cdr tree_list))))))
  
(define (deleteitem list1 item) ( cond((null? list1) '()) ((equal? (car list1) item) (deleteitem (cdr list1) item)) (else (cons (car list1) (deleteitem (cdr list1) item)))))
(define (length lst) (cond [(empty? lst)  0] [(cons? lst)   (+ 1 (length (rest lst)))]))


(define (eliminate_deighbor_with_tent list1 tentlist) (cond
                                                      ((null? list1) '())
                                                      ((if (ADJACENT-WITH-LIST (car list1) tentlist ) (eliminate_deighbor_with_tent (cdr list1) tentlist) (cons(car list1) (eliminate_deighbor_with_tent (cdr list1) tentlist)))))) 


                                                      

