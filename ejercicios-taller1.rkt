;; Integrantes:
;; Juan Jose Atuesta Flor -> Ejercicios: 7-10 14 y 17 


;; FUNCIONES AUXILIARES PROPIAS

;; myAppend:
;; Proposito:
;; L x L -> L' : Procedimiento que une dos listas de manera recursiva 
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

(define myAppend (lambda (l1 l2)
               (cond 
                 [(null? l1) l2]
                 [else (cons (car l1) 
                                  (myAppend (cdr l1) l2))]
                               )))
;; pruebas
(myAppend '(1 2 3) '(1 2 3))
(myAppend ' (2 3 4 5) '(5 7 8))



;; Ejercicio 7
;; cartesian-filter
;; Proposito:
;; L x L x P -> L' : Procedimiento que dado dos listas, devuelve una lista con  todas los pares posibles entre dos listas que cumplen con la condicion dada por el predicado P

;; <lista> :=
(define cartesian-filter 
  (lambda (l1 l2 F)
  (define combinacion
     (
      lambda (x l)
      (
      cond 
      [(null? l) '()]         
      [(F x (car l)) (cons  
              (cons x (cons (car l) '())) 
              (combinacion x (cdr l)  ))]
      [else (combinacion x (cdr l))] 
      )
      ) 
       )

    (
    cond 
    [(null? l1) '()]
    [ else (myAppend (combinacion (car l1) l2) (cartesian-filter (cdr l1) l2 F) )]
     )
  

    )
)

;; pruebas
(cartesian-filter '(1 2 3) '(4 5 6) (lambda (x y) (< x y)))
(cartesian-filter '(1 2 3) '(1 2 3 4) (lambda (x y) (= (+ x y) 5)))

