#lang eopl
;; Integrantes:
;; Juan Jose Atuesta Flor -> Ejercicios: 7-10 14 y 17
;; William Rooselbelt May Barreto -> Ejercicios: 6, 11-13 , 15, 18


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



;; Ejercicio 1

;; filter-pairs : L x (X -> bool) x (Y -> bool) -> L
;; Proposito: dada una lista de pares (x y), retorna una lista con
;; unicamente los pares donde x cumple P y y cumple Q.
;;
;; <lista-de-pares> := ()
;;                   := ((<num> <num>) <lista-de-pares>)
(define filter-pairs
  (lambda (L P Q)
    (if (null? L)
        '()
        (if (and (P (car (car L))) (Q (car (cdr (car L)))))
            (cons (car L) (filter-pairs (cdr L) P Q))
            (filter-pairs (cdr L) P Q)))))

;; Pruebas
(filter-pairs '((3 6) (5 10) (8 12) (7 14)) odd? even?)
(filter-pairs '((5 9) (10 90) (82 7) (15 20)) even? even?)

;; Ejercicio 2

;; transform-pairs : L x (X -> bool) x (X -> X) -> L
;; Proposito: para cada par (x y) de L, si ambos cumplen P aplica F
;; a cada elemento; si no, deja el par sin modificar.
;;
;; <lista-de-pares> := ()
;;                   := ((<num> <num>) <lista-de-pares>)
(define transform-pairs
  (lambda (L P F)
    (if (null? L)
        '()
        (if (and (P (car (car L))) (P (car (cdr (car L)))))
            (cons (list (F (car (car L))) (F (car (cdr (car L)))))
                  (transform-pairs (cdr L) P F))
            (transform-pairs (cdr L) P F)))))

;; Pruebas
(transform-pairs '((2 4) (3 6) (8 10)) even? add1)
(transform-pairs '((3 2) (4 2) (1 5) (2 8)) even? sqr)

;; Ejercicio 3

;; mi-concat : L1 x L2 -> L
;; Proposito: concatena dos listas sin usar la funcion append.
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)
(define mi-concat
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (mi-concat (cdr l1) l2)))))

;; Pruebas
(mi-concat '(1 2) '(3 4))
(mi-concat '() '(a b))

;; flatten-one : L -> L
;; Proposito: elimina exactamente un nivel de anidamiento de L,
;; conservando sin modificar los elementos que no son lista.
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)
(define flatten-one
  (lambda (L)
    (if (null? L)
        '()
        (if (list? (car L))
            (mi-concat (car L) (flatten-one (cdr L)))
            (cons (car L) (flatten-one (cdr L)))))))

;; Pruebas
(flatten-one '((1 2) (3 4) (5 6)))
(flatten-one '((una) (buena idea) ((de programacion))))

;; Ejercicio 4

;; mayor5? : num -> bool
;; Proposito: predicado auxiliar, indica si un numero es mayor o igual a 5.
(define (mayor5? x)
  (>= x 5))

;; Pruebas
(mayor5? 6)
(mayor5? 3)

;; list-remove-if : L x num x (X -> bool) -> L
;; Proposito: elimina el elemento en la posicion n (desde cero) de L
;; unicamente si dicho elemento cumple el predicado P.
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)
(define (list-remove-if L n P)
  (cond
    [(null? L) '()]
    [(zero? n)
     (if (P (car L))
         (cdr L)
         L)]
    [else
     (cons (car L)
           (list-remove-if (cdr L) (sub1 n) P))]))

;; Pruebas
(list-remove-if '(5 8 7 6) 2 odd?)
(list-remove-if '(5 8 7 6) 3 mayor5?)

;; Ejercicio 5

;; reverse-filter-equal? : L1 x L2 x (X -> bool) -> bool
;; Proposito: determina si, considerando solo los elementos de cada
;; lista que cumplen P, L2 corresponde a L1 recorrida en orden inverso.
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)
(define (reverse-filter-equal? L1 L2 P)

  ;; filtrar : L -> L
  ;; Proposito: retorna los elementos de L que cumplen el predicado P.
  (define (filtrar L)
    (cond
      [(null? L) '()]
      [(P (car L))
       (cons (car L) (filtrar (cdr L)))]
      [else
       (filtrar (cdr L))]))

  ;; invertir : L -> L
  ;; Proposito: retorna L en orden inverso, sin usar reverse.
  (define (invertir L)
    (define (aux L acumulado)
      (if (null? L)
          acumulado
          (aux (cdr L)
               (cons (car L) acumulado))))
    (aux L '()))

  ;; iguales? : L1 x L2 -> bool
  ;; Proposito: determina si dos listas son iguales elemento a elemento.
  (define (iguales? L1 L2)
    (cond
      [(and (null? L1) (null? L2)) #t]
      [(or (null? L1) (null? L2)) #f]
      [(equal? (car L1) (car L2))
       (iguales? (cdr L1) (cdr L2))]
      [else #f]))

  (iguales? (filtrar L1)
            (invertir (filtrar L2))))

;; Pruebas
(reverse-filter-equal? '(1 2 3 4 5) '(5 3 1 8 6) odd?)
(reverse-filter-equal? '(1 2 3 4 5) '(5 3 2 1) even?)

;;Ejercicio 6 
;;replace-nth:
;;Proposito:
;;E x R x N x L -> L': Procedimiento que reemplaza unicamente la N-esima ocurrencia del elemento E por el elemento R en la lista L,
;;contando las ocurrencias desde 0, en caso de que el elemento E no tenga n+1 ocurrencias retorna la lista L sin modificaciones.
;;
;;<lista> := ()
;;        := (<valor-de-scheme> <list>)
(define replace-nth
  (lambda (E R N L)
    (cond
     [(null? L) L]
     [(equal? (car L) E)(if (equal? N 0) (cons R (cdr L)) (cons (car L)(replace-nth E R (- N 1) (cdr L))))]
     [else (cons (car L)(replace-nth E R N (cdr L)))]
     )
    )
  )
;;Pruebas
(replace-nth 'a 'x 0 '(a))
(replace-nth 'a 'x 1 '())
(replace-nth 'a 'x 3 '(a b a c a))
(replace-nth 't 'x 0 '(a b a c))



;; Ejercicio 7
;; cartesian-filter
;; Proposito:
;; L x L x P -> L' : Procedimiento que dado dos listas, devuelve una lista con  todas los pares posibles entre dos listas que cumplen con la condicion dada por el predicado P
;; <tupla> := ( <int> <int> )
;; <lista> := ()
;;         := ( <tupla> <lista>)

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

;; Ejercicio 8
;; group-by 
;; Proposito:
;; F x L -> ((v, L')) : Procedimiento para que dado una funcion F y una lista L retorna una lista de pares de forma (v, L') donde c es un valor producido por F y L' la lista que contiene
;; todos los elementos de L para los cuales la función F retorna dicho valor 
;; <tupla> := (<Schame-Value> <lista>)
;; <lista> := ()
;;         := ( <Schame-Value> <lista>)
;; <grupos> := ()
;;          := ( <tupla> <grupos>)


(define group-by (lambda (F l)
        (define isInList? (lambda (x lista)
              (
               cond
               [(null? lista) #f]
               [(equal? (car lista) x) #t]
               [else (isInList? x (cdr lista))]
               )
                            ))


 (define posiblesValores (lambda (l1 posiblesV)
                  (
                   cond 
                   [(null? l1) posiblesV]
                   [(isInList? (F (car l1)) posiblesV) (posiblesValores (cdr l1) posiblesV)]
                   [else (posiblesValores (cdr l1) (myAppend posiblesV (cons (F(car l1)) '())))]
                   )
                                  ))
  (define list_group (lambda (x lista)
                       (
                        cond
                        [(null? lista) '()]
                        [(equal? (F (car lista)) x) (cons (car lista) (list_group x (cdr lista)))]
                        [else (list_group x (cdr lista))]
                        )
                       ))
  (define group-creator (lambda (listaEntrada listaValores listaFinal) 
        (
        cond 
        [(null? listaValores) listaFinal]
        [else (group-creator listaEntrada (cdr listaValores)  (myAppend listaFinal (cons (cons 
                                                                  (car listaValores) (cons 
                                                                                      (list_group (car listaValores) listaEntrada) '())) '())))]

                                                                         ))
     )
   (group-creator l (posiblesValores l '()) '())
                   ))

;; Pruebas
 (group-by (lambda (x) (if (number? x) 'numero 'otro)) '(a 2 b 4 c 7 9 0 d f y 4 b))
(group-by (lambda (x) (if (= (remainder x 2) 0) 'par 'impar)) '(1 2 3 4 5 6 7 8 9))
(group-by (lambda (x) (* x x)) '(1 2 1 3 4 5 5 9))



;; Ejercicio 9



;; Ejercicio 10



;;Ejercicio 11
;;merge-by:
;;Proposito:
;;F x L1 x L2 -> L': Compara posicion a posicion los elementos de dos listas L1 y L2 del mismo tamaño usando la funcion binaria F, si (F (car L1) (car L2)) es #t entonces
;;retorna una nueva lista con el elemento de L1 en la posicion n-esima, en caso contrario sera con el elemento de L2
;;<lista>:= ()
;;      := (<valor-de-scheme><lista>)
(define merge-by
  (lambda (F L1 L2)
    (cond
      [(null? L1)'()]
      [(F (car L1) (car L2))(cons (car L1) (merge-by F (cdr L1) (cdr L2)))]
      [else (cons (car L2) (merge-by F (cdr L1) (cdr L2)))]
      )
    )
  )
;;Pruebas
(merge-by > '() '())
(merge-by equal? '(a b c) '(a z c))



;; Ejercicio 12
;;filter-map-acum
;;Proposito
;; a x b x G x F x Acum x P -> Valor: Programa que recorre los numeros del intervalo (a,b). Para cada numero que satisface el predicado P, aplica la funcion G y combina el resultado
;;con el acumulador, mediante la función binaria F. Retorna el valor final del acumulador
;;<entero>:= ... |-2|-1|0|1|2|...
(define filter-map-acum
  (lambda (a b G F acum P)
    (cond
     [(> a b) acum]
     [(P a) (filter-map-acum (+ a 1) b G F (F acum (G a)) P)]
     [else (filter-map-acum (+ a 1) b G F acum P)]
     )
    )
  )
;;Pruebas

(filter-map-acum 5 1 (lambda (x) (* x 2)) + 50 odd?)
(filter-map-acum 3 3 (lambda (x) (* x x)) + 0 odd?)

;; Ejercicio 13
;;operate
;;Proposito:
;;L x L -> Valor: Procedimiento que aplica sucesivamente las funciones binarias de la lista lrators a los valores e la lista lrands y retorna el resultado de las operaciones.
;;<lista-de-funciones>:= () | (<funcion-binaria><lista-de-funciones>)
;;<lista-de-numeros>:= () | (<numero><lista-de-numeros>)
(define operate
  (lambda (lrators lrands)
    (cond
      [(null? lrators) (car lrands)]
      [else (operate (cdr lrators) (cons((car lrators) (car lrands) (cadr lrands)) (cddr lrands)))]
      )
    )
  )
;;Pruebas
(operate '() '(42))
(operate (list + * -) '(2 3 0 7))



;; Ejercicio 14



;; Ejercicio 15
;;same-elements?
;;Proposito
;;BST1 x BST2 -> Booleano
;;Recibe dos arboles binarios de busqueda representados por listas BST1 y BST2 Y determina si ambos contienen exactamente los mismos numeros independientemente de la estructura de los
;;nodos, devuelve #t si son exactamente igualels y #f en el caso constrario
;;<arbol-binario>:= empty
;;               := (numero <arbol-binario> <arbol-binario>)

;; Funciones auxiliares

;; my-append:
;; Proposito:
;; L x L -> L' : Une dos listas recursivamente 
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

;;arbol-a-lista:
;;Proposito
;; BST -> L: Convierte el arbol binario de busqueda representado por listas y retorna una lista ordenada con todos sus numeros.
;;<arbol-binario>:= () | (<numero> <arbol-binario> <arbol-binario>
;;               := () | (<numero> <lista-de-numeros>)

(define mi-append
  (lambda (L1 L2)
    (cond
      [(null? L1) L2]
      [else (cons (car L1) (mi-append (cdr L1) L2))]
      )
    )
  )                               
;;Pruebas
(mi-append '(7 5 3) '(4 9 1))
(mi-append ' (8 6 2 1) '(7 8))
(mi-append '(3 2 1) '(4 6 2 8 9))

(define arbol-a-lista
  (lambda (BST)
    (cond
      [(null? BST) '()]
      [else (mi-append (arbol-a-lista (cadr BST)) (cons (car BST) (arbol-a-lista (caddr BST))))]
      )
    )
  )
;;Pruebas
(arbol-a-lista '())
(arbol-a-lista '(5 () ()))
(arbol-a-lista '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))

(define same-elements?
  (lambda (BST1 BST2)
      (equal? (arbol-a-lista BST1) (arbol-a-lista BST2))
      )
    )
;;Pruebas
(same-elements? '() '())
(same-elements? '() '(5 () ()))
(same-elements? '(5 (3 () ()) (7 () ()))
                '(5 (3 () ()) ()))



;; Ejercicio 16

;; caminos-suma : BST x num -> L
;; Proposito: retorna todos los caminos desde la raiz hasta una hoja
;; de un arbol binario cuya suma de valores sea exactamente n,
;; en orden de recorrido izquierda-derecha.
;;
;; <arbol-binario> := empty
;;                 := (numero <arbol-binario> <arbol-binario>)
(define (caminos-suma arbol n)

  ;; invertir : L -> L  (reutilizada del ejercicio 5)
  (define (invertir L)
    (define (aux L acum)
      (if (null? L)
          acum
          (aux (cdr L)
               (cons (car L) acum))))
    (aux L '()))

  ;; buscar : BST x num x L x L -> L
  ;; Proposito: recorre el arbol acumulando caminos validos en res.
  (define (buscar arbol suma camino res)
    (cond
      [(null? arbol)
       res]
      [else
       (let* ([valor (car arbol)]
              [izq (cadr arbol)]
              [der (caddr arbol)]
              [nueva-suma (+ suma valor)]
              [nuevo-camino (cons valor camino)])
         (cond
           [(and (null? izq) (null? der))
            (if (= nueva-suma n)
                (cons (invertir nuevo-camino) res)
                res)]
           [else
            (let ([res-izq (buscar izq nueva-suma nuevo-camino res)])
              (buscar der nueva-suma nuevo-camino res-izq))]))]))

  (invertir (buscar arbol 0 '() '())))

;; Pruebas
(caminos-suma '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))) 12)
(caminos-suma '(-7 (-8 () ()) (3 (-5 (-6 () ()) (-4 () (-2 () ()))) ())) -15)

;; Ejercicio 17



;;Ejercicio 18
;;to-infinix
;;Proposito
;;E -> E': Recibe una expresion E en notacion prefija representada mediante listas y retorna la misma expresion en notacion infija
;;<expresion>:= <numero> | <operador> <expresion> <expresion>
;;<operador> := + | - | * | / | <simbolo>
(define to-infinix
  (lambda (E)
    (cond
      [(integer? E) E]
      [else (cons (to-infinix (cadr E)) (cons (car E) (cons (to-infinix (caddr E))'())))]
      )
    )
  )
(to-infinix 42)
(to-infinix '( - 10 5))
(to-infinix '(/ (+ (* 2 3) 4) (- 10 2)))
