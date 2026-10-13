#lang eopl
;; Juan Jose Atuesta Flor - 2438928
;; William Rooselbelt May Barreto - 2435731
;; Miguel Angel Martinez Eraso -

;;Ejercicio 1: William May
;;TAD EXPRESION basado en listas

;;Gramatica
;;<expresion>::= <numero> 
;;           ::= <identificador>
;;           ::= (<expresion> + <expresion>)
;;           ::= (<expresion> - <expresion>)
;;           ::= (<expresion> * <expresion>)

;;Constante
;;const
;;Proposito
;;Int -> expresion: Constructor que recibe un numero entero y retorna la representacion abstracta de listas de una constante
;;<expresion> ::= (const <numero>)

(define const
  (lambda (n)
    (list 'const n)
    )
  )
;;Pruebas 
;;(const 5) -> '(const 5)
;;(const 0) -> '(const 0)
;;(const 9) -> '(const 9)

;;const?
;;Proposito:
;;Valor-de-scheme -> Booleano: Predicado que determina si un valor dado corresponde a la representacion abstracta en listas de una constante

(define const?
  (lambda (exp)
    (and (pair? exp)(eqv? (car exp) 'const))
    )
  )
;;Pruebas 
;;(const? '(const 5)) -> #t
;;(const? '(var x)) -> #f
;;(const? '(const 9)) -> #t

;;const->value
;;Proposito:
;;expresion -> Entero: Extractor que obtiene el valor numerico de una constante

(define const->value
  (lambda (exp)
    (cadr exp)
    )
  )

;;Pruebas
;;(const->value '(const 5)) -> 5
;;(const->value '(const -12)) -> -12
;;(const ->value '(const 20)) -> 20

;;Variable

;;var:
;;Proposito:
;;Simbolo -> expresion: Constructor que recibe un simbolo y retona la representaion abstracta en listas de una variable
;;<expresion> ::= (var <simbolo>)

(define var
  (lambda (id)
    (list 'var id)
    )
  )

;;Pruebas
;;(var 'x) -> '(var x)
;;(var 'y) -> '(var y)
;;(var 'z) -> '(var z)

;;var?
;;Proposito:
;;Valor-de-scheme -> Booleano: Predicado que determina si un valor dado corresponde a la representacion abstracta en listas de una variable

(define var?
  (lambda (exp)
    (and (pair? exp)
         (eqv? (car exp) 'var)
         )
    )
  )

;;Pruebas
;;(var? '(var x)) -> #t
;;(var? '(const 5)) -> #f

;;var->name
;;Proposito
;;expresion -> Simbolo: Extractor que obtiene el nombre o simbolo de una variable

(define var->name
  (lambda (exp)
    (cadr exp)
    )
  )

;;Pruebas
;;(var->name '(var x)) -> 'x
;;(var->name '(var resultado)) -> 'resultado

;;Suma

;;add:
;;Proposito:
;;expresion x expresion -> expresion: Constructor que recibe dos expresiones y retorna la representacion abstracta en listas de su suma

(define add
  (lambda (e1 e2)
    (list 'add e1 e2)
    )
  )

;;Pruebas
;;(add (const 3) (var 'x)) -> '(add (const 3) (var x))
;;(add (var 'a) (var 'b)) -> '(add (var a) (var b))
;;(add (const 5)(const 2)) -> (add (const 5) (const 2))

;;add?
;;Proposito
;;Valor-de-scheme -> Boolean: Predicado que determina si un valor dado corresponde a una suma en la representacion basada en listas

(define add?
  (lambda (exp)
    (and (pair? exp)(eqv? (car exp) 'add))
    )
  )

;;Pruebas
;;(add? '(add (const 3)(var x))) -> #t
;;(add? '(sub (const 3)(var x))) -> #f

;;add->left
;;Proposito:
;;expresion->expresion: Extractor que obtiene la expresion de la izquierda de una suma basada en listas

(define add->left
  (lambda (exp)
    (cadr exp)
    )
  )

;;Pruebas
;;(add->left '(add (const 3)(var x))) -> '(const 3)
;;(add->left '(add (var a)(var b))) -> '(var a)

;;add->rigth
;;Proposito:
;;expresion->expresion: Extractor que obtiene la expresion de la derecha de una suma basada en listas

(define add->right
  (lambda (exp)
    (caddr exp)
    )
  )

;;Pruebas
;;(add->right '(add (const 3)(var x))) -> '(var x)
;;(add->right '(add (var a)(var b))) -> '(var b)

;;Resta

;;sub
;;Proposito:
;;expresion x expresion: Constructor que recibe dos expresiones y retorna la representacion abstracta en listas de una resta

(define sub
  (lambda (e1 e2)
    (list 'sub e1 e2)
    )
  )

;;Pruebas
;;(sub (const 3)(var 'x)) -> '(sub (const 3) (var x))
;;(sub (var 'a)(const 5)) -> '(sub (var a) (const 5))

;;sub?
;;Proposito:
;;Valor-de-Scheme -> Booleano: Predicado que determina si un valor dado corresponde a una resta en la representacion basada en listas

(define sub?
  (lambda (exp)
    (and (pair? exp) (eqv? (car exp) 'sub))
    )
  )

;;Pruebas
;;(sub? '(sub (const 3)(var 'x))) -> #t
;;(sub? '(add (const 3)(var 'x))) -> #f

;;sub->left
;;Proposito:
;;expresion -> expresion: Extractor que obtiene la expresin de la izquierda de una resta basada en listas

(define sub->left
  (lambda (exp)
    (cadr exp)
    )
  )

;;Pruebas
;;(sub->left '(sub (const 3)(var x))) -> '(const 3)
;;(sub->left '(sub (var a)(const 9))) -> '(var a)

;;sub->right
;;Proposito:
;;expresion->expresion: Extractor que obtiene la expresion de la derecha de una resta basada en listas

(define sub->right
  (lambda (exp)
    (caddr exp)
    )
  )
;;Pruebas
;;(sub->right '(sub (const 3)(var x))) -> '(var x)
;;(sub->right '(sub (var a)(const 9))) -> '(const 9)

;;Multiplicacion

;;mul
;;Proposito:
;; expresion x expresion -> expresion: Constructor que recibe dos expresiones y retorna la representacion abstracta en listas de una multiplicacion

(define mul
  (lambda (e1 e2)
    (list 'mul e1 e2)
    )
  )
;;Pruebas
;;(mul (const 3)(var 'x)) -> '(mul (const 3) (var x))
;;(mul (var 'a)(const 23)) -> '(mul (var a) (const 23))

;;mul?
;;Proposito:
;;Valor-de-Scheme -> Booleano: Predicado que determina si un valor dado corresponde a una multiplicacion en la representacion basada en listas

(define mul?
  (lambda (exp)
    (and (pair? exp)(eqv? (car exp) 'mul))
    )
  )
;;Pruebas
;;(mul? '(mul (const 3)(var x))) -> #t
;;(mul? '(add (const 3)(var y))) -> #f
;;(mul? '(sub (const 3) (var z))) -> #f

;;mul-left
;;Proposito:
;;expresion -> expresion: Extractor que obtiene la expresion de la izquierda de una multiplicacion basada en listas

(define mul-left
  (lambda (exp)
    (cadr exp)
    )
  )
;;Pruebas
;;(mul-left '(mul (var x)(const 7))) -> '(var x)
;;(mul-left '(mul (const 5)(var y))) -> '(const 5)

;;mul-right
;;Proposito
;;expresion -> expresion: Extractor que obtiene la expresion de la derecha de una multiplicacion basada en listas

(define mul-right
  (lambda (exp)
    (caddr exp)
    )
  )
;;Pruebas
;;(mul-right '(sub (var x)(const 7))) -> '(const 7)
;;(mul-right '(sub (const 5)(var y))) -> '(var y)


;;Pruebas 
(define c1 (const 5))
(define c2 (const 3))
(define v1 (var 'x))
(define v2 (var 'y))
(define suma1 (add c1 v1))
(define resta1 (sub v2 (const 2)))
(define mult1 (mul c1 c2))
(define expresion-anidada (mul suma1 resta1))
(define expresion-no-trivial-1
    (mul (add (const 3)(var 'x))
         (sub (var 'y)(const 2))))
(define expresion-no-trivial-2
    (mul (add (mul (var 'a)(const 4))
              (sub (var 'b)(const 10)))
         (const 2)))




;;TAD EXPRESION con datatype

;;Gramatica

;;<expresion> ::= (const-exp <numero>)
;;            ::= (var-exp <simbolo>)
;;            ::= (add-exp <expresion><expresion>)
;;            ::= (sub-exp <expresion><expresion>)
;;            ::= (mul-exp <expresion><expresion>)

;;expresion
;;Proposito: Define el TAD expresion y sus variantes para representar constantes, variables, sumas, restas y multiplicaciones

(define-datatype expresion expresion?
  (const-exp (num integer?))
  (var-exp (id symbol?))
  (add-exp
   (left expresion?)
   (right expresion?))
  (sub-exp
   (left expresion?)
   (right expresion?))
  (mul-exp
   (left expresion?)
   (right expresion?))
  )

;;Pruebas

(define k1 (const-exp 5))
(define k2 (const-exp 3))
(define v3 (var-exp 'x))
(define v4 (var-exp 'y))
(define suma-simple (add-exp (const-exp 3)(var-exp 'x)))
(define resta-simple (sub-exp (var-exp 'y)(const-exp 2)))
(define mult-simple (mul-exp (var-exp 'a)(var-exp 'b)))
(define no-trivial-1
  (mul-exp (add-exp (const-exp 3)(var-exp 'x))
           (sub-exp (var-exp 'y)(const-exp 2))
           )
  )
(define no-trivial-2
  (mul-exp (add-exp (mul-exp (var-exp 'a)(const-exp 4))
                    (sub-exp (var-exp 'b)(const-exp 10)))
           (const-exp 2)
           )
  )
(define no-trivial-3
  (add-exp (sub-exp (const-exp 10)
                    (mul-exp (var-exp 'x)(var-exp 'y)))
           (mul-exp (var-exp 'z)(const-exp 5))
           )
  )
   
