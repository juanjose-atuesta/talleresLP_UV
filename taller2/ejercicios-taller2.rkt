#lang eopl
;; Juan Jose Atuesta Flor - 2438928
;; William Rooselbelt May Barreto - 2435731
;; Miguel Angel Martinez Eraso - 2437814

;;Declaración de uso de la IA
;;En este taller se uso la IA para entender conceptos de parse y unparse, para entender en su totalidad que hace un constructor, extractor y predicados y para la generacion de
;;algunos casos de prueba.

;;===============================================
;;
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
;;(const->value '(const 20)) -> 20

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

;;mul->left
;;Proposito:
;;expresion -> expresion: Extractor que obtiene la expresion de la izquierda de una multiplicacion basada en listas

(define mul->left
  (lambda (exp)
    (cadr exp)
    )
  )
;;Pruebas
;;(mul->left '(mul (var x)(const 7))) -> '(var x)
;;(mul->left '(mul (const 5)(var y))) -> '(const 5)

;;mul->right
;;Proposito
;;expresion -> expresion: Extractor que obtiene la expresion de la derecha de una multiplicacion basada en listas

(define mul->right
  (lambda (exp)
    (caddr exp)
    )
  )
;;Pruebas
;;(mul->right '(mul (var x)(const 7))) -> '(const 7)
;;(mul->right '(mul (const 5)(var y))) -> '(var y)


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


;;===============================================
;;
;; PARSE-LT : SchemeVal -> List
;; Propósito:
;; Recibe una representación en sintaxis concreta (notación infija) y construye
;; su correspondiente representación abstracta en listas.

(define PARSE-LT
  (lambda (expr)
    (cond
      [(number? expr) (const expr)]
      [(symbol? expr) (var expr)]
      [(pair? expr)
       (let ([left (car expr)]
             [op (cadr expr)]
             [right (caddr expr)])
         (cond
           [(eqv? op '+) (add (PARSE-LT left) (PARSE-LT right))]
           [(eqv? op '-) (sub (PARSE-LT left) (PARSE-LT right))]
           [(eqv? op '*) (mul (PARSE-LT left) (PARSE-LT right))]
           [else (eopl:error 'PARSE-LT "Operador desconocido: ~s" op)]))]
      [else (eopl:error 'PARSE-LT "Sintaxis concreta inválida: ~s" expr)])))

;;Pruebas
;; 1. Número constante
(PARSE-LT 5)

;; 2. Variable
(PARSE-LT 'x)

;; 3. Expresión simple (Suma)
(PARSE-LT '(3 + x))

;; 4. Expresión compuesta (Multiplicación de suma y resta)
(PARSE-LT '((3 + x) * (y - 2)))

;; 5. Expresión anidada compleja
(PARSE-LT '(((a * 5) + (10 - b)) * c))


;; PARSE-DT : SchemeVal -> expresion
;; Propósito:
;; Recibe una representación en sintaxis concreta (notación infija) y construye
;; su correspondiente árbol de sintaxis abstracta basado en datatypes.

(define PARSE-DT
  (lambda (expr)
    (cond
      [(number? expr) (const-exp expr)]
      [(symbol? expr) (var-exp expr)]
      [(pair? expr)
       (let ([left (car expr)]
             [op (cadr expr)]
             [right (caddr expr)])
         (cond
           [(eqv? op '+) (add-exp (PARSE-DT left) (PARSE-DT right))]
           [(eqv? op '-) (sub-exp (PARSE-DT left) (PARSE-DT right))]
           [(eqv? op '*) (mul-exp (PARSE-DT left) (PARSE-DT right))]
           [else (eopl:error 'PARSE-DT "Operador desconocido: ~s" op)]))]
      [else (eopl:error 'PARSE-DT "Sintaxis concreta inválida: ~s" expr)])))

;;Pruebas 

;; 1. Número constante
(PARSE-DT 5)

;; 2. Variable
(PARSE-DT 'x)

;; 3. Expresión simple (Suma)
(PARSE-DT '(3 + x))

;; 4. Expresión compuesta (Multiplicación de suma y resta)
(PARSE-DT '((3 + x) * (y - 2)))

;; 5. Expresión anidada compleja
(PARSE-DT '(((a * 5) + (10 - b)) * c))


;; UNPARSE-LT : List -> SchemeVal
;; Propósito:
;; Recibe una estructura en sintaxis abstracta basada en listas y retorna
;; la sintaxis concreta equivalente en notación infija.

(define UNPARSE-LT
  (lambda (expr)
    (cond
      [(const? expr) (const->value expr)]
      [(var? expr) (var->name expr)]
      [(add? expr)
       (list (UNPARSE-LT (add->left expr))
             '+
             (UNPARSE-LT (add->right expr)))]
      [(sub? expr)
       (list (UNPARSE-LT (sub->left expr))
             '-
             (UNPARSE-LT (sub->right expr)))]
      [(mul? expr)
       (list (UNPARSE-LT (mul->left expr))
             '*
             (UNPARSE-LT (mul->right expr)))]
      [else (eopl:error 'UNPARSE-LT "Estructura abstracta en listas inválida: ~s" expr)])))

;;Pruebas 
;; 1. Número constante
(UNPARSE-LT '(const 5))

;; 2. Variable
(UNPARSE-LT '(var x))

;; 3. Expresión simple (Suma)
(UNPARSE-LT '(add (const 3) (var x)))

;; 4. Expresión compuesta (Multiplicación de suma y resta)
(UNPARSE-LT '(mul (add (const 3) (var x)) (sub (var y) (const 2))))

;; 5. Expresión anidada compleja
(UNPARSE-LT '(mul (add (mul (var a) (const 5)) (sub (const 10) (var b))) (var c)))


;; UNPARSE-DT : expresion -> SchemeVal
;; Propósito:
;; Recibe una estructura de árbol en sintaxis abstracta basada en datatypes
;; y retorna la sintaxis concreta equivalente en notación infija.

(define UNPARSE-DT
  (lambda (expr)
    (cases expresion expr
      (const-exp (num) num)
      (var-exp (id) id)
      (add-exp (left right)
        (list (UNPARSE-DT left) '+ (UNPARSE-DT right)))
      (sub-exp (left right)
        (list (UNPARSE-DT left) '- (UNPARSE-DT right)))
      (mul-exp (left right)
        (list (UNPARSE-DT left) '* (UNPARSE-DT right))))))

;;Pruebas 
;; 1. Número constante
(UNPARSE-DT (const-exp 5))

;; 2. Variable
(UNPARSE-DT (var-exp 'x))

;; 3. Expresión simple (Suma)
(UNPARSE-DT (add-exp (const-exp 3) (var-exp 'x)))

;; 4. Expresión compuesta (utilizando PARSE-DT)
(UNPARSE-DT (PARSE-DT '((3 + x) * (y - 2))))

;; 5. Expresión anidada compleja (utilizando PARSE-DT)
(UNPARSE-DT (PARSE-DT '(((a * 5) + (10 - b)) * c)))

;;===============================================
;;
;;Ejercicio 3: Miguel Martinez
;;Evaluacion de expresiones y ambientes con datatypes

;;Gramatica del ambiente
;;<ambiente> ::= (empty-env-record)
;;           ::= (extended-env-record (<simbolo>*) (<numero>*) <ambiente>)

;;ambiente
;;Proposito: Define el TAD ambiente. Un ambiente es vacio o es otro ambiente
;;extendido con una lista de simbolos y una lista de valores asociados

(define-datatype ambiente ambiente?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of number?))
   (env ambiente?))
  )

;;empty-env
;;Proposito:
;;() -> ambiente: Constructor que retorna un ambiente vacio

(define empty-env
  (lambda ()
    (empty-env-record)
    )
  )

;;Pruebas
;;(ambiente? (empty-env)) -> #t
;;(APPLY-ENV (empty-env) 'x) -> error: no hay ligadura para x

;;extend-env
;;Proposito:
;;Lista-de-simbolos x Lista-de-numeros x ambiente -> ambiente: Constructor que extiende
;;un ambiente asociando cada simbolo de la primera lista con el valor de la misma posicion

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)
    )
  )

;;Pruebas
;;(ambiente? (extend-env '(x y) '(1 2) (empty-env))) -> #t
;;(APPLY-ENV (extend-env '(a) '(7) (empty-env)) 'a) -> 7

;;buscar-en-rib
;;Proposito:
;;Simbolo x Lista-de-simbolos x Lista-de-numeros x ambiente -> numero: Funcion auxiliar que
;;recorre a la vez la lista de simbolos y la de valores. Si encuentra el simbolo retorna el
;;valor de la misma posicion; si no, busca en el ambiente anterior

(define buscar-en-rib
  (lambda (variable syms vals env)
    (cond
      [(null? syms) (APPLY-ENV env variable)]
      [(eqv? (car syms) variable) (car vals)]
      [else (buscar-en-rib variable (cdr syms) (cdr vals) env)]
      )
    )
  )

;;Pruebas
;;(buscar-en-rib 'y '(x y z) '(5 10 2) (empty-env)) -> 10
;;(buscar-en-rib 'w '(x) '(5) (extend-env '(w) '(8) (empty-env))) -> 8

;;APPLY-ENV
;;Proposito:
;;ambiente x Simbolo -> numero: Retorna el valor asociado a una variable en el ambiente.
;;Si la variable no esta ligada, reporta un error

(define APPLY-ENV
  (lambda (amb variable)
    (cases ambiente amb
      (empty-env-record ()
                        (eopl:error 'APPLY-ENV "No hay ligadura para la variable: ~s" variable))
      (extended-env-record (syms vals env)
                           (buscar-en-rib variable syms vals env))
      )
    )
  )

;;Pruebas
;;(APPLY-ENV (extend-env '(x y z) '(5 10 2) (empty-env)) 'x) -> 5
;;(APPLY-ENV (extend-env '(x) '(100) (extend-env '(x y) '(5 10) (empty-env))) 'x) -> 100
;;(APPLY-ENV (extend-env '(x) '(100) (extend-env '(x y) '(5 10) (empty-env))) 'y) -> 10

;;EVALUAR
;;Proposito:
;;expresion x ambiente -> numero: Evalua una expresion en sintaxis abstracta (datatype)
;;usando el ambiente para obtener el valor de las variables

(define EVALUAR
  (lambda (expr amb)
    (cases expresion expr
      (const-exp (num) num)
      (var-exp (id) (APPLY-ENV amb id))
      (add-exp (left right)
               (+ (EVALUAR left amb) (EVALUAR right amb)))
      (sub-exp (left right)
               (- (EVALUAR left amb) (EVALUAR right amb)))
      (mul-exp (left right)
               (* (EVALUAR left amb) (EVALUAR right amb)))
      )
    )
  )

;;Pruebas

(define ambiente-prueba
  (extend-env '(x y z) '(5 10 2) (empty-env)))

(define ambiente-anidado
  (extend-env '(x a) '(100 7) ambiente-prueba))

;; 1. Ejemplo del taller: ((3 * 5) * (10 - 2)) = 120
(EVALUAR (PARSE-DT '((3 * x) * (y - 2))) ambiente-prueba)

;; 2. Constante
(EVALUAR (const-exp 42) ambiente-prueba)

;; 3. Variable
(EVALUAR (var-exp 'z) ambiente-prueba)

;; 4. Suma y resta: (5 + (10 - 2)) = 13
(EVALUAR (PARSE-DT '(x + (y - z))) ambiente-prueba)

;; 5. Variable repetida en ambiente anidado, x vale 100 y a vale 7: ((100 * 7) + 2) = 702
(EVALUAR (PARSE-DT '((x * a) + z)) ambiente-anidado)

;; 6. Usando los datatypes directamente: (10 - (x * y)) = 10 - 50 = -40
(EVALUAR (sub-exp (const-exp 10) (mul-exp (var-exp 'x) (var-exp 'y))) ambiente-prueba)