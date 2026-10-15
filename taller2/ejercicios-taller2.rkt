#lang eopl
;; Juan Jose Atuesta Flor - 2438928
;; William Rooselbelt May Barreto - 2435731
;; Miguel Angel Martinez Eraso -

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
;;(mul->right '(sub (var x)(const 7))) -> '(const 7)
;;(mul->right '(sub (const 5)(var y))) -> '(var y)


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
;; GRAMÁTICAS EN FORMA BNF
;;
;; 1. Sintaxis Concreta del Lenguaje de Expresiones:
;;    <expresion-concreta> ::= <numero>
;;                         ::= <simbolo>
;;                         ::= (<expresion-concreta> + <expresion-concreta>)
;;                         ::= (<expresion-concreta> - <expresion-concreta>)
;;                         ::= (<expresion-concreta> * <expresion-concreta>)
;;
;; 2. Sintaxis Abstracta del TAD Expresión (Datatypes):
;;    <expresion> ::= (const-exp <numero>)
;;                ::= (var-exp <simbolo>)
;;                ::= (add-exp <expresion> <expresion>)
;;                ::= (sub-exp <expresion> <expresion>)
;;                ::= (mul-exp <expresion> <expresion>)
;;
;; 3. Sintaxis Abstracta del TAD Ambiente (Datatypes):
;;    <environment> ::= (empty-env-record)
;;                  ::= (extended-env-record (list-of symbol?) 
;;                                           (list-of integer?) 
;;                                           <environment>)

;; 1. DEFINICIÓN DEL TAD EXPRESION (DATATYPES)

;; expresion
;; Propósito: Define el TAD abstracto de expresiones aritméticas con soporte para
;;            constantes enteras, variables, sumas, restas y multiplicaciones.
(define-datatype expresion expresion?
  (const-exp (num integer?))
  (var-exp (id symbol?))
  (add-exp (left expresion?) (right expresion?))
  (sub-exp (left expresion?) (right expresion?))
  (mul-exp (left expresion?) (right expresion?)))

;; 2. PARSEADOR: SINTAXIS CONCRETA -> SINTAXIS ABSTRACTA (DATATYPES)

;; PARSE-DT: SchemeVal -> expresion
;; Propósito: Recibe una representación de sintaxis concreta en listas de Racket
;;            y retorna la representación abstracta correspondiente construida
;;            mediante define-datatype.
(define PARSE-DT
  (lambda (expr)
    (cond
      ((integer? expr) (const-exp expr))
      ((symbol? expr) (var-exp expr))
      ((pair? expr)
       (let ((left (car expr))
             (op (cadr expr))
             (right (caddr expr)))
         (cond
           ((eqv? op '+) (add-exp (PARSE-DT left) (PARSE-DT right)))
           ((eqv? op '-) (sub-exp (PARSE-DT left) (PARSE-DT right)))
           ((eqv? op '*) (mul-exp (PARSE-DT left) (PARSE-DT right)))
           (else (eopl:error 'PARSE-DT "Operador no soportado: ~s" op)))))
      (else (eopl:error 'PARSE-DT "Sintaxis concreta inválida: ~s" expr)))))

;; 3. DEFINICIÓN DEL TAD AMBIENTE (DATATYPES)

;; environment
;; Propósito: Define el tipo abstracto de datos para representar ambientes de ligadura
;;            de variables mediante la representación basada en datatypes.
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of integer?))
   (env environment?)))

;; empty-env: () -> environment
;; Propósito: Constructor que retorna un ambiente vacío.
(define empty-env
  (lambda ()
    (empty-env-record)))

;; extend-env: (list-of symbol?) x (list-of integer?) x environment -> environment
;; Propósito: Constructor que extiende un ambiente asociando una lista de símbolos
;;            a una lista correspondiente de valores numéricos.
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

;; list-find-position: symbol x (list-of symbol?) -> integer o #f
;; Propósito: Función auxiliar que busca la posición indexada desde 0 de un símbolo
;;            en una lista de símbolos. Retorna #f si el símbolo no está presente.
(define list-find-position
  (lambda (sym syms)
    (letrec ((loop (lambda (s lst pos)
                     (cond
                       ((null? lst) #f)
                       ((eqv? s (car lst)) pos)
                       (else (loop s (cdr lst) (+ pos 1)))))))
      (loop sym syms 0))))

;; APPLY-ENV: environment x symbol -> integer
;; Propósito: Busca el valor numérico asociado a una variable (símbolo) dentro
;;            del ambiente. Lanza un error si la variable no se encuentra ligada.
(define APPLY-ENV
  (lambda (env search-sym)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'APPLY-ENV "No hay ligadura para la variable: ~s" search-sym))
      (extended-env-record (syms vals old-env)
        (let ((pos (list-find-position search-sym syms)))
          (if (number? pos)
              (list-ref vals pos)
              (APPLY-ENV old-env search-sym)))))))

;; Alias opcional en minúsculas para compatibilidad
(define apply-env APPLY-ENV)

;; 4. EVALUADOR DE EXPRESIONES (EVALUAR)

;; EVALUAR: expresion x environment -> integer
;; Propósito: Recibe una expresión en sintaxis abstracta (DATATYPES) y un ambiente,
;;            y calcula el valor numérico resultante procesando inductivamente
;;            cada variante de la expresión.
(define EVALUAR
  (lambda (expr env)
    (cases expresion expr
      (const-exp (num) num)
      (var-exp (id) (APPLY-ENV env id))
      (add-exp (left right)
        (+ (EVALUAR left env) (EVALUAR right env)))
      (sub-exp (left right)
        (- (EVALUAR left env) (EVALUAR right env)))
      (mul-exp (left right)
        (* (EVALUAR left env) (EVALUAR right env))))))

;; Alias opcional en minúsculas para compatibilidad
(define evaluar EVALUAR)

;; 5. PRUEBAS

;; Definición de Ambientes de Prueba
(define env-vacio (empty-env))
(define env-prueba
  (extend-env
   '(x y z)
   '(5 10 2)
   env-vacio))

;; Ambiente con sombras de variables (shadowing)
(define env-anidado
  (extend-env
   '(x a)
   '(100 7)
   env-prueba))

;; --- Pruebas de Búsqueda en Ambientes (APPLY-ENV) ---
;; Prueba 1: Variable 'x' en env-prueba -> Debe retornar 5
(APPLY-ENV env-prueba 'x)

;; Prueba 2: Variable 'y' en env-prueba -> Debe retornar 10
(APPLY-ENV env-prueba 'y)

;; Prueba 3: Sombreado de 'x' en env-anidado -> Debe retornar 100
(APPLY-ENV env-anidado 'x)

;; --- Pruebas del Evaluador (EVALUAR) ---

;; Ejemplo del Taller:
;; Evaluar '((3 * x) * (y - 2)) con x=5, y=10, z=2
;; Resultado esperado: ((3 * 5) * (10 - 2)) = 15 * 8 = 120
(define ej-oficial (PARSE-DT '((3 * x) * (y - 2))))
(EVALUAR ej-oficial env-prueba) ; -> Retorna 120

;; Prueba 1: Evaluación de constante simple
(EVALUAR (const-exp 42) env-prueba) ; -> Retorna 42

;; Prueba 2: Evaluación de variable
(EVALUAR (var-exp 'z) env-prueba) ; -> Retorna 2

;; Prueba 3: Suma y Resta combinada: '(x + (y - z)) -> (5 + (10 - 2)) = 13
(EVALUAR (PARSE-DT '(x + (y - z))) env-prueba) ; -> Retorna 13

;; Prueba 4: Expresión con ambiente anidado usando 'a' y 'x' sombreado
;; '((x * a) + z) -> ((100 * 7) + 2) = 702
(EVALUAR (PARSE-DT '((x * a) + z)) env-anidado) ; -> Retorna 702
