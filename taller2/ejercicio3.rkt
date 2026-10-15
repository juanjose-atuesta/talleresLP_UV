#lang eopl

;; =============================================================================
;; TALLER 2 - EJERCICIO 3: EVALUACIÓN DE EXPRESIONES Y MANEJO DE AMBIENTES
;; Curso: Fundamentos de Interpretación y Compilación de Lenguajes de Programación
;; Universidad del Valle
;; =============================================================================

;; =============================================================================
;; GRAMÁTICAS EN FORMA BNF
;; =============================================================================
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
;; =============================================================================

;; =============================================================================
;; 1. DEFINICIÓN DEL TAD EXPRESION (DATATYPES)
;; =============================================================================

;; expresion
;; Propósito: Define el TAD abstracto de expresiones aritméticas con soporte para
;;            constantes enteras, variables, sumas, restas y multiplicaciones.
(define-datatype expresion expresion?
  (const-exp (num integer?))
  (var-exp (id symbol?))
  (add-exp (left expresion?) (right expresion?))
  (sub-exp (left expresion?) (right expresion?))
  (mul-exp (left expresion?) (right expresion?)))

;; =============================================================================
;; 2. PARSEADOR: SINTAXIS CONCRETA -> SINTAXIS ABSTRACTA (DATATYPES)
;; =============================================================================

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

;; =============================================================================
;; 3. DEFINICIÓN DEL TAD AMBIENTE (DATATYPES)
;; =============================================================================

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

;; =============================================================================
;; 4. EVALUADOR DE EXPRESIONES (EVALUAR)
;; =============================================================================

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

;; =============================================================================
;; 5. PRUEBAS UNITARIAS Y EJEMPLOS DE USO
;; =============================================================================

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

;; Ejemplo Oficial del Taller:
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