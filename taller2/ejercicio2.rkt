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
