#lang eopl
;Ejercicio 6 
;;replace-nth:
;;Proposito:
;;valor-de-Scheme x valor-de-scheme x Numero x Lista -> Lista: Procedimiento que reemplaza unicamente la N-esima ocurrencia del elemento E por el elemento R en la lista L,
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
