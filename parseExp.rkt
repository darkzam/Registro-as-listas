#lang eopl

(define-datatype reg reg?
  (reg-vacio)
  (reg-exp (item itm?) (reg reg?))
  )

(define-datatype itm itm?
  (item-assigned (id symbol?)(val number?))
  (item-unassigned (id symbol?) (vac vac?))
  )

(define-datatype vac vac?
  (vacio)
  )

(define registro(lambda(item reg)
                  (if(vac? reg)
                     (reg-exp item (reg-vacio))
                     (reg-exp item reg)
                     
                     )))

(define item(lambda(sym val)
              (if(vac? val)
                 (item-unassigned sym val)
                 (item-assigned sym val)
                 )))

(define empty(lambda()
               (vacio
                )))

;parser usando las interfaces (procedimientos) registro,item,empty
;que se comunican internamente con los procedimientos del tipo de dato
;ej: (parser (registro (item 'g 20) (registro (item 'h (empty)) (empty))))

(define parser(lambda(datum)
                datum
                ))

;parser sin ninguna interfaz, recibe una lista de simbolos y crea el arbol
;de sintaxis abstracta de manera recursiva
;ej: (parse2 '(registro (item g 20) (registro (item h (empty)) (empty))))

(define parse2(lambda(datum)
                 (
                  cond
                   [(eqv? (car datum) 'empty) (reg-vacio)]
                   [(eqv? (car datum) 'item)
                    (if(equal? (caddr datum) '(empty))
                       (item-unassigned (cadr datum) (vacio))
                       (item-assigned (cadr datum) (caddr datum)))]
                   [(eqv? (car datum) 'registro) (reg-exp (parse2 (cadr datum)) (parse2 (caddr datum)))]
                   [else (eopl:error 'parser2 "No cumple con la gramatica Registro: ~s" (car datum)) ]
                   )))

;unparser , recibe un arbol de sintaxis abstracta (struct)
;y lo convierte a una lista de simbolos o lenguaje definido
;ej: (unparse (parser2 '(registro (item g 20) (registro (item h (empty)) (empty)))))

(define unparse(lambda(datum)
                 (
                  cond
                   [(vac? datum) (list 'empty)]
                   [(itm? datum)
                    (cases itm datum
                        (item-assigned (sym var) (list 'item sym var))
                        (item-unassigned (sym vac) (list 'item sym (unparse vac))))]
                   [(reg? datum)
                    (cases reg datum
                      (reg-vacio () (list 'empty))
                      (reg-exp (item reghijo) (list 'registro (unparse item)(unparse reghijo))))]
                   [else (eopl:error 'unparse "No es un arbol de sintaxis abstracta gramatica Registro: ~s" datum)]
                  )))