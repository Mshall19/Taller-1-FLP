#lang racket
;autores: Kevin Alexis Lorza 2266098m, Cesar David Peñaranda 2266265, Juan David Perez 2266289

;Representación de un chip primitivo
(define (crear-chip-prim tipo)
  (lambda (operacion)
    (cond
      [(eq? operacion 'tipo) tipo])))

;Predicado para verificar si es un chip primitivo
(define (es-chip-prim? chip)
  (or (equal? (chip 'tipo) 'AND)
      (equal? (chip 'tipo) 'OR)
      (equal? (chip 'tipo) 'NOT)
      (equal? (chip 'tipo) 'XOR)
      (equal? (chip 'tipo) 'NAND)
      (equal? (chip 'tipo) 'NOR)
      (equal? (chip 'tipo) 'XNOR)))

;Representación de un circuito simple
(define (crear-circuito-simple entradas salidas chip)
  (lambda (operacion)
    (cond
      [(eq? operacion 'tipo) 'circuito-simple]
      [(eq? operacion 'entradas) entradas]
      [(eq? operacion 'salidas) salidas]
      [(eq? operacion 'chip) chip])))

;Predicado para verificar si es un circuito simple
(define (es-circuito-simple? circuito)
  (equal? (circuito 'tipo) 'circuito-simple))

;Representación de un circuito compuesto
(define (crear-circuito-compuesto entradas salidas subcircuitos)
  (lambda (operacion)
    (cond
      [(eq? operacion 'tipo) 'circuito-compuesto]
      [(eq? operacion 'entradas) entradas]
      [(eq? operacion 'salidas) salidas]
      [(eq? operacion 'subcircuitos) subcircuitos])))

;Predicado para verificar si es un circuito compuesto
(define (es-circuito-compuesto? circuito)
  (equal? (circuito 'tipo) 'circuito-compuesto))

;Extractores para circuitos
(define (obtener-entradas circuito)
  (circuito 'entradas))

(define (obtener-salidas circuito)
  (circuito 'salidas))

(define (obtener-chip circuito)
  (circuito 'chip))

(define (obtener-subcircuitos circuito)
  (circuito 'subcircuitos))

;Función para obtener el tipo del chip
(define (tipo-chip chip)
  (chip 'tipo))

;Función para obtener información de un circuito
(define (informacion-circuito circuito)
  (cond
    [(es-circuito-simple? circuito)
     (list 'circuito-simple
           (obtener-entradas circuito)
           (obtener-salidas circuito)
           (tipo-chip (obtener-chip circuito)))]
    [(es-circuito-compuesto? circuito)
     (cons 'circuito-compuesto
           (list (obtener-entradas circuito)
                 (obtener-salidas circuito)
                 (map informacion-circuito (obtener-subcircuitos circuito))))]))

;Ejemplos de uso

;Creación de chips primitivos
(define chip-and (crear-chip-prim 'AND))
(define chip-or (crear-chip-prim 'OR))
(define chip-not (crear-chip-prim 'NOT))

;Creación de un circuito simple
(define circuito-simple (crear-circuito-simple '(a b) '(out) chip-and))

;Ejemplo 1: Creación de un circuito compuesto con subcircuitos
(define subcircuito-1 (crear-circuito-simple '(x y) '(out1) chip-or))
(define subcircuito-2 (crear-circuito-simple '(out1 z) '(out2) chip-not))
(define circuito-compuesto (crear-circuito-compuesto '(x y z) '(out2) (list subcircuito-1 subcircuito-2)))

;Ejemplo 2: Circuito Simple con un chip XOR
(define circuito-simple-xor (crear-circuito-simple '(a b) '(out) (crear-chip-prim 'XOR)))

;Ejemplo 3: Circuito Compuesto con un chip AND y un chip NAND
(define subcircuito-3 (crear-circuito-simple '(a b) '(out1) (crear-chip-prim 'AND)))
(define subcircuito-4 (crear-circuito-simple '(out1 c) '(out2) (crear-chip-prim 'NAND)))
(define circuito-compuesto-1 (crear-circuito-compuesto '(a b c) '(out2) (list subcircuito-3 subcircuito-4)))

;Ejemplo 4: Circuito Simple con un chip NOR
(define circuito-simple-nor (crear-circuito-simple '(x y) '(out) (crear-chip-prim 'NOR)))

;Ejemplo 5: Circuito Compuesto con chips OR y NOT
(define subcircuito-5 (crear-circuito-simple '(x y) '(out1) (crear-chip-prim 'OR)))
(define subcircuito-6 (crear-circuito-simple '(out1 z) '(out2) (crear-chip-prim 'NOT)))
(define circuito-compuesto-2 (crear-circuito-compuesto '(x y z) '(out2) (list subcircuito-5 subcircuito-6)))

;Obtener información de todos los circuitos creados

(informacion-circuito circuito-compuesto)
(informacion-circuito circuito-simple-xor)
(informacion-circuito circuito-compuesto-1)
(informacion-circuito circuito-simple-nor)
(informacion-circuito circuito-compuesto-2)