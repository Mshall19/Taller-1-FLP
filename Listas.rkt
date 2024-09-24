#lang racket

;; Representación de un chip primitivo
(define (prim-chip chip) chip)
(define (chip-or) '(chip-or))
(define (chip-and) '(chip-and))
(define (chip-not) '(chip-not))
(define (chip-xor) '(chip-xor))
(define (chip-nand) '(chip-nand))
(define (chip-nor) '(chip-nor))
(define (chip-xnor) '(chip-xnor))

;; Predicados para verificar el tipo de chip primitivo
(define (chip-prim? chip)
  (or (equal? chip '(chip-or))
      (equal? chip '(chip-and))
      (equal? chip '(chip-not))
      (equal? chip '(chip-xor))
      (equal? chip '(chip-nand))
      (equal? chip '(chip-nor))
      (equal? chip '(chip-xnor))))

;; Representación de un circuito simple
(define (circuito-simple inputs outputs chip)
  (list 'circuito-simple inputs outputs chip))

;; Predicado para verificar si es un circuito simple
(define (circuito-simple? circuito)
  (and (list? circuito) (equal? (first circuito) 'circuito-simple)))

;; Observadores para extraer los datos de un circuito simple
(define (circuito-simple-inputs circuito) (second circuito))
(define (circuito-simple-outputs circuito) (third circuito))
(define (circuito-simple-chip circuito) (fourth circuito))

;; Representación de un circuito complejo
(define (circuito-complejo inputs outputs sub-circuitos)
  (list 'circuito-complejo inputs outputs sub-circuitos))

;; Predicado para verificar si es un circuito complejo
(define (circuito-complejo? circuito)
  (and (list? circuito) (equal? (first circuito) 'circuito-complejo)))

;; Observadores para extraer los datos de un circuito complejo
(define (circuito-complejo-inputs circuito) (second circuito))
(define (circuito-complejo-outputs circuito) (third circuito))
(define (circuito-complejo-sub-circuitos circuito) (fourth circuito))

;; Ejemplos de circuitos simples y complejos

;; Ejemplo 1: Circuito simple con AND
(define circuito-simple-ejemplo1
  (circuito-simple '(a b) '(s) (prim-chip (chip-and))))

;; Ejemplo 2: Circuito simple con OR
(define circuito-simple-ejemplo2
  (circuito-simple '(x y) '(z) (prim-chip (chip-or))))

;; Ejemplo 3: Circuito simple con NOT
(define circuito-simple-ejemplo3
  (circuito-simple '(i1 i2) '(o1) (prim-chip (chip-not))))

;; Ejemplo 4: Circuito complejo que combina dos circuitos simples (AND y OR)
(define circuito-complejo-ejemplo1
  (circuito-complejo '(a b c) '(d)
    (list (circuito-simple '(a b) '(x) (prim-chip (chip-and)))
          (circuito-simple '(x c) '(d) (prim-chip (chip-or))))))

;; Ejemplo 5: Circuito complejo que combina XOR y NOR
(define circuito-complejo-ejemplo2
  (circuito-complejo '(p q) '(r)
    (list (circuito-simple '(p) '(s) (prim-chip (chip-xor)))
          (circuito-simple '(q s) '(r) (prim-chip (chip-nor))))))

;; Pruebas

;; Prueba para el Ejemplo 1 (Circuito simple con AND)
(circuito-simple-inputs circuito-simple-ejemplo1)  
(circuito-simple-outputs circuito-simple-ejemplo1) 
(circuito-simple-chip circuito-simple-ejemplo1)    
(newline)

;; Prueba para el Ejemplo 2 (Circuito simple con OR)
(circuito-simple-inputs circuito-simple-ejemplo2)  
(circuito-simple-outputs circuito-simple-ejemplo2) 
(circuito-simple-chip circuito-simple-ejemplo2)    
(newline)

;; Prueba para el Ejemplo 3 (Circuito simple con NOT)
(circuito-simple-inputs circuito-simple-ejemplo3)  
(circuito-simple-outputs circuito-simple-ejemplo3)
(circuito-simple-chip circuito-simple-ejemplo3)    
(newline)

;; Prueba para el Ejemplo 4 (Circuito complejo con AND y OR)
(circuito-complejo-inputs circuito-complejo-ejemplo1)  
(circuito-complejo-sub-circuitos circuito-complejo-ejemplo1) 
(newline)

;; Prueba para el Ejemplo 5 (Circuito complejo con XOR y NOR)
(circuito-complejo-inputs circuito-complejo-ejemplo2)
(circuito-complejo-sub-circuitos circuito-complejo-ejemplo2)  
(newline)