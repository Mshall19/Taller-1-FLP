#lang eopl
;autores: Kevin Alexis Lorza 2266098m Cesar David Peñaranda 2266265, Juan David Perez 2266289


;en este apartado se repersentan tanto los circuitos simples y compuestos 
;representados por una lista (c1 y c2), como los chips simples y compuestos 

(define-datatype circuito circuito?
    (cir_simple (c1 (list-of symbol?))
                (c2 (list-of symbol?))
                (ch chip?))
    (cir_comp   (cir1 circuito?)
                (cir2 (list-of circuito?))
                (in (list-of symbol?))
                (out (list-of symbol?)))
                )

(define-datatype chip chip?
    (prim_chip
        (cp chip_prim?))
    (comp_chip
        (c1 (list-of symbol?))
        (c2 (list-of symbol?))
        (cir circuito?))
        )

;aqui definimos varios tipos de chips primitivos como prim_or, prim_and, prim_nor, prim_nand, prim_not, prim_xor y prim_xnor

(define-datatype chip_prim chip_prim?
    (prim_or)
    (prim_and)
    (prim_nor)
    (prim_nand)
    (prim_not)
    (prim_xor)
    (prim_xnor)
    )

;y por ultimop aqui creamos los circuitos y chips, ya sean simples o compuestos, usando tipos diferentes tipos de datos
;para representar conexiones entre entradas y salidas con chips lógicos and y or
(define chip1
    (comp_chip
    '(INA INB INC IND)
    '(OUTA)
    (cir_comp
        (cir_simple '(a b) '(e)
        (prim_chip (prim_and)))
        (list
        (cir_simple '(c d) '(f)
            (prim_chip (prim_and)))
        (cir_simple '(e f) '(g)
            (prim_chip (prim_or))))
    '(a b c d)
    '(g)))
    )

(define circuito1
    (cir_comp
        (cir_simple '(a b) '(e)
        (prim_chip (prim_and)))
        (list
        (cir_simple '(c d) '(f)
            (prim_chip (prim_and)))
        (cir_simple '(e f) '(g)
            (prim_chip (prim_or))))
    '(a b c d)
    '(g))
    )