; Autores: Juan Pablo Ospina, 2411023. Diana Marcela Oviedo, 2459375
; <circuito> ::= circ_simple(<cable>*)(<cable>*) <chip>
;                 simple-circuit (in out chip)
;             ::= circ.comp <circuito> {<circuito>}+ input {cable}* output {cable}*
;                 complex-circuit(circ lcircs in out)

; <chip> ::= <chip_prim>
;             prim-chip(chip_prim)

;             ::= chip ( ---> {(port)}*)
;                     (<--- {(port)}*)
;                     <circuito>
;             comp-chip(in out circ)

; <chip prim> ::= prim_or
;                 chip-or()

;             ::= prim_and
;                 chip-and()

;             ::= prim_not
;                 chip-not()

;             ::= prim_xor
;                 chip-xor()

;             ::= prim_nand
;                 chip-nand()

;             ::= prim_nor
;                 chip-nor()

;             ::= prim_xnor
;                 chip-xnor()


; Proposito: Construir el parser y unparser para pasar de sintaxis concreta a abstracta y viceversa
#lang eopl

(define a (complex-circuit
 (simple-circuit '(m n o p)
                 '(e f)
                 (comp-chip '(INA INB INC IND)
                            '(OUTD OUTF)
                            (complex-circuit
                             (simple-circuit '(a b) '(e) (prim-chip (chip-and)))
                             (list (simple-circuit '(c d) '(f) (prim-chip (chip-and))))
                             '(a b c d)
                             '(e f))))
 (list (simple-circuit
        '(e f)
        '(z)
        (comp-chip '(INE INF) '(OUTA) (simple-circuit '(e f) '(g) (prim-chip (chip-or))))))
 '(m n o p)
 '(z)))


(define b (comp-chip '(INA INB INC IND)
           '(OUTA)
           (complex-circuit (simple-circuit '(a b) '(e) (prim-chip (chip-and)))
                            (list (simple-circuit '(c d) '(f) (prim-chip (chip-and)))
                                  (simple-circuit '(e f) '(g) (prim-chip (chip-or))))
                            '(a b c d)
                            '(g))))

(define parse
    (lambda (exp)
        (cond
            [(equal? (car exp) 'simple-circuit) (simple-circuit (cadr exp) (caddr exp) (parse (cadddr exp)))]
            [(equal? (car exp) 'complex-circuit) (complex-circuit (cadr exp) (caddr exp) (cadddr exp) (cadddr (cdr exp)))]
            [(equal? (car exp) 'prim-chip) (prim-chip (parse (cadr exp)))]
            [(equal? (car exp) 'comp-chip) (comp-chip (cadr exp) (caddr exp) (parse (cadddr exp)))]
            [(equal? (car exp) 'chip-or) (chip-or)]
            [(equal? (car exp) 'chip-and) (chip-and)]
            [(equal? (car exp) 'chip-not) (chip-not)]
            [(equal? (car exp) 'chip-xor) (chip-xor)]
            [(equal? (car exp) 'chip-nand) (chip-nand)]
            [(equal? (car exp) 'chip-nor) (chip-nor)]
            [(equal? (car exp) 'chip-xnor) (chip-xnor)]
        )
    )
)

(define unparse
    (lambda (exp)
        (cases circuito exp
            (simple-circuit (in out chip) (list 'simple-circuit in out (unparse chip)))
            (complex-circuit (circ lcircs in out) (list 'complex-circuit circ lcircs in out))
            (prim-chip (chip_prim) (list 'prim-chip (unparse chip_prim)))
            (comp-chip (in out circ) (list 'comp-chip in out (unparse circ)))
            (chip-or () (list 'chip-or))
            (chip-and () (list 'chip-and))
            (chip-not () (list 'chip-not))
            (chip-xor () (list 'chip-xor))
            (chip-nand () (list 'chip-nand))
            (chip-nor () (list 'chip-nor))
            (chip-xnor () (list 'chip-xnor))
        )
    )
)