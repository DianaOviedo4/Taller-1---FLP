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


; Proposito: Construir un TAD basado en listas para el lenguaje de programación de circuitos
;            digitales LCD.
#lang eopl

;; Constructores
(define simple-circuit (lambda (in out chip) (list 'simple-circuit in out chip)))

(define complex-circuit (lambda (circ lcircs in out) (list 'complex-circuit circ lcircs in out)))

(define prim-chip (lambda (chip_prim) (list 'prim-chip chip_prim)))

(define comp-chip (lambda (in out circ) (list 'comp-chip in out circ)))

(define chip-or (lambda () (list 'chip-or)))

(define chip-and (lambda () (list 'chip-and)))

(define chip-not (lambda () (list 'chip-not)))

(define chip-xor (lambda () (list 'chip-xor)))

(define chip-nand (lambda () (list 'chip-nand)))

(define chip-nor (lambda () (list 'chip-nor)))

(define chip-xnor (lambda () (list 'chip-xnor)))

;; Observadores

;; Predicados

(define simple-circuit? (lambda (x) (equal? (car x) 'simple-circuit)))

(define complex-circuit? (lambda (x) (equal? (car x) 'complex-circuit)))

(define prim-chip? (lambda (x) (equal? (car x) 'prim-chip)))

(define comp-chip? (lambda (x) (equal? (car x) 'comp-chip)))

(define chip-or? (lambda (x) (equal? (car x) 'chip-or)))

(define chip-and? (lambda (x) (equal? (car x) 'chip-and)))

(define chip-not? (lambda (x) (equal? (car x) 'chip-not)))

(define chip-xor? (lambda (x) (equal? (car x) 'chip-xor)))

(define chip-nand? (lambda (x) (equal? (car x) 'chip-nand)))

(define chip-nor? (lambda (x) (equal? (car x) 'chip-nor)))

(define chip-xnor? (lambda (x) (equal? (car x) 'chip-xnor)))

;; Extractores

(define simple-circuit->in (lambda (x) (cadr x)))

(define simple-circuit->out (lambda (x) (caddr x)))

(define simple-circuit->chip (lambda (x) (cadddr x)))

(define complex-circuit->circ (lambda (x) (cadr x)))

(define complex-circuit->lcircs (lambda (x) (caddr x)))

(define complex-circuit->in (lambda (x) (cadddr x)))

(define complex-circuit->out (lambda (x) (cadddr (cdr x))))

(define prim-chip->chip_prim (lambda (x) (cadr x)))

(define comp-chip->in (lambda (x) (cadr x)))

(define comp-chip->out (lambda (x) (caddr x)))

(define comp-chip->circ (lambda (x) (cadddr x)))

;; Area del programador

(define b (comp-chip '(INA INB INC IND)
           '(OUTA)
           (complex-circuit (simple-circuit '(a b) '(e) (prim-chip (chip-and)))
                            (list (simple-circuit '(c d) '(f) (prim-chip (chip-and)))
                                  (simple-circuit '(e f) '(g) (prim-chip (chip-or))))
                            '(a b c d)
                            '(g))))

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
