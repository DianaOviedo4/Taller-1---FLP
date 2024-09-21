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


; Proposito: Construir un TAD basado en listas para la creación de circuitos
;            digitales LCD.
#lang eopl

;; Constructores
;; Proposito: Definir los constructores para el TAD circuito 

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
;; Proposito: Definir los observadores para el TAD circuito
;; Predicados
;; Proposito: Evaluar cada entrada para determinar si es un simple-circuito, complex-circuito, prim-chip, comp-chip, chip-or, chip-and, chip-not, chip-xor, chip-nand, chip-nor, chip-xnor
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
;; Proposito: Extraer los datosde cada circuito y chip
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
; Ejemplo de uso de los parsers y unparsers
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


(define c
      (simple-circuit '(x y) '(z) (prim-chip (chip-xor))))

(define d
      (complex-circuit
       (simple-circuit '(a b) '(c) (prim-chip (chip-nand)))
       (list (simple-circuit '(d e) '(f) (prim-chip (chip-nor))))
       '(a b d e)
       '(c f)))

(define e
      (comp-chip
       '(IN1 IN2)
       '(OUT1)
       (simple-circuit '(g h) '(i) (prim-chip (chip-xnor)))))

(define f
      (complex-circuit
       (simple-circuit '(j k) '(l) (prim-chip (chip-not)))
       (list (simple-circuit '(m n) '(o) (prim-chip (chip-or))))
       '(j k m n)
       '(l o)))

(define g
      (comp-chip
       '(INX INY)
       '(OUTZ)
       (complex-circuit
            (simple-circuit '(p q) '(r) (prim-chip (chip-and)))
            (list (simple-circuit '(s t) '(u) (prim-chip (chip-xor))))
            '(p q s t)
            '(r u))))