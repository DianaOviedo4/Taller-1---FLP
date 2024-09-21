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

;; Importar el archivo representacion-datatype.rkt
(require rackunit "representacion-datatype.rkt")

;; Parser para el lenguaje de programación de circuitos digitales LCD
;; Proposito: Convertir una expresión de listas a su representación abstracta (AST)
(define parser
  (lambda (exp)
    (cond
      [(eqv? (car exp) 'simple-circuit) (simple-circuit (cadr exp) (caddr exp) (parser (cadddr exp)))]
      [(eqv? (car exp) 'complex-circuit)
       (complex-circuit (parser (cadr exp)) (map parser (caddr exp)) (cadddr exp) (cadddr (cdr exp)))]
      [(eqv? (car exp) 'prim-chip) (prim-chip (parser (cadr exp)))]
      [(eqv? (car exp) 'comp-chip) (comp-chip (cadr exp) (caddr exp) (parser (cadddr exp)))]
      [(eqv? (car exp) 'chip-or) (chip-or)]
      [(eqv? (car exp) 'chip-and) (chip-and)]
      [(eqv? (car exp) 'chip-not) (chip-not)]
      [(eqv? (car exp) 'chip-xor) (chip-xor)]
      [(eqv? (car exp) 'chip-nand) (chip-nand)]
      [(eqv? (car exp) 'chip-nor) (chip-nor)]
      [(eqv? (car exp) 'chip-xnor) (chip-xnor)])))

;; Unparser para el lenguaje de programación de circuitos digitales LCD
;; Proposito: Convertir una expresión de la representación abstracta a su sintaxis concreta (listas)
(define unparse
  (lambda (exp)
    (cond
      [(circuito? exp)
       (cases circuito
              exp
              (simple-circuit (in out chip) (list 'simple-circuit in out (unparse chip)))
              (complex-circuit (circ lcircs in out)
                               (list 'complex-circuit (unparse circ) (map unparse lcircs) in out)))]

      [(chip? exp)
       (cases chip
              exp
              (prim-chip (chip_prim) (list 'prim-chip (unparse chip_prim)))
              (comp-chip (in out circ) (list 'comp-chip in out (unparse circ))))]

      [(chip_prim? exp)
       (cases chip_prim
              exp
              (chip-or () (list 'chip-or))
              (chip-and () (list 'chip-and))
              (chip-not () (list 'chip-not))
              (chip-xor () (list 'chip-xor))
              (chip-nand () (list 'chip-nand))
              (chip-nor () (list 'chip-nor))
              (chip-xnor () (list 'chip-xnor)))]

      [else (eopl:error "unparse: no se encuentra el caso" exp)])))
; ====================================== AREA DEL PROGRAMADOR =======================================
; Ejemplo de uso de los parsers y unparsers
(define a
  '(complex-circuit
    (simple-circuit (m n o p)
                    (e f)
                    (comp-chip (INA INB INC IND)
                               (OUTD OUTF)
                               (complex-circuit (simple-circuit (a b) (e) (prim-chip (chip-and)))
                                                ((simple-circuit (c d) (f) (prim-chip (chip-and))))
                                                (a b c d)
                                                (e f))))
    ((simple-circuit (e f)
                     (z)
                     (comp-chip (INE INF) (OUTA) (simple-circuit (e f) (g) (prim-chip (chip-or))))))
    (m n o p)
    (z)))

(define b
  '(comp-chip (INA INB INC IND)
              (OUTA)
              (complex-circuit (simple-circuit (a b) (e) (prim-chip (chip-and)))
                               ((simple-circuit (c d) (f) (prim-chip (chip-and)))
                                (simple-circuit (e f) (g) (prim-chip (chip-or))))
                               (a b c d)
                               (g))))
(define c
  '(simple-circuit (x y z w)
                   (a b c)
                   (comp-chip (IN1 IN2 IN3)
                              (OUT1 OUT2)
                              (complex-circuit (simple-circuit (p q) (r) (prim-chip (chip-xor)))
                                               ((simple-circuit (s t) (u) (prim-chip (chip-nand))))
                                               (p q s t)
                                               (r u)))))

(define d
  '(complex-circuit (simple-circuit (a b c) (d e) (prim-chip (chip-nand)))
                    ((simple-circuit (f g) (h) (prim-chip (chip-nor)))
                     (simple-circuit (i j) (k) (prim-chip (chip-xnor))))
                    (a b c f g i j)
                    (d e h k)))

(define e
  '(comp-chip (IN1 IN2 IN3)
              (OUT1 OUT2)
              (complex-circuit (simple-circuit (x y) (z) (prim-chip (chip-xnor)))
                               ((simple-circuit (a b) (c) (prim-chip (chip-or)))
                                (simple-circuit (d e) (f) (prim-chip (chip-and))))
                               (x y a b d e)
                               (z c f))))

(define f
  '(simple-circuit (p q r s)
                   (t u v)
                   (comp-chip (INP INQ INR)
                              (OUTP OUTQ OUTR)
                              (complex-circuit (simple-circuit (w x) (y) (prim-chip (chip-or)))
                                               ((simple-circuit (z a) (b) (prim-chip (chip-not))))
                                               (w x z a)
                                               (y b)))))

(define g
  '(complex-circuit (simple-circuit (i j k) (l m) (prim-chip (chip-and)))
                    ((simple-circuit (n o) (p) (prim-chip (chip-not)))
                     (simple-circuit (q r) (s) (prim-chip (chip-xor)))
                     (simple-circuit (t u) (v) (prim-chip (chip-nand))))
                    (i j k n o q r t u)
                    (l m p s v)))

;; Pruebas
(display "-----------------Parsers-----------------")
(newline)
(display (parser a))
(newline)
(display "fin parser a")
(newline)
(display (parser b))
(newline)
(display "fin parser b")
(newline)
(display (parser c))
(newline)
(display "fin parser c")
(newline)
(display (parser d))
(newline)
(display "fin parser d")
(newline)
(display (parser e))
(newline)
(display "fin parser e")
(newline)
(display (parser f))
(newline)
(display "fin parser f")
(newline)
(display (parser g))
(newline)
(display "fin parser g")
(newline)
(display "-----------------Unparsers-----------------")
(newline)
(display (unparse (parser a)))
(newline)
(display "fin unparser a")
(newline)
(display (unparse (parser b)))
(newline)
(display "fin unparser b")
(newline)
(display (unparse (parser c)))
(newline)
(display "fin unparser c")
(newline)
(display (unparse (parser d)))
(newline)
(display "fin unparser d")
(newline)
(display (unparse (parser e)))
(newline)
(display "fin unparser e")
(newline)
(display (unparse (parser f)))
(newline)
(display "fin unparser f")
(newline)
(display (unparse (parser g)))
(newline)
(display "fin unparser g")
