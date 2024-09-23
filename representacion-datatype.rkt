#lang eopl
;Autores: Diana Oviedo 202459375, Juan Pablo Ospina 202411023, Carlos Gutierrez 202059817

#| <circuito> := circ_simple({cable}∗)
                        ({ cable }∗)
                        <chip>
              simple−circuit( in  out chip )
              := circ_comp<circuito> {<circuito>}+
                                        input { cable }∗
                                        output { cable }∗
              complex−circuit(circ lcircs in out )|#

; Proposito
; Crear un TAD (tipo abstracto de datos) basado en la representacion por datatypes para los circuitos
; los cuales pueden ser simples o compuestos segun la gramatica anterior.

(define-datatype circuito circuito?
      (simple-circuit                
        (cableIn (list-of symbol?)) ;nombre-variante(id(cualquierCosa)predicado)
        (cableOut (list-of symbol?))
        (chip chip?)
        )
      (complex-circuit
        (circ circuito?)
        (lcircs (list-of circuito?))
        (in (list-of symbol?))
        (out(list-of symbol?))
        )
)

#|(define imprimeCircuito
  (lambda (circuitoSC)
    (cases circuito circuitoSC 
      (simple-circuit (chip cableIn cableOut) 
              "Circuito simple:\n- Chip: ~a\n- Entrada: ~a\n- Salida: ~a\n" 
              chip cableIn cableOut)
      (complex-circuit (circ lcircs in out)
              "Circuito complejo:\n- Subcircuito: ~a\n- Lista de circuitos: ~a\n- Entrada: ~a\n- Salida: ~a\n"
              circ lcircs in out)
    (else eopl:error "Esto es un error del CIRC"))
  )
)|#

; ===============================================================================================================
#|<chip> := <chip_prim>
         prim−chip ( chip−prim )

         := chip ( −−> {( port ) }∗)
                 ( <−− {( port ) }∗)
                  <circuito>
         comp−chip ( in , out , circ )
|#
; Proposito :
; Crear un TAD (tipo abstracto de datos) basado en la representacion por datatypes para los chips
; los cuales pueden ser primitivos o compuestos segun la gramatica anterior.

(define-datatype chip chip?
        (prim-chip
          (chip-prim chip_prim?))
        (comp-chip
          (portIn (list-of symbol?))
          (portOut (list-of symbol?))
          (circuito circuito?)
          )
)

; ===============================================================================================================
#|<chip prim> := prim_or
                chip−or ( )
              := prim and
                chip−and ( )
              := prim not
                chip−not ( )
              := prim xor
                chip−xor ( )
              := prim nand
                chip−nand ( )
              := prim nor
                chip−nor ( )
              := prim xnor 
                chip−xnor ( ) 
|#

; Proposito 
; Crear un TAD (tipo abstracto de datos) basado en la representacion por datatypes para los chips primitivos.

#|(define-datatype chipPrim chip_prim?
          (chip-or
           (prim_or symbol?))
          (chip-and
            (prim_and symbol?))
          (chip-not
            (prim_and symbol?))
          (chip-xor
            (prim_xor symbol?))
          (chip-nand
            (prim_nand symbol?))
          (chip-nor
            (prim_nor symbol?))
          (chip-xnor
            (prim_xnor symbol?))
)|#

(define-datatype chip_prim chip_prim?
  (chip-or)
  (chip-and)
  (chip-not)
  (chip-xor)
  (chip-nand)
  (chip-nor)
  (chip-xnor))


; ====================================== AREA DEL PROGRAMADOR =======================================
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


; ========================================================================================================================
(define d (comp-chip '(INA INB INC IND)
           '(OUTC)
           (complex-circuit (simple-circuit '(m n) '(o) (prim-chip (chip-nand)))
                            (list (simple-circuit '(p q) '(r) (prim-chip (chip-xnor)))
                                  (simple-circuit '(o r) '(s) (prim-chip (chip-or))))
                            '(m n p q)
                            '(s))))

(define c (complex-circuit
  (simple-circuit '(a b c d)
                  '(x y)
                  (comp-chip '(INL INM INN INO)
                             '(OUTP OUTQ)
                             (complex-circuit
                              (simple-circuit '(e f) '(g) (prim-chip (chip-nor)))
                              (list (simple-circuit '(h i) '(j) (prim-chip (chip-and))))
                              '(e f h i)
                              '(g j))))
  (list (simple-circuit '(x y)
                        '(z)
                        (comp-chip '(INI INJ) '(OUTI) (simple-circuit '(x y) '(z) (prim-chip (chip-xor))))))
  '(a b c d)
  '(z)))

; ========================================================================================================================
(define e
  (complex-circuit
    (simple-circuit '(a b) '(c) (prim-chip (chip-and)))
    (list (simple-circuit '(c d) '(e) (prim-chip (chip-or))))
    '(a b d)
    '(e)))

; =========================================================================================================================
(define f
  (simple-circuit '(x y) '(z) (prim-chip (chip-xor))))

; ========================================================================================================================
(define g
  '(comp-chip (IN1 IN2 IN3)
              (OUT1 OUT2)
              (complex-circuit (simple-circuit (p q) (r) (prim-chip (chip-and)))
                               ((simple-circuit (s t) (u) (prim-chip (chip-nor)))
                                (simple-circuit (v w) (x) (prim-chip (chip-xor))))
                               (p q s t v w)
                               (r u x))))
; =========================================================================================================================
(define h
  '(comp-chip (IN1 IN2 IN3 IN4)
              (OUT1 OUT2)
              (complex-circuit (simple-circuit (a b) (c) (prim-chip (chip-or)))
                               ((simple-circuit (d e) (f) (prim-chip (chip-and)))
                                (simple-circuit (g h) (i) (prim-chip (chip-xnor))))
                               (a b d e g h)
                               (c f i))))


;; Pruebas
(display a)
(newline)
(display "Fin Ejemplo a")
(newline)
(display b)
(newline)
(display "Fin Ejemplo b")
(newline)
(display c)
(newline)
(display "Fin Ejemplo c")
(newline)
(display d)
(newline)
(display "Fin Ejemplo d")
(newline)
(display e)
(newline)
(display "Fin Ejemplo e")
(newline)
(display f)
(newline)
(display "Fin Ejemplo f")
(newline)
(display g)
(newline)
(display "Fin Ejemplo g")
(newline)
(display h)
(newline)
(display "Fin Ejemplo h")
(newline)


(provide circuito chip chip_prim simple-circuit complex-circuit prim-chip comp-chip chip-or chip-and chip-not chip-xor chip-nand chip-nor chip-xnor
        circuito? chip? chip_prim? circuito? )
