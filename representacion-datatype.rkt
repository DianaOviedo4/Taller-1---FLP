#lang eopl
;Autores: Diana Oviedo 202459375, Juan Pablo Ospina 202411023

#| <circuito> := circ_simple({cable}∗)
                        ({ cable }∗)
                        <chip>
              simple−circuit( in  out chip )
              := circ_comp<circuito> {<circuito>}+
                                        input { cable }∗
                                        output { cable }∗
              complex−circuit(circ lcircs in out )|#

; CONSTRUCTORES Y PREDICADOS
;; Definir list-symbol?

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

; EXTRACTORES

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



#|<chip> := <chip_prim>
         prim−chip ( chip−prim )

         := chip ( −−> {( port ) }∗)
                 ( <−− {( port ) }∗)
                  <circuito>
         comp−chip ( in , out , circ )
|#

; CONSTRUCTORES Y PREDICADOS

(define-datatype chip chip?
        (prim-chip
          (chip-prim chip_prim?))
        (comp-chip
          (portIn (list-of symbol?))
          (portOut (list-of symbol?))
          (circuito circuito?)
          )
)

; EXTRACTORES

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

; CONSTRUCTORES
; OBSERVADORES - PREDICADOs
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

(provide circuito chip chip_prim simple-circuit complex-circuit prim-chip comp-chip chip-or chip-and chip-not chip-xor chip-nand chip-nor chip-xnor
        circuito? chip? chip_prim? circuito? )