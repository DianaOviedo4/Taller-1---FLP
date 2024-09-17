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

(define-datatype circuito circuitoSC?
      (simple-circuit 
        (chip symbol?); nombre-variante(id(cualquierCosa) predicado)                 
        (cableIn (list-of symbol?))
        (cableOut (list-of symbol?))
        )
      (complex-circuito
        (circ circuitoSC?)
        (lcircs (list-of circuitoSC?))
        (in (list-of symbol?))
        (out(list-of symbol?))
        )
)

;; Ejemplos de circuitos
;(define circuito1 (simple-circuit 'chipA '(a b) '(out1 out2)))
;(define circuito2 (complex-circuito circuito1 (list circuito1) '(in1 in2) '(out1)))

;; Prueba de las instancias
;(display circuito1)
;(display circuito2)


; EXTRACTORES
#|(define (imprimeCircuitos circuito)
  (cases circuito circuitoSC
      (simple-circuit (chip cableIn cableOut) 
              printf "Circuito simple:\n- Chip: ~a\n- Entrada: ~a\n- Salida: ~a\n" 
              chip cableIn cableOut)
      (complex-circuito (circ lcircs in out)
              printf "Circuito complejo:\n- Subcircuito: ~a\n- Lista de circuitos: ~a\n- Entrada: ~a\n- Salida: ~a\n"
              circ lcircs in out)
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

(define-datatype chip chiPC?
        (prim-chip
          (chip-prim symbol?))
        (comp-chip
          (portIn (list-of symbol?))
          (portOut (list-of symbol?))
          (circuito circuitoSC?)
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
(define-datatype chipPrim chip_prim?
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
)

