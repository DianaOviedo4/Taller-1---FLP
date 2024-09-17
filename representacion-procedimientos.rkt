#lang eopl
;Autores: Diana Oviedo 202459375, Juan Pablo Ospina 202411023
; La representacion por procedimie ntos es una especie de empaquetamiento
; y devuelve un lambda en vez de lista

#| <circuito> := circ_simple({cable}∗)
                        ({ cable }∗)
                        <chip>
              simple−circuit( in  out chip )
              := circ_comp<circuito> {<circuito>}+
                                        input { cable }∗
                                        output { cable }∗
              complex−circuit(circ lcircs in out )|#
; CONSTRUIR INTERFAZ : PROCEDIMIENTOS:
; CONSTRUCTORES
(define simple-circuit
      (lambda (in out chip)
        (lambda (s)
            (cond
              [(= s 0) in]
              [(= s 1) out]
              [(= s 2) chip]
              [(= s 3) 'simple-circuit]
              [else (eopl:error 'simple-circuit "No existe la senal SIMPLE_CIRCUIT")]
              )
          )
        ) 
  )

(define complex-circuit
      (lambda (circ lcircs in out)
        (lambda (s)
          (cond
            [(= s 0) circ]
            [(= s 1) lcircs]
            [(= s 2) in]
            [(= s 3) out]
            [(= s 4) 'complex-circuit]
            [else (eopl:error 'complex-circuit "No existe la senal COMPLEX_CIRCUIT")]
            )
          )
        )
  )

; OBSERVADORES - PREDICADOS
(define simple-circuit?
    (lambda(in out chip)
      (lambda (circuito) 
        (eq? (circuito 0) 'in)
        (eq? (circuito 1) 'out)
        (eq? (circuito 2) 'chip)
        (eq? (circuito 3) 'simple-circuit)
      )
    )
)


(define complex-circuit?
    (lambda (circ lcircs in out)
      (lambda(circuito)
        (eq? (circuito 0) 'circ)
        (eq? (circuito 1) 'lcircs)
        (eq? (circuito 2) 'in)
        (eq? (circuito 3) 'out)
        (eq? (circuito 4) 'complex-circuit)
      )
    )
)

; OBSERVADORES - EXTRACTORES
; CIRCUITO SIMPLE
(define simple-circuit->cableIn
      (lambda (circuito)
        (circuito 0))
)
(define simple-circuit->cableOut
      (lambda (circuito)
        (circuito 1))
)
(define simple-circuit->chip
      (lambda (circuito)
        (circuito 2))
)
; CIRCUITO COMPLEJO
(define complex-circuit->circ
      (lambda (circuito)
        (circuito 0))
)

(define complex-circuit->lcircs
      (lambda (circuito)
        (circuito 1))
)
(define complex-circuit->in
      (lambda (circuito)
        (circuito 2))
)
(define complex-circuit->out
      (lambda (circuito)
        (circuito 3))
)

#|<chip> := <chip_prim>
         prim−chip ( chip−prim )

         := chip ( −−> {( port ) }∗)
                 ( <−− {( port ) }∗)
                 <circuito>
          comp−chip ( in , out , circ )
|#

; CONSTRUIR INTERFAZ : PROCEDIMIENTOS:
; CONSTRUCTORES
(define prim-chip
    (lambda (chip-prim)
      (lambda (s)
        (cond
        [(eq? s 'prim) chip-prim]
        [else (eopl:error 'prim-chip "No existe la senal PRIM_CHIP")]
        )
      )
    )
)


(define comp-chip
    (lambda (in out circ)
      (lambda (s)
        (cond
         [(= s 0) in]
         [(= s 1) out]
         [(= s 2) circ]
         [(= s 3) 'comp-chip]
         [else (eopl:error 'comp-chip "No existe la senal COMP_CHIP")]
        )
      )
    )
)


; OBSERVADORES - PREDICADOS
(define prim-chip?
    (lambda (chip)
      (eq? ('prim chip) 'prim-chip )
      )
  )

(define comp-chip?
    (lambda (in out circ)
      (lambda (chip)
          (eq? (chip 0) 'in)
          (eq? (chip 1) 'out)
          (eq? (chip 2) 'circ)
          (eq? (chip 3) 'comp-chip)  
      )
    )
  )

; OBSERVADORES - EXTRACTORES
; CHIP PRIMITIVO
(define prim-chip->chip-prim
    (lambda (chip)
      (chip 'prim))
)
; CHIP COMPLEJO
(define comp-chip->in
    (lambda (chip)
      (chip 0))
)
(define comp-chip->out
    (lambda (chip)
      (chip 1))

)
(define comp-chip->circ
    (lambda (chip)
      (chip 2))
)

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

; CONSTRUIR INTERFAZ : PROCEDIMIENTOS:
; CONSTRUCTORES
(define chip-or 
    (lambda ()
      (lambda (s); s- Es una senal o selector 
        (cond
          [(equal? s 'or) 'chip-or]
          [(= s 1) '()] ; sobra
          [else (eopl:error 'chip-or "No Existe la senal")]
          )
      )
    )
)

(define chip-and
    (lambda ()
      (lambda (s) ; s- Es una senal o selector 
        (cond
          [(equal? s 'and) 'chip-and]
          [else (eopl:error 'chip-and "No Existe la senal")]
          ) 
    )
  )
)


(define chip-not
    (lambda(s)
      (cond 
        [(equal? s 'not) 'chip-not]
        [else (eopl:error 'chip-not "No Existe la senal")]
      )
    )
 )

(define chip-xor
    (lambda(s)
      (cond
      [(equal? s 'xor) 'chip-xor]
      [else (eopl:error 'chip-xor "No Existe la senal")]
      )
    )
)


(define chip-nand
    (lambda(s)
      (cond
      [(equal? s 'nand) 'chip-nand]
      [else (eopl:error 'chip-nand "No existe la senal")]
      )
    )
)


(define chip-nor
    (lambda(s)
      (cond
      [(equal? s 'nor) 'chip-nor]
      [else (eopl:error 'chip-nor "No existe la senal")]
      )
    )
)


(define chip-xnor
    (lambda(s)
      (cond
      [(equal? s 'xnor) 'chip-xnor]
      [else (eopl:error 'chip-xnor "NO existe la senal")]
      )
    )
)

; OBSERVADORES - PREDICADOS

(define chip-or?
    (lambda (chip_prim)
      (eq? (chip_prim 'or) 'chip-or)
    )
)

(define chip-and?
    (lambda(chip_prim)
    (eq? (chip_prim 'and) 'chip-and)
    )
)

(define chip-not?
    (lambda (chip_prim)
    (eq? (chip_prim 'not) 'chip-not)
    )
)


(define chip-xor?
    (lambda(chip_prim)
    (eq? (chip_prim 'xor) 'chip-xor)
    )
)


(define chip-nand?
    (lambda(chip_prim)
    (eq? (chip_prim 'nand) 'chip-nand)
    )
)


(define chip-nor?
    (lambda(chip_prim)
    (eq? (chip_prim 'nor)  'chip-nor)
    )
)

(define chip-xnor?
    (lambda(chip_prim)
    (eq? (chip_prim 'xnor)  'chip-xnor)
    )
)

; OBSERVADORES - EXTRACTORES : NO SE TIENE YA QUE EN LA GRAMATICA NO TENEMOS NINGUN DATO POR EXTRAER.

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
