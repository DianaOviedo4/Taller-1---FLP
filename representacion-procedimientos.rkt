#lang eopl
;Autores: Diana Oviedo 202459375, Juan Pablo Ospina 202411023, Carlos Gutierrez 202059817
; La representacion por procedimientos es una especie de empaquetamiento
; y devuelve un lambda en vez de lista

#| <circuito> := circ_simple({cable}∗)
                        ({ cable }∗)
                        <chip>
              simple−circuit( in  out chip )
              := circ_comp<circuito> {<circuito>}+
                                        input { cable }∗
                                        output { cable }∗
              complex−circuit(circ lcircs in out )|#
; Proposito :
; Definir un TAD (Tipo abstracto de dato) basado en la representacion por procedimientos de un circuito
; el cual puede ser simple o compuesto.

; CONSTRUIR INTERFAZ : PROCEDIMIENTOS:
; CONSTRUCTORES : Nos permite crear instancias de los circuitos con in, out, chips y subcircuitos (para los 
; complejos).

(define simple-circuit
      (lambda (in out chip)
        (lambda (s) ; se define un selector o señal
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

; OBSERVADORES - PREDICADOS : Verificar si el circuito simple y complejo tienen los componentes esperados.
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

; OBSERVADORES - EXTRACTORES : Nos permite acceder directamente a los diferentes componentes del circuito
; ya sea simple o compuesto.
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

; ===============================================================================================================

#|<chip> := <chip_prim>
         prim−chip ( chip−prim )

         := chip ( −−> {( port ) }∗)
                 ( <−− {( port ) }∗)
                 <circuito>
          comp−chip ( in , out , circ )
|#

; Proposito :
; Definir un TAD (Tipo abstracto de dato) basado en la representacion por procedimientos de un chip
; el cual puede ser primitivo o compuesto.
;
; CONSTRUIR INTERFAZ : PROCEDIMIENTOS:
; CONSTRUCTORES : Nos permite crear instancias de los chips, ya sean primitivos o compuestos.

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


; OBSERVADORES - PREDICADOS : Verificar si el chip simple y complejo tienen los componentes esperados.

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

; OBSERVADORES - EXTRACTORES : Nos permite acceder directamente a los diferentes componentes del chip
; ya sea primitivo o compuesto.
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

; Proposito :
; Definir un TAD (Tipo abstracto de dato) basado en la representacion por procedimientos de un chip primitivo.

; CONSTRUIR INTERFAZ : PROCEDIMIENTOS:
; CONSTRUCTORES : Nos permite crear instancias de los chips primitivos.

(define chip-or 
    (lambda ()
      (lambda (s); s- Es una senal o selector 
        (cond
          [(equal? s 'or) 'chip-or]
          [(= s 1) '()] ; sobra
          [else (eopl:error 'chip-or "No Existe el selector")]
          )
      )
    )
)

(define chip-and
    (lambda ()
      (lambda (s) ; s- Es una senal o selector 
        (cond
          [(equal? s 'and) 'chip-and]
          [else (eopl:error 'chip-and "No Existe el selector")]
          ) 
    )
  )
)


(define chip-not
  (lambda ()
    (lambda(s)
      (cond 
        [(equal? s 'not) 'chip-not]
        [else (eopl:error 'chip-not "No Existe el selector")]
        )
    )
  )
)

(define chip-xor
  (lambda ()
    (lambda(s)
      (cond
      [(equal? s 'xor) 'chip-xor]
      [else (eopl:error 'chip-xor "No Existe el selector")]
      )
    )
  )
)


(define chip-nand
  (lambda ()
    (lambda(s)
      (cond
      [(equal? s 'nand) 'chip-nand]
      [else (eopl:error 'chip-nand "No existe el selector")]
      )
    )
  )
)


(define chip-nor
  (lambda ()
    (lambda(s)
      (cond
      [(equal? s 'nor) 'chip-nor]
      [else (eopl:error 'chip-nor "No existe el selector")]
      )
    )
  )
)


(define chip-xnor
  (lambda ()
    (lambda(s)
      (cond
      [(equal? s 'xnor) 'chip-xnor]
      [else (eopl:error 'chip-xnor "NO existe el selector")]
      )
    )
  )
)

; OBSERVADORES - PREDICADOS : Verificar si los chips primitivos tienen los componentes esperados.

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
; =========================================================================================================
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

(define d (comp-chip '(INA INB INC IND)
           '(OUTC)
           (complex-circuit (simple-circuit '(m n) '(o) (prim-chip (chip-nand)))
                            (list (simple-circuit '(p q) '(r) (prim-chip (chip-xnor)))
                                  (simple-circuit '(o r) '(s) (prim-chip (chip-or))))
                            '(m n p q)
                            '(s))))
; ===========================================================================================================
(define e
  (comp-chip '(IN1 IN2 IN3)
              '(OUT1 OUT2)
              (complex-circuit (simple-circuit '(p q) '(r) (prim-chip (chip-and)))
                               (list (simple-circuit '(s t) '(u) (prim-chip (chip-nor)))
                                  (simple-circuit '(v w) '(x) (prim-chip (chip-xor))))
                               '(p q s t v w)
                               '(r u x))))
; =========================================================================================================================
(define f
        (complex-circuit (simple-circuit '(a b) '(c) (prim-chip (chip-or)))
                               (list(simple-circuit '(d e) '(f) (prim-chip (chip-and)))
                                (simple-circuit '(g h) '(i) (prim-chip (chip-xnor))))
                               '(a b d e g h)
                               '(c f i)))

; =========================================================================================================================
(define g
  (simple-circuit '(x y) '(z) (prim-chip (chip-xor))))
