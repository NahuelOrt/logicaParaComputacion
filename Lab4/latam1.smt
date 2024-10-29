; Script template de SMT-LIB para resolver el ejercicio 3.2

(set-logic QF_UF)
(set-option :produce-models true)

(declare-fun p () Bool)
; declarar variables proposicionales aquí

(define-fun formula () Bool
   ; insertar formalización aquí
)

(assert formula)
(check-sat)
(get-model)
