#lang racket

;; ============================================================
;; Descripción: Código base proporcionado en el
;;              enunciado del laboratorio.
;; ============================================================

(provide CARD-TYPE
         card-type?
         ELEMENT-TYPE
         element-type?
         ENERGY)

;; ============================================================
;; ENUMERACIÓN DE TIPOS DE CARTA
;; ============================================================

;; Descripción: Lista de tipos de carta válidos en el juego.
;; Representación: Lista de símbolos.
(define CARD-TYPE '(pokemon energy trainer))

;; Descripción: Verifica si un símbolo es un tipo de carta válido.
;; Dom: t (symbol)
;; Rec: boolean
;; Tipo recursión: No aplica
(define (card-type? t)
  (and (symbol? t)
       (member t CARD-TYPE)
       #t))

;; ============================================================
;; ENUMERACIÓN DE TIPOS ELEMENTALES
;; ============================================================

;; Descripción: Lista de tipos elementales válidos en el juego.
;; Representación: Lista de símbolos.
(define ELEMENT-TYPE '(grass fire water lightning psychic fighting darkness metal colorless fairy))

;; Descripción: Verifica si un símbolo es un tipo elemental válido.
;; Dom: e (symbol)
;; Rec: boolean
;; Tipo recursión: No aplica
(define (element-type? e)
  (and (symbol? e)
       (member e ELEMENT-TYPE)
       #t))

;; ============================================================
;; LISTA DE ENERGÍAS
;; ============================================================

;; Descripción: Lista asociativa de nombres de energía con su tipo elemental.
;; Representación: Lista de pares (nombre . tipo).
(define ENERGY
  '((fire-energy      . fire)
    (water-energy     . water)
    (grass-energy     . grass)
    (lightning-energy . lightning)
    (psychic-energy   . psychic)
    (fighting-energy  . fighting)
    (darkness-energy  . darkness)
    (metal-energy     . metal)
    (fairy-energy     . fairy)
    (colorless-energy . colorless)))
