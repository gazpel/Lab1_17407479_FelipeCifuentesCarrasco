#lang racket

;; ============================================================
;; TDA Card (Carta)
;; ============================================================
;;
;; Representación: Lista con estructura variable
;; según el tipo de carta.
;;
;; Carta Pokémon:
;; (list 'card 'pokemon nombre evoluciona-de PS tipo-elem
;;       debilidad resistencia costo-retirada es-EX
;;       habilidad ataques)
;; Índices: 0:'card 1:'pokemon 2:nombre 3:evoluciona-de
;;          4:PS 5:tipo 6:debilidad 7:resistencia
;;          8:costo-retirada 9:es-EX 10:habilidad 11:ataques
;;
;; Carta Energía:
;; (list 'card 'energy nombre tipo-energia)
;; Índices: 0:'card 1:'energy 2:nombre 3:tipo-energia
;;
;; Carta Entrenador:
;; (list 'card 'trainer nombre subtipo texto funcion-accion)
;; Índices: 0:'card 1:'trainer 2:nombre 3:subtipo
;;          4:texto 5:funcion-accion
;;
;; ============================================================

(require "base_17407479_CifuentesCarrasco.rkt")
(require "TDAataque_17407479_CifuentesCarrasco.rkt")

(provide card
         card?
         card-type
         card-name
         ;; Funciones de pertenencia
         card-pokemon?
         card-energy?
         card-trainer?
         card-basic-pokemon?
         card-evolution-pokemon?
         ;; Selectores Pokémon
         card-evolves-from
         card-hp
         card-pokemon-type
         card-weakness
         card-resistance
         card-retreat-cost
         card-is-ex?
         card-ability
         card-attacks
         ;; Selectores Energía
         card-energy-type
         ;; Selectores Entrenador
         card-trainer-type
         card-text
         card-trainer-action)

;; ============================================================
;; TDA CARD - Constructor
;; ============================================================

; Descripción: Crea una carta del juego Pokémon TCG. Usa parámetros
;              n-variables para soportar tres tipos de carta:
;              - Pokémon: tipo, nombre, evoluciona-de, PS, tipo-elem,
;                debilidad, resistencia, costo-retirada, es-EX,
;                habilidad, lista-ataques
;              - Energía: tipo, nombre, tipo-energía
;              - Entrenador: tipo, nombre, subtipo, texto, función-acción
; Dom: tipo (CARD-TYPE) X args... (parámetros variables según tipo)
; Rec: card
; Tipo recursión: No aplica
(define (card tipo . args)
  (cond
    ;; --- Carta Pokémon (10 args adicionales) ---
    [(eq? tipo 'pokemon)
     (let ([nombre        (list-ref args 0)]
           [evoluciona-de (list-ref args 1)]
           [ps            (list-ref args 2)]
           [tipo-elem     (list-ref args 3)]
           [debilidad     (list-ref args 4)]
           [resistencia   (list-ref args 5)]
           [costo-ret     (list-ref args 6)]
           [es-ex         (list-ref args 7)]
           [habilidad     (list-ref args 8)]
           [ataques       (list-ref args 9)])
       (list 'card 'pokemon nombre evoluciona-de ps
             tipo-elem debilidad resistencia costo-ret
             es-ex habilidad ataques))]
    ;; --- Carta Energía (2 args adicionales) ---
    [(eq? tipo 'energy)
     (let ([nombre     (list-ref args 0)]
           [tipo-ener  (list-ref args 1)])
       (list 'card 'energy nombre tipo-ener))]
    ;; --- Carta Entrenador (4 args adicionales) ---
    [(eq? tipo 'trainer)
     (let ([nombre   (list-ref args 0)]
           [subtipo  (list-ref args 1)]
           [texto    (list-ref args 2)]
           [accion   (list-ref args 3)])
       (list 'card 'trainer nombre subtipo texto accion))]
    [else
     (error "card: tipo de carta no válido" tipo)]))

;; ============================================================
;; TDA CARD - Funciones de Pertenencia
;; ============================================================

; Descripción: Verifica si un elemento es una carta válida.
; Dom: x (any)
; Rec: boolean
; Tipo recursión: No aplica
(define (card? x)
  (and (list? x)
       (not (null? x))
       (eq? (car x) 'card)))

; Descripción: Verifica si una carta es de tipo Pokémon.
; Dom: c (card)
; Rec: boolean
; Tipo recursión: No aplica
(define (card-pokemon? c)
  (and (card? c)
       (eq? (list-ref c 1) 'pokemon)))

; Descripción: Verifica si una carta es de tipo energía.
; Dom: c (card)
; Rec: boolean
; Tipo recursión: No aplica
(define (card-energy? c)
  (and (card? c)
       (eq? (list-ref c 1) 'energy)))

; Descripción: Verifica si una carta es de tipo entrenador.
; Dom: c (card)
; Rec: boolean
; Tipo recursión: No aplica
(define (card-trainer? c)
  (and (card? c)
       (eq? (list-ref c 1) 'trainer)))

; Descripción: Verifica si una carta Pokémon es básica
;              (no evoluciona de ningún otro Pokémon).
; Dom: c (card)
; Rec: boolean
; Tipo recursión: No aplica
(define (card-basic-pokemon? c)
  (and (card-pokemon? c)
       (null? (list-ref c 3))))

; Descripción: Verifica si una carta Pokémon es de evolución
;              (evoluciona de otro Pokémon).
; Dom: c (card)
; Rec: boolean
; Tipo recursión: No aplica
(define (card-evolution-pokemon? c)
  (and (card-pokemon? c)
       (not (null? (list-ref c 3)))))

;; ============================================================
;; TDA CARD - Selectores Comunes
;; ============================================================

; Descripción: Obtiene el tipo de carta (pokemon, energy, trainer).
; Dom: c (card)
; Rec: tipo (CARD-TYPE)
; Tipo recursión: No aplica
(define (card-type c)
  (list-ref c 1))

; Descripción: Obtiene el nombre de la carta.
; Dom: c (card)
; Rec: nombre (string)
; Tipo recursión: No aplica
(define (card-name c)
  (list-ref c 2))

;; ============================================================
;; TDA CARD - Selectores Pokémon
;; ============================================================

; Descripción: Obtiene el nombre del Pokémon del cual evoluciona.
;              Retorna null si es un Pokémon básico.
; Dom: c (card) ; debe ser card-pokemon?
; Rec: evoluciona-de (string o null)
; Tipo recursión: No aplica
(define (card-evolves-from c)
  (list-ref c 3))

; Descripción: Obtiene los puntos de salud (PS) del Pokémon.
; Dom: c (card) ; debe ser card-pokemon?
; Rec: PS (int+)
; Tipo recursión: No aplica
(define (card-hp c)
  (list-ref c 4))

; Descripción: Obtiene el tipo elemental del Pokémon.
; Dom: c (card) ; debe ser card-pokemon?
; Rec: tipo (ELEMENT-TYPE)
; Tipo recursión: No aplica
(define (card-pokemon-type c)
  (list-ref c 5))

; Descripción: Obtiene el tipo de debilidad del Pokémon.
;              Retorna null si no tiene debilidad.
; Dom: c (card) ; debe ser card-pokemon?
; Rec: debilidad (ELEMENT-TYPE o null)
; Tipo recursión: No aplica
(define (card-weakness c)
  (list-ref c 6))

; Descripción: Obtiene el tipo de resistencia del Pokémon.
;              Retorna null si no tiene resistencia.
; Dom: c (card) ; debe ser card-pokemon?
; Rec: resistencia (ELEMENT-TYPE o null)
; Tipo recursión: No aplica
(define (card-resistance c)
  (list-ref c 7))

; Descripción: Obtiene el costo de retirada del Pokémon
;              (cantidad de energías incoloras necesarias).
; Dom: c (card) ; debe ser card-pokemon?
; Rec: costo (int >= 0)
; Tipo recursión: No aplica
(define (card-retreat-cost c)
  (list-ref c 8))

; Descripción: Verifica si el Pokémon es de tipo EX.
;              Los Pokémon EX otorgan 2 premios al ser derrotados.
; Dom: c (card) ; debe ser card-pokemon?
; Rec: boolean
; Tipo recursión: No aplica
(define (card-is-ex? c)
  (list-ref c 9))

; Descripción: Obtiene la habilidad del Pokémon.
;              Retorna null si no tiene habilidad.
; Dom: c (card) ; debe ser card-pokemon?
; Rec: habilidad (ability o null)
; Tipo recursión: No aplica
(define (card-ability c)
  (list-ref c 10))

; Descripción: Obtiene la lista de ataques del Pokémon.
; Dom: c (card) ; debe ser card-pokemon?
; Rec: ataques (List attack)
; Tipo recursión: No aplica
(define (card-attacks c)
  (list-ref c 11))

;; ============================================================
;; TDA CARD - Selectores Energía
;; ============================================================

; Descripción: Obtiene el tipo elemental de la carta de energía.
; Dom: c (card) ; debe ser card-energy?
; Rec: tipo (ELEMENT-TYPE)
; Tipo recursión: No aplica
(define (card-energy-type c)
  (list-ref c 3))

;; ============================================================
;; TDA CARD - Selectores Entrenador
;; ============================================================

; Descripción: Obtiene el subtipo de la carta de entrenador
;              ("objeto" o "partidario").
; Dom: c (card) ; debe ser card-trainer?
; Rec: subtipo (string)
; Tipo recursión: No aplica
(define (card-trainer-type c)
  (list-ref c 3))

; Descripción: Obtiene el texto descriptivo de la carta de entrenador.
; Dom: c (card) ; debe ser card-trainer?
; Rec: texto (string)
; Tipo recursión: No aplica
(define (card-text c)
  (list-ref c 4))

; Descripción: Obtiene la función de acción de la carta de entrenador.
; Dom: c (card) ; debe ser card-trainer?
; Rec: funcion-accion (function)
; Tipo recursión: No aplica
(define (card-trainer-action c)
  (list-ref c 5))
