#lang racket

;; ============================================================
;; TDA Jugador
;; ============================================================
;; Representación: Lista
;; (list 'player id deck hand active bench prizes discard)
;; Índices: 0:tag 1:id 2:deck 3:hand 4:active
;;          5:bench 6:prizes 7:discard
;; ============================================================

(require "base_17407479_CifuentesCarrasco.rkt")
(require "TDAcarta_17407479_CifuentesCarrasco.rkt")
(require "TDApokemonEnJuego_17407479_CifuentesCarrasco.rkt")

(provide make-player
         player?
         player-id player-deck player-hand player-active
         player-bench player-prizes player-discard
         player-set-deck player-set-hand player-set-active
         player-set-bench player-set-prizes player-set-discard)

;; ============================================================
;; Constructor
;; ============================================================

;; Descripción: Crea un jugador con su estado inicial.
;; Dom: id (int) X deck (List card) X hand (List card) X
;;      active (pokemon-in-play o null) X bench (List pokemon-in-play) X
;;      prizes (List card) X discard (List card)
;; Rec: player
;; Tipo recursión: No aplica
(define (make-player id deck hand active bench prizes discard)
  (list 'player id deck hand active bench prizes discard))

;; ============================================================
;; Función de Pertenencia
;; ============================================================

;; Descripción: Verifica si un elemento es un jugador válido.
;; Dom: x (any)
;; Rec: boolean
;; Tipo recursión: No aplica
(define (player? x)
  (and (list? x) (not (null? x)) (eq? (car x) 'player)))

;; ============================================================
;; Selectores
;; ============================================================

;; Descripción: Obtiene el ID del jugador (1 o 2).
;; Dom: p (player)  ;  Rec: int
;; Tipo recursión: No aplica
(define (player-id p) (list-ref p 1))

;; Descripción: Obtiene las cartas restantes en el mazo.
;; Dom: p (player)  ;  Rec: List card
;; Tipo recursión: No aplica
(define (player-deck p) (list-ref p 2))

;; Descripción: Obtiene las cartas en la mano.
;; Dom: p (player)  ;  Rec: List card
;; Tipo recursión: No aplica
(define (player-hand p) (list-ref p 3))

;; Descripción: Obtiene el Pokémon activo (o null si no hay).
;; Dom: p (player)  ;  Rec: pokemon-in-play o null
;; Tipo recursión: No aplica
(define (player-active p) (list-ref p 4))

;; Descripción: Obtiene la banca de Pokémon en juego.
;; Dom: p (player)  ;  Rec: List pokemon-in-play
;; Tipo recursión: No aplica
(define (player-bench p) (list-ref p 5))

;; Descripción: Obtiene las cartas de premio.
;; Dom: p (player)  ;  Rec: List card
;; Tipo recursión: No aplica
(define (player-prizes p) (list-ref p 6))

;; Descripción: Obtiene la pila de descarte.
;; Dom: p (player)  ;  Rec: List card
;; Tipo recursión: No aplica
(define (player-discard p) (list-ref p 7))

;; ============================================================
;; Modificadores
;; ============================================================

;; Descripción: Modifica el mazo del jugador.
;; Dom: p (player) X nuevo-deck (List card)
;; Rec: player
;; Tipo recursión: No aplica
(define (player-set-deck p nuevo-deck)
  (make-player (player-id p) nuevo-deck (player-hand p) (player-active p)
               (player-bench p) (player-prizes p) (player-discard p)))

;; Descripción: Modifica la mano del jugador.
;; Dom: p (player) X nueva-mano (List card)
;; Rec: player
;; Tipo recursión: No aplica
(define (player-set-hand p nueva-mano)
  (make-player (player-id p) (player-deck p) nueva-mano (player-active p)
               (player-bench p) (player-prizes p) (player-discard p)))

;; Descripción: Modifica el Pokémon activo del jugador.
;; Dom: p (player) X nuevo-activo (pokemon-in-play o null)
;; Rec: player
;; Tipo recursión: No aplica
(define (player-set-active p nuevo-activo)
  (make-player (player-id p) (player-deck p) (player-hand p) nuevo-activo
               (player-bench p) (player-prizes p) (player-discard p)))

;; Descripción: Modifica la banca del jugador.
;; Dom: p (player) X nueva-banca (List pokemon-in-play)
;; Rec: player
;; Tipo recursión: No aplica
(define (player-set-bench p nueva-banca)
  (make-player (player-id p) (player-deck p) (player-hand p) (player-active p)
               nueva-banca (player-prizes p) (player-discard p)))

;; Descripción: Modifica las cartas de premio del jugador.
;; Dom: p (player) X nuevos-premios (List card)
;; Rec: player
;; Tipo recursión: No aplica
(define (player-set-prizes p nuevos-premios)
  (make-player (player-id p) (player-deck p) (player-hand p) (player-active p)
               (player-bench p) nuevos-premios (player-discard p)))

;; Descripción: Modifica la pila de descarte del jugador.
;; Dom: p (player) X nuevo-descarte (List card)
;; Rec: player
;; Tipo recursión: No aplica
(define (player-set-discard p nuevo-descarte)
  (make-player (player-id p) (player-deck p) (player-hand p) (player-active p)
               (player-bench p) (player-prizes p) nuevo-descarte))
