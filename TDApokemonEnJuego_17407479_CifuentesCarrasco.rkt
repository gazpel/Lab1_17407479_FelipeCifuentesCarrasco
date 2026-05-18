#lang racket

;; ============================================================
;; TDA PokémonEnJuego
;; ============================================================
;; Representación: Lista
;; (list 'poke-in-play carta energias daño pila-evolucion
;;       estado turno-colocado id)
;; Índices: 0:tag 1:carta 2:energias 3:daño
;;          4:pila-evolucion 5:estado 6:turno-colocado 7:id
;; ============================================================

(require "base_17407479_CifuentesCarrasco.rkt")
(require "TDAataque_17407479_CifuentesCarrasco.rkt")
(require "TDAcarta_17407479_CifuentesCarrasco.rkt")

(provide make-pokemon-in-play
         pokemon-in-play?
         pip-card pip-energies pip-damage pip-evolutions
         pip-status pip-turn-placed pip-id
         pip-name pip-hp-actual pip-pokemon-type
         pip-set-damage pip-add-energy pip-set-status
         pip-evolve pip-set-energies
         pip-matches-card?)

;; ============================================================
;; Constructor
;; ============================================================

;; Descripción: Crea un Pokémon en juego a partir de una carta
;;              Pokémon. Inicia con 0 daño, sin energías, estado
;;              normal y un ID único.
;; Dom: carta (card pokemon) X turno (int) X id (int)
;; Rec: pokemon-in-play
;; Tipo recursión: No aplica
(define (make-pokemon-in-play carta turno id)
  (list 'poke-in-play carta '() 0 '() 'normal turno id))

;; ============================================================
;; Función de Pertenencia
;; ============================================================

;; Descripción: Verifica si un elemento es un Pokémon en juego.
;; Dom: x (any)
;; Rec: boolean
;; Tipo recursión: No aplica
(define (pokemon-in-play? x)
  (and (list? x) (not (null? x)) (eq? (car x) 'poke-in-play)))

;; ============================================================
;; Selectores
;; ============================================================

;; Descripción: Obtiene la carta Pokémon actual (la evolución más reciente).
;; Dom: p (pokemon-in-play)
;; Rec: carta (card)
;; Tipo recursión: No aplica
(define (pip-card p) (list-ref p 1))

;; Descripción: Obtiene las energías adjuntadas.
;; Dom: p (pokemon-in-play)
;; Rec: energias (List card-energy)
;; Tipo recursión: No aplica
(define (pip-energies p) (list-ref p 2))

;; Descripción: Obtiene el daño acumulado.
;; Dom: p (pokemon-in-play)
;; Rec: daño (int >= 0)
;; Tipo recursión: No aplica
(define (pip-damage p) (list-ref p 3))

;; Descripción: Obtiene la pila de evoluciones previas.
;; Dom: p (pokemon-in-play)
;; Rec: evoluciones (List card)
;; Tipo recursión: No aplica
(define (pip-evolutions p) (list-ref p 4))

;; Descripción: Obtiene el estado actual del Pokémon.
;; Dom: p (pokemon-in-play)
;; Rec: estado (symbol: normal|asleep|confused|paralyzed|poisoned)
;; Tipo recursión: No aplica
(define (pip-status p) (list-ref p 5))

;; Descripción: Obtiene el turno en que fue colocado en juego.
;; Dom: p (pokemon-in-play)
;; Rec: turno (int)
;; Tipo recursión: No aplica
(define (pip-turn-placed p) (list-ref p 6))

;; Descripción: Obtiene el ID único del Pokémon en juego.
;; Dom: p (pokemon-in-play)
;; Rec: id (int)
;; Tipo recursión: No aplica
(define (pip-id p) (list-ref p 7))

;; Selectores derivados

;; Descripción: Obtiene el nombre del Pokémon en juego.
;; Dom: p (pokemon-in-play)
;; Rec: nombre (string)
;; Tipo recursión: No aplica
(define (pip-name p) (card-name (pip-card p)))

;; Descripción: Obtiene los PS actuales (PS máx - daño).
;; Dom: p (pokemon-in-play)
;; Rec: ps-actuales (int)
;; Tipo recursión: No aplica
(define (pip-hp-actual p)
  (- (card-hp (pip-card p)) (pip-damage p)))

;; Descripción: Obtiene el tipo elemental del Pokémon.
;; Dom: p (pokemon-in-play)
;; Rec: tipo (ELEMENT-TYPE)
;; Tipo recursión: No aplica
(define (pip-pokemon-type p) (card-pokemon-type (pip-card p)))

;; ============================================================
;; Modificadores
;; ============================================================

;; Descripción: Establece el daño del Pokémon en juego.
;; Dom: p (pokemon-in-play) X nuevo-daño (int >= 0)
;; Rec: pokemon-in-play
;; Tipo recursión: No aplica
(define (pip-set-damage p nuevo-daño)
  (list 'poke-in-play (pip-card p) (pip-energies p) nuevo-daño
        (pip-evolutions p) (pip-status p) (pip-turn-placed p) (pip-id p)))

;; Descripción: Agrega una carta de energía al Pokémon.
;; Dom: p (pokemon-in-play) X energia (card-energy)
;; Rec: pokemon-in-play
;; Tipo recursión: No aplica
(define (pip-add-energy p energia)
  (list 'poke-in-play (pip-card p) (append (pip-energies p) (list energia))
        (pip-damage p) (pip-evolutions p) (pip-status p)
        (pip-turn-placed p) (pip-id p)))

;; Descripción: Establece las energías del Pokémon.
;; Dom: p (pokemon-in-play) X energias (List card-energy)
;; Rec: pokemon-in-play
;; Tipo recursión: No aplica
(define (pip-set-energies p energias)
  (list 'poke-in-play (pip-card p) energias (pip-damage p)
        (pip-evolutions p) (pip-status p) (pip-turn-placed p) (pip-id p)))

;; Descripción: Establece el estado del Pokémon.
;; Dom: p (pokemon-in-play) X estado (symbol)
;; Rec: pokemon-in-play
;; Tipo recursión: No aplica
(define (pip-set-status p estado)
  (list 'poke-in-play (pip-card p) (pip-energies p) (pip-damage p)
        (pip-evolutions p) estado (pip-turn-placed p) (pip-id p)))

;; Descripción: Evoluciona el Pokémon colocando una nueva carta
;;              encima. Hereda energías y daño, cura estados.
;; Dom: p (pokemon-in-play) X carta-evo (card pokemon)
;; Rec: pokemon-in-play
;; Tipo recursión: No aplica
(define (pip-evolve p carta-evo)
  (list 'poke-in-play carta-evo (pip-energies p) (pip-damage p)
        (cons (pip-card p) (pip-evolutions p)) 'normal
        (pip-turn-placed p) (pip-id p)))

;; ============================================================
;; Otras funciones
;; ============================================================

;; Descripción: Verifica si un Pokémon en juego corresponde a
;;              una carta dada (compara por nombre).
;; Dom: p (pokemon-in-play) X carta (card)
;; Rec: boolean
;; Tipo recursión: No aplica
(define (pip-matches-card? p carta)
  (string=? (pip-name p) (card-name carta)))
