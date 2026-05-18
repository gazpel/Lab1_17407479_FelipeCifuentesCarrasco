#lang racket

;; ============================================================
;; TDA Ataque (Attack) y TDA Habilidad (Ability)
;; ============================================================
;;
;; TDA Ataque
;; Representación: Lista
;; (list 'attack costo nombre descripcion daño funcion-accion)
;;
;; Donde:
;;   - costo: lista de símbolos de tipo de energía (ELEMENT-TYPE)
;;   - nombre: string con el nombre del ataque
;;   - descripcion: string con la descripción del efecto
;;   - daño: entero >= 0 (extraído automáticamente de la descripción)
;;   - funcion-accion: función (lambda (juego args) -> Juego)
;;
;; TDA Habilidad
;; Representación: Lista
;; (list 'ability nombre descripcion funcion-accion)
;;
;; Donde:
;;   - nombre: string con el nombre de la habilidad
;;   - descripcion: string con la descripción del efecto
;;   - funcion-accion: función (lambda (juego args) -> Juego)
;;
;; ============================================================

(require "base_17407479_CifuentesCarrasco.rkt")

(provide attack
         attack?
         attack-cost
         attack-name
         attack-description
         attack-damage
         attack-action
         ability
         ability?
         ability-name
         ability-description
         ability-action)

;; ============================================================
;; FUNCIONES AUXILIARES
;; ============================================================

;; Descripción: Extrae el valor numérico de daño base desde la
;;              descripción de un ataque. Busca el patrón
;;              "Inflige X de" donde X es un número entero.
;;              Si no encuentra el patrón, retorna 0.
;; Dom: desc (string)
;; Rec: daño (int >= 0)
;; Tipo recursión: No aplica
(define (extraer-daño desc)
  (let ([matches (regexp-match #rx"Inflige ([0-9]+) de" desc)])
    (if matches
        (string->number (cadr matches))
        0)))

;; ============================================================
;; TDA ATAQUE - Constructor
;; ============================================================

;; Descripción: Crea un ataque de Pokémon con su costo de energía,
;;              nombre, descripción y función de acción asociada.
;;              El daño base se extrae automáticamente de la
;;              descripción mediante la función extraer-daño.
;; Dom: costo (List ELEMENT-TYPE) X nombre (string) X
;;      descripcion (string) X funcion-accion (function)
;; Rec: attack
;; Tipo recursión: No aplica
(define (attack costo nombre descripcion funcion-accion)
  (list 'attack costo nombre descripcion (extraer-daño descripcion) funcion-accion))

;; ============================================================
;; TDA ATAQUE - Función de Pertenencia
;; ============================================================

;; Descripción: Verifica si un elemento es un ataque válido.
;;              Comprueba que sea una lista de 6 elementos con
;;              la etiqueta 'attack como primer elemento.
;; Dom: x (any)
;; Rec: boolean
;; Tipo recursión: No aplica
(define (attack? x)
  (and (list? x)
       (= (length x) 6)
       (eq? (car x) 'attack)))

;; ============================================================
;; TDA ATAQUE - Selectores
;; ============================================================

;; Descripción: Obtiene el costo de energía del ataque.
;; Dom: ataque (attack)
;; Rec: costo (List ELEMENT-TYPE)
;; Tipo recursión: No aplica
(define (attack-cost ataque)
  (list-ref ataque 1))

;; Descripción: Obtiene el nombre del ataque.
;; Dom: ataque (attack)
;; Rec: nombre (string)
;; Tipo recursión: No aplica
(define (attack-name ataque)
  (list-ref ataque 2))

;; Descripción: Obtiene la descripción del ataque.
;; Dom: ataque (attack)
;; Rec: descripcion (string)
;; Tipo recursión: No aplica
(define (attack-description ataque)
  (list-ref ataque 3))

;; Descripción: Obtiene el daño base del ataque.
;; Dom: ataque (attack)
;; Rec: daño (int >= 0)
;; Tipo recursión: No aplica
(define (attack-damage ataque)
  (list-ref ataque 4))

;; Descripción: Obtiene la función de acción del ataque.
;; Dom: ataque (attack)
;; Rec: funcion-accion (function)
;; Tipo recursión: No aplica
(define (attack-action ataque)
  (list-ref ataque 5))

;; ============================================================
;; TDA HABILIDAD - Constructor
;; ============================================================

;; Descripción: Crea una habilidad de Pokémon con su nombre,
;;              descripción y función de acción asociada.
;;              A diferencia de los ataques, las habilidades no
;;              tienen costo de energía para ser utilizadas.
;; Dom: nombre (string) X descripcion (string) X
;;      funcion-accion (function)
;; Rec: ability
;; Tipo recursión: No aplica
(define (ability nombre descripcion funcion-accion)
  (list 'ability nombre descripcion funcion-accion))

;; ============================================================
;; TDA HABILIDAD - Función de Pertenencia
;; ============================================================

;; Descripción: Verifica si un elemento es una habilidad válida.
;;              Comprueba que sea una lista de 4 elementos con
;;              la etiqueta 'ability como primer elemento.
;; Dom: x (any)
;; Rec: boolean
;; Tipo recursión: No aplica
(define (ability? x)
  (and (list? x)
       (= (length x) 4)
       (eq? (car x) 'ability)))

;; ============================================================
;; TDA HABILIDAD - Selectores
;; ============================================================

;; Descripción: Obtiene el nombre de la habilidad.
;; Dom: hab (ability)
;; Rec: nombre (string)
;; Tipo recursión: No aplica
(define (ability-name hab)
  (list-ref hab 1))

;; Descripción: Obtiene la descripción de la habilidad.
;; Dom: hab (ability)
;; Rec: descripcion (string)
;; Tipo recursión: No aplica
(define (ability-description hab)
  (list-ref hab 2))

;; Descripción: Obtiene la función de acción de la habilidad.
;; Dom: hab (ability)
;; Rec: funcion-accion (function)
;; Tipo recursión: No aplica
(define (ability-action hab)
  (list-ref hab 3))
