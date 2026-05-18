#lang racket

;; ============================================================
;; TDA Deck (Mazo)
;; ============================================================
;;
;; Representación: Lista etiquetada
;; (list 'deck lista-de-cartas)
;; Donde lista-de-cartas es una lista de exactamente 60 cartas.
;;
;; Índices: 0:'deck  1:lista-de-cartas
;;
;; ============================================================

(require "base_17407479_CifuentesCarrasco.rkt")
(require "TDAataque_17407479_CifuentesCarrasco.rkt")
(require "TDAcarta_17407479_CifuentesCarrasco.rkt")

(provide deck
         deck?
         deck-cards
         deck-size
         deck-first-card
         deck-rest
         shuffleDeck
         randomPuro
         shuffle-helper
         avanzar-semilla)

;; ============================================================
;; FUNCIONES AUXILIARES
;; ============================================================

;; Descripción: Generador de números pseudoaleatorios.
;;              Dado un número Xn,produce el siguiente número de
;;              la secuencia.
;;              Proporcionada en el enunciado del laboratorio.
;; Dom: Xn (int+)
;; Rec: int+
;; Tipo recursión: No aplica
(define (randomPuro Xn)
  (modulo (+ (* Xn 1103515245) 12345) 2147483648))

;; Descripción: Cuenta cuántas veces aparece un nombre en una
;;              lista de cartas. Se usa para validar el límite
;;              de 4 copias por nombre.
;; Dom: nombre (string) X cartas (List card)
;; Rec: cantidad (int >= 0)
;; Tipo recursión: Cola - se usa foldl para recorrer toda la lista
;;                 acumulando el conteo.
(define (contar-por-nombre nombre cartas)
  (foldl (lambda (c acc)
           (if (string=? (card-name c) nombre)
               (+ acc 1)
               acc))
         0
         cartas))

; Descripción: Obtiene una lista de nombres únicos a partir
;              de una lista de cartas.
; Dom: cartas (List card)
; Rec: nombres (List string)
; Tipo recursión: Cola - se usa foldl para recorrer la lista
;                 acumulando los nombres no repetidos.
(define (nombres-unicos cartas)
  (foldl (lambda (c acc)
           (let ([nombre (card-name c)])
             (if (member nombre acc)
                 acc
                 (cons nombre acc))))
         '()
         cartas))

; Descripción: Verifica si alguna carta no-energía supera el
;              límite de 4 copias por nombre en la lista.
; Dom: cartas (List card)
; Rec: boolean (#t si es válido, #f si hay exceso)
; Tipo recursión: Cola - se usa andmap que itera sobre todos
;                 los nombres verificando la condición.
(define (copias-validas? cartas)
  (let ([no-energia (filter (lambda (c) (not (card-energy? c))) cartas)])
    (andmap (lambda (nombre)
              (<= (contar-por-nombre nombre no-energia) 4))
            (nombres-unicos no-energia))))

; Descripción: Verifica si hay al menos un Pokémon básico
;              en la lista de cartas.
; Dom: cartas (List card)
; Rec: boolean
; Tipo recursión: No aplica - se usa ormap para buscar.
(define (tiene-basico? cartas)
  (ormap card-basic-pokemon? cartas))

; Descripción: Remueve el elemento en el índice dado de una lista.
;              Índice 0 corresponde al primer elemento.
; Dom: lst (List) X idx (int >= 0)
; Rec: List (sin el elemento en idx)
; Tipo recursión: Natural - se recorre la lista hasta encontrar
;                 el índice deseado, reconstruyendo con cons.
(define (remover-en-indice lst idx)
  (cond
    [(null? lst) '()]
    [(= idx 0) (cdr lst)]
    [else (cons (car lst) (remover-en-indice (cdr lst) (- idx 1)))]))

;; ============================================================
;; TDA DECK - Constructor
;; ============================================================

;; Descripción: Crea un mazo de exactamente 60 cartas. Valida:
;;              - Exactamente 60 cartas
;;              - Máximo 4 copias de cartas con el mismo nombre
;;                (las energías básicas no tienen este límite)
;;              - Al menos un Pokémon básico en el mazo
;; Dom: cartas(card, card, ...) — exactamente 60 cartas
;; Rec: deck
;; Tipo recursión: No aplica
(define (deck . cartas)
  (cond
    [(not (= (length cartas) 60))
     (error "deck: el mazo debe tener exactamente 60 cartas, se recibieron"
            (length cartas))]
    [(not (copias-validas? cartas))
     (error "deck: se excede el límite de 4 copias para alguna carta")]
    [(not (tiene-basico? cartas))
     (error "deck: el mazo debe incluir al menos un Pokémon básico")]
    [else
     (list 'deck cartas)]))

;; ============================================================
;; TDA DECK - Función de Pertenencia
;; ============================================================

;; Descripción: Verifica si un elemento es un mazo válido.
;; Dom: x (any)
;; Rec: boolean
;; Tipo recursión: No aplica
(define (deck? x)
  (and (list? x)
       (= (length x) 2)
       (eq? (car x) 'deck)))

;; ============================================================
;; TDA DECK - Selectores
;; ============================================================

;; Descripción: Obtiene la lista de cartas del mazo.
;; Dom: d (deck)
;; Rec: cartas (List card)
;; Tipo recursión: No aplica
(define (deck-cards d)
  (list-ref d 1))

; Descripción: Obtiene la cantidad de cartas en el mazo.
; Dom: d (deck)
; Rec: cantidad (int >= 0)
; Tipo recursión: No aplica
(define (deck-size d)
  (length (deck-cards d)))

; Descripción: Obtiene la primera carta del mazo (la de arriba).
; Dom: d (deck)
; Rec: carta (card)
; Tipo recursión: No aplica
(define (deck-first-card d)
  (car (deck-cards d)))

; Descripción: Obtiene el mazo sin la primera carta.
;              Retorna una lista de cartas (no un deck, ya que
;              no tendría 60 cartas).
; Dom: d (deck)
; Rec: cartas (List card)
; Tipo recursión: No aplica
(define (deck-rest d)
  (cdr (deck-cards d)))

;; ============================================================
;; TDA DECK - shuffleDeck
;; ============================================================

;; Descripción: Función auxiliar que baraja una lista de cartas
;;              de forma determinista usando una semilla. En cada
;;              paso selecciona una carta usando el índice generado
;;              por randomPuro, la coloca al inicio del resultado
;;              y continúa con las cartas restantes.
;; Dom: cartas (List card) X semilla (int+)
;; Rec: cartas-barajadas (List card)
;; Tipo recursión: Natural - en cada llamada se reduce la lista
;;                 en un elemento hasta quedar vacía. Se usa
;;                 recursión natural porque se necesita construir
;;                 la lista resultado con cons al retornar.
(define (shuffle-helper cartas semilla)
  (if (null? cartas)
      '()
      (let* ([nueva-semilla (randomPuro semilla)]
             [idx (modulo nueva-semilla (length cartas))]
             [carta-elegida (list-ref cartas idx)]
             [restantes (remover-en-indice cartas idx)])
        (cons carta-elegida (shuffle-helper restantes nueva-semilla)))))

;; Descripción: Baraja un mazo de cartas de forma determinista
;;              usando una semilla para el generador de números
;;              pseudoaleatorios. La misma semilla siempre produce
;;              el mismo resultado (transparencia referencial).
;; Dom: mazo (deck) X semilla (int+)
;; Rec: deck (mazo barajado)
;; Tipo recursión: No aplica (delega a shuffle-helper)
(define (shuffleDeck mazo semilla)
  (list 'deck (shuffle-helper (deck-cards mazo) semilla)))

;; Descripción: Avanza la semilla n veces aplicando randomPuro
;;              de forma sucesiva. Útil para calcular el estado
;;              de la semilla después de una operación de barajado.
;; Dom: semilla (int+) X n (int >= 0)
;; Rec: semilla-avanzada (int+)
;; Tipo recursión: Cola - el resultado se calcula directamente
;;                 en cada llamada recursiva sin acumular.
(define (avanzar-semilla semilla n)
  (if (= n 0)
      semilla
      (avanzar-semilla (randomPuro semilla) (- n 1))))
