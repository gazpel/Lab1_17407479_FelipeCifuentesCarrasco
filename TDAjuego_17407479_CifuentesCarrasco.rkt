#lang racket

;; ============================================================
;; TDA Juego e initGame
;; ============================================================
;; Representación: Lista
;; (list 'game p1 p2 turno-actual num-turno
;;       energia-jugada partidario-jugado robo-hecho
;;       semilla next-id ganador)
;; Índices: 0:tag 1:p1 2:p2 3:turno-actual 4:num-turno
;;          5:energia-jugada 6:partidario-jugado 7:robo-hecho
;;          8:semilla 9:next-id 10:ganador
;; ============================================================

(require "base_17407479_CifuentesCarrasco.rkt")
(require "TDAataque_17407479_CifuentesCarrasco.rkt")
(require "TDAcarta_17407479_CifuentesCarrasco.rkt")
(require "TDAmazo_17407479_CifuentesCarrasco.rkt")
(require "TDApokemonEnJuego_17407479_CifuentesCarrasco.rkt")
(require "TDAjugador_17407479_CifuentesCarrasco.rkt")

(provide make-game game?
         game-player1 game-player2 game-current-turn game-turn-number
         game-energy-played game-supporter-played game-draw-done
         game-seed game-next-id game-winner
         game-set-player1 game-set-player2 game-set-current-turn
         game-set-turn-number game-set-energy-played
         game-set-supporter-played game-set-draw-done
         game-set-seed game-set-next-id game-set-winner
         game-get-current-player game-get-opponent
         game-update-current-player game-update-opponent
         initGame)

;; ============================================================
;; Constructor
;; ============================================================

;; Descripción: Crea un estado de juego completo.
;; Dom: p1 (player) X p2 (player) X turno-actual (int) X
;;      num-turno (int) X energia-jugada (bool) X
;;      partidario-jugado (bool) X robo-hecho (bool) X
;;      semilla (int+) X next-id (int) X ganador (int o null)
;; Rec: game
;; Tipo recursión: No aplica
(define (make-game p1 p2 turno-actual num-turno
                   energia-jugada partidario-jugado robo-hecho
                   semilla next-id ganador)
  (list 'game p1 p2 turno-actual num-turno
        energia-jugada partidario-jugado robo-hecho
        semilla next-id ganador))

;; ============================================================
;; Función de Pertenencia
;; ============================================================

;; Descripción: Verifica si un elemento es un juego válido.
;; Dom: x (any)  ;  Rec: boolean
;; Tipo recursión: No aplica
(define (game? x)
  (and (list? x) (not (null? x)) (eq? (car x) 'game)))

;; ============================================================
;; Selectores
;; ============================================================

(define (game-player1 g) (list-ref g 1))
(define (game-player2 g) (list-ref g 2))
(define (game-current-turn g) (list-ref g 3))
(define (game-turn-number g) (list-ref g 4))
(define (game-energy-played g) (list-ref g 5))
(define (game-supporter-played g) (list-ref g 6))
(define (game-draw-done g) (list-ref g 7))
(define (game-seed g) (list-ref g 8))
(define (game-next-id g) (list-ref g 9))
(define (game-winner g) (list-ref g 10))

;; ============================================================
;; Modificadores
;; ============================================================

;; Descripción: Función auxiliar que reconstruye el juego
;;              reemplazando un campo por índice.
;; Dom: g (game) X idx (int) X valor (any)
;; Rec: game
;; Tipo recursión: No aplica
(define (game-set g idx valor)
  (let ([campos (list (game-player1 g) (game-player2 g)
                      (game-current-turn g) (game-turn-number g)
                      (game-energy-played g) (game-supporter-played g)
                      (game-draw-done g) (game-seed g)
                      (game-next-id g) (game-winner g))])
    (apply make-game
           (map (lambda (v i) (if (= i idx) valor v))
                campos
                '(0 1 2 3 4 5 6 7 8 9)))))

(define (game-set-player1 g v) (game-set g 0 v))
(define (game-set-player2 g v) (game-set g 1 v))
(define (game-set-current-turn g v) (game-set g 2 v))
(define (game-set-turn-number g v) (game-set g 3 v))
(define (game-set-energy-played g v) (game-set g 4 v))
(define (game-set-supporter-played g v) (game-set g 5 v))
(define (game-set-draw-done g v) (game-set g 6 v))
(define (game-set-seed g v) (game-set g 7 v))
(define (game-set-next-id g v) (game-set g 8 v))
(define (game-set-winner g v) (game-set g 9 v))

;; ============================================================
;; Funciones de Turno activo
;; ============================================================

;; Descripción: Obtiene el jugador cuyo turno es el actual.
;; Dom: g (game)  ;  Rec: player
;; Tipo recursión: No aplica
(define (game-get-current-player g)
  (if (= (game-current-turn g) 1)
      (game-player1 g)
      (game-player2 g)))

;; Descripción: Obtiene el jugador oponente al turno actual.
;; Dom: g (game)  ;  Rec: player
;; Tipo recursión: No aplica
(define (game-get-opponent g)
  (if (= (game-current-turn g) 1)
      (game-player2 g)
      (game-player1 g)))

;; Descripción: Actualiza el jugador actual en el juego.
;; Dom: g (game) X jugador (player)
;; Rec: game
;; Tipo recursión: No aplica
(define (game-update-current-player g jugador)
  (if (= (game-current-turn g) 1)
      (game-set-player1 g jugador)
      (game-set-player2 g jugador)))

;; Descripción: Actualiza el oponente en el juego.
;; Dom: g (game) X jugador (player)
;; Rec: game
;; Tipo recursión: No aplica
(define (game-update-opponent g jugador)
  (if (= (game-current-turn g) 1)
      (game-set-player2 g jugador)
      (game-set-player1 g jugador)))

;; ============================================================
;; initGame
;; ============================================================

;; Descripción: Saca 7 cartas de un mazo barajado y verifica que
;;              haya al menos un Pokémon básico. Si no hay, vuelve
;;              a barajar todas las cartas y reintenta.
;;              Retorna (list mano cartas-restantes semilla-final).
;; Dom: cartas (List card) X semilla (int+)
;; Rec: (List (List card) (List card) int+)
;; Tipo recursión: Cola - reintenta con nuevo barajado hasta
;;                 encontrar una mano con Pokémon básico.
(define (sacar-mano-con-basico cartas semilla)
  (let ([mano (take cartas 7)]
        [resto (drop cartas 7)])
    (if (ormap card-basic-pokemon? mano)
        (list mano resto semilla)
        (let* ([nueva-semilla (randomPuro semilla)]
               [rebarajado (shuffle-helper cartas nueva-semilla)]
               [semilla-final (avanzar-semilla nueva-semilla
                                               (length cartas))])
          (sacar-mano-con-basico rebarajado semilla-final)))))

;; Descripción: Inicia un juego de Pokémon TCG. Realiza:
;;              1. Baraja ambos mazos con la semilla.
;;              2. Cada jugador roba 7 cartas (rebaraja si no
;;                 tiene Pokémon básico).
;;              3. Toma 6 cartas de premio por jugador.
;;              4. Lanza moneda para determinar quién comienza.
;; Dom: mazo1 (deck) X mazo2 (deck) X semilla (int+)
;; Rec: game
;; Tipo recursión: No aplica (delega a sacar-mano-con-basico)
(define (initGame mazo1 mazo2 semilla)
  (let* (;; Barajar mazo 1
         [d1s (shuffle-helper (deck-cards mazo1) semilla)]
         [sem1 (avanzar-semilla semilla 60)]
         ;; Barajar mazo 2
         [d2s (shuffle-helper (deck-cards mazo2) sem1)]
         [sem2 (avanzar-semilla sem1 60)]
         ;; Sacar manos con verificación de básico
         [res1 (sacar-mano-con-basico d1s sem2)]
         [mano1 (car res1)]
         [resto1 (cadr res1)]
         [sem3 (caddr res1)]
         [res2 (sacar-mano-con-basico d2s sem3)]
         [mano2 (car res2)]
         [resto2 (cadr res2)]
         [sem4 (caddr res2)]
         ;; Premios y mazos finales
         [premios1 (take resto1 6)]
         [deck1 (drop resto1 6)]
         [premios2 (take resto2 6)]
         [deck2 (drop resto2 6)]
         ;; Moneda: determina quién comienza
         [sem-moneda (randomPuro sem4)]
         [primer-jugador (- 2 (modulo sem-moneda 2))]
         ;; Crear jugadores
         [p1 (make-player 1 deck1 mano1 null '() premios1 '())]
         [p2 (make-player 2 deck2 mano2 null '() premios2 '())])
    (make-game p1 p2 primer-jugador 0 #f #f #f
               (randomPuro sem-moneda) 1 null)))
