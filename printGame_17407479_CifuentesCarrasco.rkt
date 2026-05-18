#lang racket

;; ============================================================
;; printGame
;; ============================================================

(require "base_17407479_CifuentesCarrasco.rkt")
(require "TDAataque_17407479_CifuentesCarrasco.rkt")
(require "TDAcarta_17407479_CifuentesCarrasco.rkt")
(require "TDAmazo_17407479_CifuentesCarrasco.rkt")
(require "TDApokemonEnJuego_17407479_CifuentesCarrasco.rkt")
(require "TDAjugador_17407479_CifuentesCarrasco.rkt")
(require "TDAjuego_17407479_CifuentesCarrasco.rkt")

(provide printGame)

;; ============================================================
;; Funciones auxiliares para construir strings
;; ============================================================

;; Descripción: Convierte una lista de energías a un string descriptivo.
;; Dom: energias (List card-energy)
;; Rec: string
;; Tipo recursión: Cola - usa foldl para acumular conteos.
(define (energias->string energias)
  (if (null? energias)
      "sin energías"
      (let ([conteos (foldl (lambda (e acc)
                              (let* ([tipo (symbol->string (card-energy-type e))]
                                     [encontrado (assoc tipo acc)])
                                (if encontrado
                                    (map (lambda (par)
                                           (if (string=? (car par) tipo)
                                               (cons tipo (+ (cdr par) 1))
                                               par))
                                         acc)
                                    (append acc (list (cons tipo 1))))))
                            '()
                            energias)])
        (string-join
         (map (lambda (par)
                (string-append (number->string (cdr par)) " " (car par)))
              conteos)
         ", "))))

;; Descripción: Convierte un Pokémon en juego a string.
;; Dom: p (pokemon-in-play)
;; Rec: string
;; Tipo recursión: No aplica
(define (pokemon-en-juego->string p)
  (string-append
   "    " (pip-name p)
   "\t" (number->string (pip-hp-actual p))
   "/" (number->string (card-hp (pip-card p))) "PS"
   "\t" (symbol->string (pip-pokemon-type p))
   "\t" (energias->string (pip-energies p))
   "\t" (number->string (pip-damage p)) " daño"
   (if (eq? (pip-status p) 'normal) ""
       (string-append " [" (symbol->string (pip-status p)) "]"))
   (if (card-is-ex? (pip-card p)) " (EX)" "")))

;; Descripción: Construye string de una lista de Pokémon en juego
;;              usando recursión de cola con acumulador.
;; Dom: pokes (List pokemon-in-play) X acc (string)
;; Rec: string
;; Tipo recursión: Cola - acumula el resultado en acc.
(define (pokes->string pokes acc)
  (if (null? pokes)
      acc
      (pokes->string (cdr pokes)
                     (string-append acc (pokemon-en-juego->string (car pokes)) "\n"))))

;; Descripción: Construye string de nombres de cartas con recursión
;;              de cola.
;; Dom: cartas (List card) X acc (string)
;; Rec: string
;; Tipo recursión: Cola
(define (nombres-cartas->string cartas acc)
  (if (null? cartas)
      acc
      (nombres-cartas->string
       (cdr cartas)
       (string-append acc "    " (card-name (car cartas)) "\n"))))

;; Descripción: Construye el string de un jugador.
;; Dom: jugador (player) X mostrar-mano (boolean) X num (int)
;; Rec: string
;; Tipo recursión: No aplica
(define (jugador->string jugador mostrar-mano num)
  (let ([mano (player-hand jugador)]
        [activo (player-active jugador)]
        [banca (player-bench jugador)]
        [premios (player-prizes jugador)]
        [deck (player-deck jugador)]
        [descarte (player-discard jugador)])
    (string-append
     "Jugador " (number->string num) ": "
     (if mostrar-mano
         (string-append (number->string (length mano)) " cartas en la mano\n"
                        (nombres-cartas->string mano ""))
         (string-append (number->string (length mano)) " cartas en la mano\n"))
     "Mazo: " (number->string (length deck)) " cartas disponibles\n"
     "Premios disponibles: " (number->string (length premios)) "\n"
     "Banca:\n"
     (if (null? banca) "    (vacía)\n" (pokes->string banca ""))
     "Pokemon activo:\n"
     (if (null? activo) "    (ninguno)\n"
         (string-append (pokemon-en-juego->string activo) "\n"))
     "Pila de descarte:\n"
     (if (null? descarte) "    (vacía)\n"
         (nombres-cartas->string descarte "")))))

;; ============================================================
;; printGame
;; ============================================================

;; Descripción: Retorna un string con la representación completa
;;              del estado del juego. Muestra la mano solo del
;;              jugador indicado en el segundo argumento.
;;              Usa recursión de cola internamente.
;;              NO usa display/write dentro de la función.
;; Dom: juego (game) X numero-jugador (int)
;; Rec: string
;; Tipo recursión: No aplica
(define (printGame juego numero-jugador)
  (let ([p1 (game-player1 juego)]
        [p2 (game-player2 juego)]
        [turno (game-current-turn juego)]
        [num-turno (game-turn-number juego)]
        [ganador (game-winner juego)])
    (string-append
     "Turno actual: Jugador " (number->string turno) "\n"
     (if (not (null? ganador))
         (string-append "*** GANADOR: Jugador " (number->string ganador) " ***\n")
         "")
     "\n"
     (jugador->string p1 (= numero-jugador 1) 1)
     "\n"
     (jugador->string p2 (= numero-jugador 2) 2))))
