#lang racket

;; ============================================================
;; Funciones de juego (RF08-RF15)
;; ============================================================

(require "base_17407479_CifuentesCarrasco.rkt")
(require "TDAataque_17407479_CifuentesCarrasco.rkt")
(require "TDAcarta_17407479_CifuentesCarrasco.rkt")
(require "TDAmazo_17407479_CifuentesCarrasco.rkt")
(require "TDApokemonEnJuego_17407479_CifuentesCarrasco.rkt")
(require "TDAjugador_17407479_CifuentesCarrasco.rkt")
(require "TDAjuego_17407479_CifuentesCarrasco.rkt")

(provide playToBench
         changeActivePokemon
         drawCardFromDeck
         useEnergyCard
         evolvePokemon
         useTrainerCard
         usePokemonAbility
         usePokemonAttack)

;; ============================================================
;; FUNCIONES AUXILIARES
;; ============================================================

;; Descripción: Remueve la primera carta con el nombre dado de una
;;              lista de cartas. Retorna la lista sin esa carta.
;; Dom: cartas (List card) X nombre (string)
;; Rec: List card
;; Tipo recursión: Natural - recorre hasta encontrar la carta.
(define (remover-carta-por-nombre cartas nombre)
  (cond
    [(null? cartas) '()]
    [(string=? (card-name (car cartas)) nombre)
     (cdr cartas)]
    [else (cons (car cartas)
                (remover-carta-por-nombre (cdr cartas) nombre))]))

;; Descripción: Busca un Pokémon en juego en la banca que
;;              coincida con una carta dada.
;; Dom: banca (List pokemon-in-play) X carta (card)
;; Rec: pokemon-in-play o #f
;; Tipo recursión: Natural
(define (buscar-en-banca banca carta)
  (cond
    [(null? banca) #f]
    [(pip-matches-card? (car banca) carta) (car banca)]
    [else (buscar-en-banca (cdr banca) carta)]))

;; Descripción: Remueve el primer Pokémon de la banca que coincida
;;              con la carta dada.
;; Dom: banca (List pokemon-in-play) X carta (card)
;; Rec: List pokemon-in-play
;; Tipo recursión: Natural
(define (remover-de-banca banca carta)
  (cond
    [(null? banca) '()]
    [(pip-matches-card? (car banca) carta) (cdr banca)]
    [else (cons (car banca) (remover-de-banca (cdr banca) carta))]))

;; Descripción: Actualiza un Pokémon en la banca reemplazándolo
;;              por una nueva versión (match por ID).
;; Dom: banca (List pip) X viejo-id (int) X nuevo-pip (pip)
;; Rec: List pokemon-in-play
;; Tipo recursión: Natural
(define (actualizar-en-banca banca viejo-id nuevo-pip)
  (cond
    [(null? banca) '()]
    [(= (pip-id (car banca)) viejo-id)
     (cons nuevo-pip (cdr banca))]
    [else (cons (car banca)
                (actualizar-en-banca (cdr banca) viejo-id nuevo-pip))]))

;; Descripción: Busca un Pokémon en juego (activo o banca) que
;;              coincida con una carta. Retorna el pip encontrado.
;; Dom: jugador (player) X carta (card)
;; Rec: pokemon-in-play o #f
;; Tipo recursión: No aplica
(define (buscar-pokemon-en-juego jugador carta)
  (let ([activo (player-active jugador)])
    (if (and (not (null? activo)) (pip-matches-card? activo carta))
        activo
        (buscar-en-banca (player-bench jugador) carta))))

;; Descripción: Actualiza un pip en el jugador (activo o banca).
;; Dom: jugador (player) X pip-viejo (pip) X pip-nuevo (pip)
;; Rec: player
;; Tipo recursión: No aplica
(define (actualizar-pokemon-en-jugador jugador pip-viejo pip-nuevo)
  (let ([activo (player-active jugador)])
    (if (and (not (null? activo)) (= (pip-id activo) (pip-id pip-viejo)))
        (player-set-active jugador pip-nuevo)
        (player-set-bench jugador
                          (actualizar-en-banca (player-bench jugador)
                                               (pip-id pip-viejo) pip-nuevo)))))

; Descripción: Verifica si una lista de energías cubre el costo
;              de un ataque. Colorless puede ser cubierto por
;              cualquier tipo de energía.
; Dom: energias-disponibles (List card) X costo (List symbol)
; Rec: boolean
; Tipo recursión: Natural
(define (cubre-costo? energias costo)
  (if (null? costo)
      #t
      (let ([tipo-req (car costo)])
        (if (eq? tipo-req 'colorless)
            ;; Colorless: cualquier energía sirve
            (if (null? energias)
                #f
                (cubre-costo? (cdr energias) (cdr costo)))
            ;; Tipo específico: buscar una energía del tipo
            (let ([encontrada (findf (lambda (e)
                                       (eq? (card-energy-type e) tipo-req))
                                     energias)])
              (if encontrada
                  (cubre-costo? (remover-carta-por-nombre
                                 energias (card-name encontrada))
                                (cdr costo))
                  #f))))))

; Descripción: Calcula el daño final aplicando debilidad y resistencia.
; Dom: daño-base (int) X tipo-atacante (symbol) X tipo-defensor (card)
; Rec: int (>= 0)
; Tipo recursión: No aplica
(define (calcular-daño daño-base tipo-atacante carta-defensor)
  (let* ([debilidad (card-weakness carta-defensor)]
         [resistencia (card-resistance carta-defensor)]
         [mult (if (and (not (null? debilidad))
                        (eq? debilidad tipo-atacante)) 2 1)]
         [rest (if (and (not (null? resistencia))
                        (eq? resistencia tipo-atacante)) 30 0)]
         [resultado (- (* daño-base mult) rest)])
    (max resultado 0)))

;; ============================================================
;; RF08 - playToBench
;; ============================================================

;; Descripción: Pone un Pokémon básico de la mano del jugador
;;              actual en la banca. Máximo 5 en banca.
;; Dom: juego (game) X carta (card)
;; Rec: game
;; Tipo recursión: No aplica
(define (playToBench juego carta)
  (let* ([jugador (game-get-current-player juego)]
         [mano (player-hand jugador)]
         [banca (player-bench jugador)])
    (cond
      [(>= (length banca) 5)
       (error "playToBench: la banca está llena (máx 5)")]
      [(not (card-pokemon? carta))
       (error "playToBench: la carta no es un Pokémon")]
      [(not (or (null? (card-evolves-from carta)) 
                (eq? (card-evolves-from carta) #f)))
       (error "playToBench: no puedes jugar una evolución directamente a la banca")]
      [else
       (let* ([nueva-mano (remover-carta-por-nombre mano (card-name carta))]
              [nuevo-pip (make-pokemon-in-play carta
                                               (game-turn-number juego)
                                               (game-next-id juego))]
              [nueva-banca (append banca (list nuevo-pip))]
              [nuevo-jugador (player-set-hand
                              (player-set-bench jugador nueva-banca)
                              nueva-mano)]
              [g1 (game-update-current-player juego nuevo-jugador)])
         (game-set-next-id g1 (+ (game-next-id juego) 1)))])))

;; ============================================================
;; RF09 - changeActivePokemon
;; ============================================================

;; Descripción: Pone un Pokémon de la banca como activo. Si ya hay
;;              activo, paga el costo de retirada y lo mueve a banca.
;; Dom: juego (game) X carta (card)
;; Rec: game
;; Tipo recursión: No aplica
(define (changeActivePokemon juego carta)
  (let* ([jugador (game-get-current-player juego)]
         [activo-actual (player-active jugador)]
         [banca (player-bench jugador)]
         [pip-banca (buscar-en-banca banca carta)])
    (cond
      [(not pip-banca)
       (error "changeActivePokemon: Pokémon no encontrado en la banca")]
      ;; No hay activo actual -> simplemente poner el de la banca
      [(null? activo-actual)
       (let* ([nueva-banca (remover-de-banca banca carta)]
              [nuevo-jugador (player-set-active
                              (player-set-bench jugador nueva-banca)
                              pip-banca)])
         (game-update-current-player juego nuevo-jugador))]
      ;; Hay activo -> pagar costo de retirada
      [else
       (let* ([costo-ret (card-retreat-cost (pip-card activo-actual))]
              [energias (pip-energies activo-actual)]
              ;; Descartar energías para pagar retirada
              [energias-descartadas (take energias (min costo-ret (length energias)))]
              [energias-restantes (drop energias (min costo-ret (length energias)))]
              [activo-retirado (pip-set-energies activo-actual energias-restantes)]
              ;; Mover activo retirado a banca, poner nuevo como activo
              [nueva-banca (append (remover-de-banca banca carta)
                                   (list activo-retirado))]
              [descarte-actual (player-discard jugador)]
              [nuevo-descarte (append descarte-actual energias-descartadas)]
              [nuevo-jugador (player-set-discard
                              (player-set-active
                               (player-set-bench jugador nueva-banca)
                               pip-banca)
                              nuevo-descarte)])
         (game-update-current-player juego nuevo-jugador))])))

;; ============================================================
;; RF10 - drawCardFromDeck
;; ============================================================

;; Descripción: Roba la primera carta del mazo del jugador actual.
;;              Debe ser la primera acción del turno. Si no quedan
;;              cartas, el oponente gana.
;; Dom: juego (game)
;; Rec: game
;; Tipo recursión: No aplica
(define (drawCardFromDeck juego)
  (let* ([jugador (game-get-current-player juego)]
         [deck (player-deck jugador)]
         [mano (player-hand jugador)]
         [num-turno (+ (game-turn-number juego) 1)])
    (cond
      [(null? deck)
       ;; Oponente gana
       (let ([ganador (if (= (game-current-turn juego) 1) 2 1)])
         (game-set-winner juego ganador))]
      [else
       (let* ([carta (car deck)]
              [nuevo-deck (cdr deck)]
              [nueva-mano (append mano (list carta))]
              [nuevo-jugador (player-set-hand
                              (player-set-deck jugador nuevo-deck)
                              nueva-mano)]
              [g1 (game-update-current-player juego nuevo-jugador)]
              [g2 (game-set-turn-number g1 num-turno)]
              [g3 (game-set-draw-done g2 #t)]
              [g4 (game-set-energy-played g3 #f)]
              [g5 (game-set-supporter-played g4 #f)])
         g5)])))

;; ============================================================
;; RF11 - useEnergyCard
;; ============================================================

;; Descripción: Adjunta una carta de energía de la mano a un
;;              Pokémon en juego del jugador actual. Solo 1 por turno.
;; Dom: juego (game) X cartaPokemon (card) X cartaEnergia (card)
;; Rec: game
;; Tipo recursión: No aplica
(define (useEnergyCard juego cartaPokemon cartaEnergia)
  (let* ([jugador (game-get-current-player juego)]
         [mano (player-hand jugador)]
         [pip (buscar-pokemon-en-juego jugador cartaPokemon)])
    (cond
      [(game-energy-played juego)
       (error "useEnergyCard: ya se jugó una energía este turno")]
      [(not pip)
       (error "useEnergyCard: Pokémon no encontrado en juego")]
      [else
       (let* ([nueva-mano (remover-carta-por-nombre mano (card-name cartaEnergia))]
              [pip-con-energia (pip-add-energy pip cartaEnergia)]
              [nuevo-jugador (actualizar-pokemon-en-jugador
                              (player-set-hand jugador nueva-mano)
                              pip pip-con-energia)]
              [g1 (game-update-current-player juego nuevo-jugador)]
              [g2 (game-set-energy-played g1 #t)])
         g2)])))

;; ============================================================
;; RF12 - evolvePokemon
;; ============================================================

;; Descripción: Evoluciona un Pokémon en juego con una carta de
;;              evolución de la mano. El Pokémon debe llevar al
;;              menos 1 turno en juego.
;; Dom: juego (game) X cartaPokemon (card) X cartaEvolucion (card)
;; Rec: game
;; Tipo recursión: No aplica
(define (evolvePokemon juego cartaPokemon cartaEvolucion)
  (let* ([jugador (game-get-current-player juego)]
         [mano (player-hand jugador)]
         [pip (buscar-pokemon-en-juego jugador cartaPokemon)])
    (cond
      [(not pip)
       (error "evolvePokemon: Pokémon no encontrado en juego")]
      [(not (string=? (card-evolves-from cartaEvolucion) (pip-name pip)))
       (error "evolvePokemon: la carta de evolución no es compatible")]
      [(= (pip-turn-placed pip) (game-turn-number juego))
       (error "evolvePokemon: el Pokémon debe llevar al menos 1 turno en juego")]
      [else
       (let* ([nueva-mano (remover-carta-por-nombre mano (card-name cartaEvolucion))]
              [pip-evolucionado (pip-evolve pip cartaEvolucion)]
              [nuevo-jugador (actualizar-pokemon-en-jugador
                              (player-set-hand jugador nueva-mano)
                              pip pip-evolucionado)])
         (game-update-current-player juego nuevo-jugador))])))

;; ============================================================
;; RF13 - useTrainerCard
;; ============================================================

; Descripción: Usa una carta de entrenador de la mano. Las de tipo
;              "partidario" solo 1 por turno. Luego va al descarte.
; Dom: juego (game) X cartaEntrenador (card) X args (List)
; Rec: game
; Tipo recursión: No aplica
(define (useTrainerCard juego cartaEntrenador args)
  (let* ([jugador (game-get-current-player juego)]
         [mano (player-hand jugador)])
    (cond
      [(and (string=? (card-trainer-type cartaEntrenador) "partidario")
            (game-supporter-played juego))
       (error "useTrainerCard: ya se usó una carta partidario este turno")]
      [else
       (let* ([nueva-mano (remover-carta-por-nombre mano (card-name cartaEntrenador))]
              [nuevo-descarte (append (player-discard jugador) (list cartaEntrenador))]
              [nuevo-jugador (player-set-discard
                              (player-set-hand jugador nueva-mano)
                              nuevo-descarte)]
              [g1 (game-update-current-player juego nuevo-jugador)]
              ;; Ejecutar la acción de la carta
              [g2 ((card-trainer-action cartaEntrenador) g1 args)]
              ;; Marcar partidario si aplica
              [g3 (if (string=? (card-trainer-type cartaEntrenador) "partidario")
                      (game-set-supporter-played g2 #t)
                      g2)])
         g3)])))

;; ============================================================
;; RF14 - usePokemonAbility
;; ============================================================

; Descripción: Usa la habilidad de un Pokémon en juego del jugador.
; Dom: juego (game) X cartaPokemon (card) X args (List)
; Rec: game
; Tipo recursión: No aplica
(define (usePokemonAbility juego cartaPokemon args)
  (let* ([jugador (game-get-current-player juego)]
         [pip (buscar-pokemon-en-juego jugador cartaPokemon)]
         [carta (pip-card pip)]
         [hab (card-ability carta)])
    (cond
      [(null? hab)
       (error "usePokemonAbility: el Pokémon no tiene habilidad")]
      [else
       ((ability-action hab) juego args)])))

;; ============================================================
;; RF15 - usePokemonAttack
;; ============================================================

;; Descripción: Usa un ataque del Pokémon activo. Aplica daño con
;;              debilidad/resistencia. Si el defensor llega a 0 PS,
;;              es KO. Luego pasa el turno.
;;              Si nombreAtaque es null, no ataca y solo pasa turno.
;; Dom: juego (game) X cartaPokemon (card) X nombreAtaque (string o null) X
;;      args (List)
;; Rec: game
;; Tipo recursión: No aplica
(define (usePokemonAttack juego cartaPokemon nombreAtaque args)
  (cond
    ;; Si no ataca, solo pasar turno
    [(null? nombreAtaque)
     (pasar-turno juego)]
    [else
     (let* ([jugador (game-get-current-player juego)]
            [activo (player-active jugador)]
            [carta-activo (pip-card activo)]
            [ataques (card-attacks carta-activo)]
            [ataque (findf (lambda (a)
                             (string=? (attack-name a) nombreAtaque))
                           ataques)])
       (cond
         [(not ataque)
          (error "usePokemonAttack: ataque no encontrado" nombreAtaque)]
         [(not (cubre-costo? (pip-energies activo) (attack-cost ataque)))
          (error "usePokemonAttack: energía insuficiente para" nombreAtaque)]
         [else
          ;; Verificar estados que impiden atacar
          (let ([estado (pip-status activo)])
            (cond
              [(eq? estado 'asleep)
               (error "usePokemonAttack: el Pokémon está dormido")]
              [(eq? estado 'paralyzed)
               (error "usePokemonAttack: el Pokémon está paralizado")]
              [else
               ;; Ejecutar acción del ataque primero
               (let* ([g1 ((attack-action ataque) juego args)]
                      ;; Recalcular referencias post-acción
                      [jugador2 (game-get-current-player g1)]
                      [activo2 (player-active jugador2)]
                      [oponente (game-get-opponent g1)]
                      [activo-rival (player-active oponente)]
                      ;; Calcular daño
                      [daño-base (attack-damage ataque)]
                      [tipo-atk (pip-pokemon-type activo2)]
                      [daño-final (if (null? activo-rival) 0
                                      (calcular-daño daño-base tipo-atk
                                                     (pip-card activo-rival)))]
                      ;; Aplicar daño al oponente
                      [g2 (if (and (not (null? activo-rival)) (> daño-final 0))
                              (let* ([nuevo-daño (+ (pip-damage activo-rival) daño-final)]
                                     [rival-dañado (pip-set-damage activo-rival nuevo-daño)]
                                     [oponente2 (player-set-active oponente rival-dañado)])
                                (game-update-opponent g1 oponente2))
                              g1)])
                 ;; Verificar KO y pasar turno
                 (resolver-ko-y-pasar-turno g2))]))]))]))

; Descripción: Resuelve KO del Pokémon activo rival si sus PS
;              llegaron a 0 y luego pasa el turno.
; Dom: juego (game)
; Rec: game
; Tipo recursión: No aplica
(define (resolver-ko-y-pasar-turno juego)
  (let* ([oponente (game-get-opponent juego)]
         [activo-rival (player-active oponente)])
    (if (and (not (null? activo-rival))
             (<= (pip-hp-actual activo-rival) 0))
        ;; KO: descartar pokémon + energías + evoluciones
        (let* ([cartas-a-descartar
                (append (list (pip-card activo-rival))
                        (pip-energies activo-rival)
                        (pip-evolutions activo-rival))]
               [nuevo-descarte (append (player-discard oponente)
                                       cartas-a-descartar)]
               [oponente2 (player-set-discard
                           (player-set-active oponente null)
                           nuevo-descarte)]
               [g1 (game-update-opponent juego oponente2)]
               ;; Tomar premios
               [premios-a-tomar (if (card-is-ex? (pip-card activo-rival)) 2 1)]
               [jugador (game-get-current-player g1)]
               [premios (player-prizes jugador)]
               [cartas-premio (take premios (min premios-a-tomar (length premios)))]
               [premios-rest (drop premios (min premios-a-tomar (length premios)))]
               [nueva-mano (append (player-hand jugador) cartas-premio)]
               [jugador2 (player-set-prizes
                          (player-set-hand jugador nueva-mano)
                          premios-rest)]
               [g2 (game-update-current-player g1 jugador2)])
          ;; Verificar victoria
          (cond
            [(null? premios-rest)
             (game-set-winner (pasar-turno g2) (game-current-turn g2))]
            [(and (null? (player-active oponente2))
                  (null? (player-bench oponente2)))
             (game-set-winner (pasar-turno g2) (game-current-turn g2))]
            [else (pasar-turno g2)]))
        ;; No KO: solo pasar turno
        (pasar-turno juego))))

; Descripción: Pasa el turno al otro jugador, resetea flags.
; Dom: juego (game)
; Rec: game
; Tipo recursión: No aplica
(define (pasar-turno juego)
  (let* ([nuevo-turno (if (= (game-current-turn juego) 1) 2 1)]
         [g1 (game-set-current-turn juego nuevo-turno)]
         [g2 (game-set-energy-played g1 #f)]
         [g3 (game-set-supporter-played g2 #f)]
         [g4 (game-set-draw-done g3 #f)])
    g4))
