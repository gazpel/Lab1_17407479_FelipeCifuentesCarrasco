#lang racket
;; ============================================================
;; Script de pruebas
;; ============================================================

(require "main_17407479_CifuentesCarrasco.rkt")

;; ============================================================
;; DEFINICIONES BÁSICAS
;; ============================================================
(define accion-sin-efecto (lambda (j a) j))
(define accion-dormir (lambda (j a) j))
(define accion-paralizar (lambda (j a) j))
(define accion-envenenar (lambda (j a) j))
(define accion-dano-variable (lambda (j a) j))
(define accion-mover-energia (lambda (j a) j))
(define accion-robar-carta (lambda (j a) j))
(define accion-superpocion (lambda (j a) j))
(define accion-pokeball (lambda (j a) j))
(define accion-cambio (lambda (j a) j))

(define a1 (attack '(psychic) "Pequeña Rabieta" "Inflige 20 de daño" accion-sin-efecto))
(define a2 (attack '(psychic colorless) "Pesadilla" "Lanza 1 moneda. Si sale cara, el Pokémon Activo rival queda Dormido" accion-dormir))
(define a3 (attack '(psychic psychic) "Bola Sombra" "Inflige 50 de daño" accion-sin-efecto))
(define a4 (attack '(psychic psychic colorless) "Golpe Psíquico" "Inflige 80 de daño" accion-sin-efecto))
(define a5 (attack '(fire) "Ascuas" "Inflige 30 de daño" accion-sin-efecto))
(define a6 (attack '(fire colorless) "Lanzallamas" "Inflige 70 de daño. Descarta 1 energía de fuego" accion-sin-efecto))
(define a7 (attack '(fire fire colorless) "Llamarada" "Inflige 120 de daño" accion-sin-efecto))
(define a8 (attack '(colorless) "Arañazo" "Inflige 10 de daño" accion-sin-efecto))
(define a9 (attack '(lightning) "Impactrueno" "Inflige 20 de daño" accion-sin-efecto))
(define a10 (attack '(lightning colorless) "Electrobola" "Inflige 50 de daño" accion-sin-efecto))
(define a11 (attack '(lightning lightning colorless) "Trueno" "Inflige daño variable según monedas" accion-dano-variable))
(define a12 (attack '(water) "Burbuja" "Lanza 1 moneda. Si sale cara, queda Paralizado" accion-paralizar))
(define a13 (attack '(water colorless) "Pistola Agua" "Inflige 40 de daño" accion-sin-efecto))
(define a14 (attack '(water water colorless) "Hidrobomba" "Inflige 90 de daño" accion-sin-efecto))
(define a15 (attack '(colorless colorless) "Placaje Tóxico" "Inflige 20 de daño y envenena al rival" accion-envenenar))
(define hab1 (ability "Traspaso de Energía" "Mueve 1 energía" accion-mover-energia))
(define hab2 (ability "Pescar" "Roba 1 carta adicional del mazo" accion-robar-carta))

(define c1 (card 'pokemon "Gastly" null 40 'psychic 'darkness 'fighting 1 #f null (list a1 a2)))
(define c2 (card 'pokemon "Abra" null 30 'psychic 'darkness null 0 #f hab1 (list a8)))
(define c3 (card 'pokemon "Charmander" null 50 'fire 'water null 1 #f null (list a5 a8)))
(define c4 (card 'pokemon "Haunter" "Gastly" 70 'psychic 'darkness 'fighting 1 #f null (list a2 a3)))
(define c5 (card 'pokemon "Charmeleon" "Charmander" 80 'fire 'water null 1 #f null (list a5 a6)))
(define c6 (card 'pokemon "Mewtwo EX" null 170 'psychic 'darkness null 2 #t null (list a3 a4)))
(define c7 (card 'pokemon "Pikachu" null 60 'lightning 'fighting null 1 #f null (list a9 a10)))
(define c8 (card 'pokemon "Squirtle" null 50 'water 'lightning null 1 #f null (list a12 a13)))
(define c9 (card 'pokemon "Magnemite" null 40 'lightning 'fighting 'metal 1 #f hab2 (list a9)))
(define c10 (card 'pokemon "Raichu" "Pikachu" 100 'lightning 'fighting null 1 #f null (list a10 a11)))
(define c11 (card 'pokemon "Wartortle" "Squirtle" 70 'water 'lightning null 1 #f null (list a13 a14)))
(define c12 (card 'pokemon "Pikachu EX" null 190 'lightning 'fighting null 2 #t null (list a10 a15)))
(define ce-psi (card 'energy "Energía Psíquica" 'psychic))
(define ce-fire (card 'energy "Energía de Fuego" 'fire))
(define ce-elec (card 'energy "Energía Eléctrica" 'lightning))
(define ce-water (card 'energy "Energía de Agua" 'water))
(define ct1 (card 'trainer "Superpoción" "objeto" "Cura 60 PS" accion-superpocion))
(define ct2 (card 'trainer "Pokeball" "objeto" "Busca pokémon" accion-pokeball))
(define ct3 (card 'trainer "Profesora Encina" "partidario" "Roba 7" accion-cambio))
(define ct4 (card 'trainer "Cambio" "objeto" "Intercambia activo" accion-cambio))
(define ct5 (card 'trainer "Superpoción" "objeto" "Cura 60 PS" accion-superpocion))
(define ct6 (card 'trainer "Profesora Encina" "partidario" "Roba 7" accion-cambio))

(define d1 (deck c1 c1 c1 c1 c4 c4 c4 c2 c2 c2 c2 c3 c3 c3 c3 c5 c5 c5 c6 c6
                 ct1 ct1 ct1 ct2 ct2 ct2 ct3 ct3
                 ce-psi ce-psi ce-psi ce-psi ce-psi ce-psi ce-psi ce-psi
                 ce-psi ce-psi ce-psi ce-psi ce-psi ce-psi ce-psi ce-psi
                 ce-fire ce-fire ce-fire ce-fire ce-fire ce-fire ce-fire ce-fire
                 ce-fire ce-fire ce-fire ce-fire ce-fire ce-fire ce-fire ce-fire))
(define d2 (deck c7 c7 c7 c7 c10 c10 c10 c8 c8 c8 c8 c11 c11 c11 c9 c9 c9 c9 c12 c12
                 ct4 ct4 ct4 ct5 ct5 ct5 ct6 ct6
                 ce-elec ce-elec ce-elec ce-elec ce-elec ce-elec ce-elec ce-elec
                 ce-elec ce-elec ce-elec ce-elec ce-elec ce-elec ce-elec ce-elec
                 ce-water ce-water ce-water ce-water ce-water ce-water ce-water ce-water
                 ce-water ce-water ce-water ce-water ce-water ce-water ce-water ce-water))


;; ============================================================
;;         EJEMPLOS POR CADA UNA DE LAS FUNCIONES
;; ============================================================
(displayln "============================================================")
(displayln "  PRIMERO: EJEMPLOS POR FUNCIÓN")
(displayln "============================================================\n")

;;  RF01 a RF03: Constructores y Selectores 
(displayln "=== RF02: TDA Ataque y Habilidad  ===")
(display "1. attack? de un ataque válido: ") (displayln (attack? a1))
(display "2. attack? de un valor inválido: ") (displayln (attack? ct6))
(display "3. ability-name de una habilidad: ") (displayln (ability-name hab1))

(displayln "\n=== RF03: TDA Card ===")
(display "1. card-pokemon? Gastly: ") (displayln (card-pokemon? c1))
(display "2. card-energy? Pikachu: ") (displayln (card-energy? c7))
(display "3. card-hp de Mewtwo EX: ") (displayln (card-hp c6))

;;  RF04: deck 
(displayln "\n=== RF04: TDA Deck ===")
(display "1. deck con 60 cartas (d1): ") (displayln (deck? d1))
(display "2. deck con 60 cartas (d2): ") (displayln (deck? d2))
(display "3. ERROR deck con menos de 60 cartas: ")
(with-handlers ([exn:fail? (lambda (e) (displayln (exn-message e)))])
  (deck c1 c2 c3))

;;  RF05: shuffleDeck 
(displayln "\n=== RF05: shuffleDeck ===")
(define d1-s1 (shuffleDeck d1 123))
(define d1-s2 (shuffleDeck d1 123))
(define d1-s3 (shuffleDeck d1 999))
(display "1. Shuffle semilla 123 (carta superior): ") (displayln (card-name (deck-first-card d1-s1)))
(display "2. Misma semilla, mismo resultado (123 = 123): ") (displayln (equal? (map card-name (deck-cards d1-s1)) (map card-name (deck-cards d1-s2))))
(display "3. Shuffle semilla 999 (distinto): ") (displayln (not (equal? (map card-name (deck-cards d1-s1)) (map card-name (deck-cards d1-s3)))))

;;  RF06: initGame 
(displayln "\n=== RF06: initGame ===")
(define g-start1 (initGame d1 d2 1531312))
(define g-start2 (initGame d1 d2 4))
(define g-start3 (initGame d1 d2 123))
(display "1. Semilla 1531312 -> Inicia el juego el Jugador ") (displayln (game-current-turn g-start1))
(display "2. Semilla 123 -> Inicia el juego el Jugador ") (displayln (game-current-turn g-start3))
(display "3. Semilla 4 -> Mano generada: ") (displayln (map card-name (player-hand (game-player1 g-start2))))
(display "4. PRUEBA ESPECIAL: Primeras 7 cartas al mezclar el mazo con semilla 4: ")
(let* ([mazo-barajado (shuffleDeck d1 4)]
       [primeras-7-cartas (take (deck-cards mazo-barajado) 7)])
  (displayln (map card-name primeras-7-cartas))
  (display " - Verificando si tiene básicos: ")
  (displayln (ormap card-basic-pokemon? primeras-7-cartas)))
  (displayln " - No tiene básicos pero al usar initGame con la semilla 4 si entrega una mano válida gracias a la función sacar-mano-con-basico.")

;;  RF07: printGame 
(displayln "\n=== RF07: printGame ===")
(display "1. Imprimir vista de J1 (oculta cartas de J2):\n") 
(displayln (printGame g-start1 1))
(display "2. Imprimir vista de J2 (oculta cartas de J1):\n") 
(displayln (printGame g-start1 2))
(display "3. Imprimir juego con otra semilla inicial:\n")
(displayln (printGame g-start2 1))

;;  RF08 a RF15: Funciones de juego 
;; Para estas pruebas usaremos el juego inicial g-start1 (inicia J1)
(displayln "\n=== Pruebas de RF08 a RF15 ===")
(define p-draw (drawCardFromDeck g-start1))  ; J1 roba

(displayln "\n--- RF08: playToBench")
(define p-bench1 (playToBench p-draw c1))
(display "1. Jugar Básico a banca (Gastly): OK. Tamaño banca: ") (displayln (length (player-bench (game-player1 p-bench1))))
(display "2. ERROR Jugar carta de Energía a la banca: ")
(with-handlers ([exn:fail? (lambda (e) (displayln (exn-message e)))])
  (playToBench p-bench1 ce-psi))
(display "3. ERROR Jugar Evolución directamente a la banca (Haunter): ")
(with-handlers ([exn:fail? (lambda (e) (displayln (exn-message e)))])
  (playToBench p-bench1 c4))

(displayln "\n--- RF09: changeActivePokemon")
(define p-active1 (changeActivePokemon p-bench1 c1))
(display "1. Promover Gastly (desde banca) a activo (sin costo inicial): OK. Activo: ") (displayln (pip-name (player-active (game-player1 p-active1))))
(display "2. ERROR Promover un Pokémon que no está en la banca (Mewtwo EX): ")
(with-handlers ([exn:fail? (lambda (e) (displayln (exn-message e)))])
  (changeActivePokemon p-active1 c6))
;; Para probar retiro con costo, adjuntamos energía y probamos
(define p-b2 (playToBench p-active1 c3)) ; Charmander a banca
(define p-act2 (useEnergyCard p-b2 c1 ce-psi)) ; Gastly tiene 1 energía (costo retiro=1)
(display "3. Retirar Gastly pagando su costo de retirada (1 energía), entra Charmander: ")
(define p-active2 (changeActivePokemon p-act2 c3))
(displayln (pip-name (player-active (game-player1 p-active2))))

(displayln "\n--- RF10: drawCardFromDeck")
(display "1. Validar robar carta al inicio del turno (estado draw-done pasa a #t): ")
(displayln (game-draw-done p-draw))
(display "2. Robar carta teniendo el mazo vacío (Declara ganador al oponente): ")
  ;; Para simular mazo vacío rápidamente sin mutar la abstracción interna
  (define (robar-n-veces j n) (if (= n 0) j (robar-n-veces (drawCardFromDeck j) (- n 1))))
(define juego-vacio (robar-n-veces p-active2 48))
(display "Ganador es Jugador ")
(displayln (game-winner juego-vacio))
(display "3. Verificar que el mazo reduce su tamaño tras robar 1 vez: ")
(displayln (< (length (player-deck (game-player1 p-draw))) (length (player-deck (game-player1 g-start1)))))

(displayln "\n--- RF11: useEnergyCard")
(define e-game p-active1)
(define e-game1 (useEnergyCard e-game c1 ce-psi))
(display "1. Adjuntar 1 Energía Psíquica al activo (Gastly): OK\n")
(display "2. ERROR Adjuntar una SEGUNDA energía en el mismo turno: ")
(with-handlers ([exn:fail? (lambda (e) (displayln (exn-message e)))])
  (useEnergyCard e-game1 c1 ce-psi))
(display "3. ERROR Adjuntar energía a un Pokémon que no está en juego: ")
(with-handlers ([exn:fail? (lambda (e) (displayln (exn-message e)))])
  (useEnergyCard e-game c6 ce-psi))
(display "\n")

(displayln "\n--- RF12: evolvePokemon ")
;; Para evolucionar, el Pokémon debe llevar al menos 1 turno.
(display "1. ERROR Evolucionar un Pokémon en el mismo turno que se jugó: ")
(with-handlers ([exn:fail? (lambda (e) (displayln (exn-message e)))])
  (evolvePokemon p-bench1 c1 c4))
(display "2. ERROR Evolucionar con un Pokémon que no es su evolución directa (Gastly a Charmeleon): ")
;; Avanzamos el juego para que Gastly no sea "nuevo"
(define ev-game (usePokemonAttack e-game1 c1 null '())) ; pasa J1
(define j2-1 (drawCardFromDeck ev-game))
(define j2-2 (playToBench j2-1 c7))
(define j2-3 (changeActivePokemon j2-2 c7))
(define j2-4 (useEnergyCard j2-3 c7 ce-elec))
(define ev-game2 (usePokemonAttack j2-4 c7 null '()))
(define ev-game3 (drawCardFromDeck ev-game2)) ; J1 roba (Gastly ya no es nuevo)
(with-handlers ([exn:fail? (lambda (e) (displayln (exn-message e)))])
  (evolvePokemon ev-game3 c1 c5))
(display "3. Evolucionar Gastly a Haunter correctamente tras esperar 1 turno: ")
(define ev-ok (evolvePokemon ev-game3 c1 c4))
(displayln (pip-name (player-active (game-player1 ev-ok))))

(displayln "\n--- RF13: useTrainerCard")
(display "1. Usar Superpoción (Objeto): OK\n")
(define tr-1 (useTrainerCard ev-ok ct5 '()))
(display "2. Usar Profesora Encina (Partidario): OK\n")
(define tr-2 (useTrainerCard tr-1 ct6 '()))
(display "3. ERROR Usar otro Partidario en el mismo turno: ")
(with-handlers ([exn:fail? (lambda (e) (displayln (exn-message e)))])
  (useTrainerCard tr-2 ct6 '()))

(displayln "\n--- RF14: usePokemonAbility")
(display "1. ERROR Intentar usar habilidad de Abra ANTES de bajarlo a la mesa: ")
(with-handlers ([exn:fail? (lambda (e) (displayln (exn-message e)))])
  (usePokemonAbility tr-2 c9 '()))
(display "2. Habilidad exitosa Traspaso (ponemos Abra en banca): ")
(define ab-1 (playToBench tr-2 c2))
(define ab-2 (usePokemonAbility ab-1 c2 '()))
(displayln "OK")
(display "3. ERROR Usar habilidad de un Pokémon sin habilidades (Gastly/Haunter): ")
(with-handlers ([exn:fail? (lambda (e) (displayln (exn-message e)))])
  (usePokemonAbility ab-2 c4 '()))

(displayln "\n--- RF15: usePokemonAttack")
(display "1. Ataque null (pasar el turno sin atacar): OK\n")
(define atk-1 (usePokemonAttack ab-2 c4 null '()))
(display "2. ERROR Atacar con ataque que requiere más energías de las adjuntas (Haunter Bola Sombra necesita 2): ")
;; Haunter tiene 1 energía psíquica (heredada de Gastly). Bola Sombra necesita 2.
(define atk-j2 (drawCardFromDeck atk-1))
(define atk-j2-pass (usePokemonAttack atk-j2 c7 null '()))
(define atk-j1 (drawCardFromDeck atk-j2-pass))
(with-handlers ([exn:fail? (lambda (e) (displayln (exn-message e)))])
  (usePokemonAttack atk-j1 c4 "Bola Sombra" '()))
(display "3. Atacar correctamente tras adjuntar energía (Haunter usa Pesadilla): ")
(define atk-j1-e (useEnergyCard atk-j1 c4 ce-psi))
(define atk-ok (usePokemonAttack atk-j1-e c4 "Pesadilla" '(123)))
(displayln "OK")

(displayln "\n============================================================")
(displayln "        SIMULACIÓN COMPLETA (ENUNCIADO)")
(displayln "============================================================\n")
(displayln "Se ejecuta el caso de uso descrito en el documento original (Turnos 1 al 16). Los turnos se juegan en el código pero el printGame al final de cada turno corresponde al inicio del siguiente.")

(define g0 (initGame d1 d2 1531312))

;; T1
(displayln "===== TURNO 1 - Jugador 1 =====")
(define g1 (drawCardFromDeck g0))
(define g2 (playToBench g1 c1))
(define g3 (changeActivePokemon g2 c1))
(define g4 (useEnergyCard g3 c1 ce-psi))
(define g5 (usePokemonAttack g4 c1 null '()))
(displayln (printGame g5 1))

;; T2
(displayln "===== TURNO 2 - Jugador 2 =====")
(define g6 (drawCardFromDeck g5))
(define g7 (playToBench g6 c7))
(define g8 (changeActivePokemon g7 c7))
(define g9 (useEnergyCard g8 c7 ce-elec))
(define g10 (usePokemonAttack g9 c7 null '()))
(displayln (printGame g10 2))

;; T3
(displayln "===== TURNO 3 - Jugador 1 =====")
(define g11 (drawCardFromDeck g10))
(define g12 (playToBench g11 c3))
(define g13 (useEnergyCard g12 c1 ce-psi))
(define g14 (usePokemonAttack g13 c1 "Pesadilla" '(777)))
(displayln (printGame g14 1))

;; T4
(displayln "===== TURNO 4 - Jugador 2 =====")
(define g15 (drawCardFromDeck g14))
(define g16 (playToBench g15 c8))
(define g17 (useEnergyCard g16 c7 ce-elec))
(define g18 (usePokemonAttack g17 c7 "Impactrueno" '()))
(displayln (printGame g18 2))

;; T5
(displayln "===== TURNO 5 - Jugador 1 =====")
(define g19 (drawCardFromDeck g18))
(define g20 (evolvePokemon g19 c1 c4))
(define g21 (useEnergyCard g20 c3 ce-fire))
(define g22 (usePokemonAttack g21 c4 "Bola Sombra" '()))
(displayln (printGame g22 1))

;; T6
(displayln "===== TURNO 6 - Jugador 2 =====")
(define g23 (drawCardFromDeck g22))
(define g24 (useTrainerCard g23 ct5 (list c7 ce-elec)))
(define g24b (playToBench g24 c9))
(define g25 (usePokemonAbility g24b c9 '()))
(define g26 (useEnergyCard g25 c7 ce-elec))
(define g27 (usePokemonAttack g26 c7 "Impactrueno" '()))
(displayln (printGame g27 2))

;; T7
(displayln "===== TURNO 7 - Jugador 1 =====")
(define g28 (drawCardFromDeck g27))
(define g29 (changeActivePokemon g28 c3))
(define g30 (useEnergyCard g29 c3 ce-fire))
(define g31 (usePokemonAttack g30 c3 "Ascuas" '()))
(displayln (printGame g31 1))

;; T8
(displayln "===== TURNO 8 - Jugador 2 =====")
(define g32 (drawCardFromDeck g31))
(define g33 (changeActivePokemon g32 c8))
(define g34 (useEnergyCard g33 c8 ce-water))
(define g35 (usePokemonAttack g34 c8 "Burbuja" '(555)))
(displayln (printGame g35 2))

;; T9
(displayln "===== TURNO 9 - Jugador 1 =====")
(define g36 (drawCardFromDeck g35))
(define g37 (playToBench g36 c2))
(define g38 (useTrainerCard g37 ct2 '()))
(define g39 (useEnergyCard g38 c2 ce-psi))
(define g40 (usePokemonAttack g39 c3 "Arañazo" '()))
(displayln (printGame g40 1))

;; T10
(displayln "===== TURNO 10 - KO CHARMANDER =====")
(define g41 (drawCardFromDeck g40))
(define g42 (useTrainerCard g41 ct6 '()))
(define g43 (useEnergyCard g42 c8 ce-water))
(define g44 (usePokemonAttack g43 c8 "Pistola Agua" '()))
(displayln (printGame g44 2))

;; T11
(displayln "===== TURNO 11 - KO SQUIRTLE =====")
(define g45 (changeActivePokemon g44 c4))
(define g46 (drawCardFromDeck g45))
(define g47 (playToBench g46 c6))
(define g48 (useEnergyCard g47 c4 ce-psi))
(define g49 (usePokemonAbility g48 c2 '()))
(define g50 (usePokemonAttack g49 c4 "Bola Sombra" '()))
(displayln (printGame g50 1))

;; T12
(displayln "===== TURNO 12 - Jugador 2 =====")
(define g51 (changeActivePokemon g50 c9))
(define g52 (drawCardFromDeck g51))
(define g53 (playToBench g52 c7))
(define g54 (useEnergyCard g53 c9 ce-elec))
(define g55 (usePokemonAttack g54 c9 "Impactrueno" '()))
(displayln (printGame g55 2))

;; T13
(displayln "===== TURNO 13 - Jugador 1 =====")
(define g56 (drawCardFromDeck g55))
(define g57 (useEnergyCard g56 c6 ce-psi))
(define g58 (usePokemonAttack g57 c4 "Bola Sombra" '()))
(displayln (printGame g58 1))

;; T14
(displayln "===== TURNO 14 - Jugador 2 =====")
(define g60 (changeActivePokemon g58 c7))
(define g61 (drawCardFromDeck g60))
(define g62 (evolvePokemon g61 c7 c10))
(define g63 (useEnergyCard g62 c10 ce-elec))
(define g64 (usePokemonAttack g63 c10 null '()))
(displayln (printGame g64 2))

;; T15
(displayln "===== TURNO 15 - Jugador 1 =====")
(define g65 (drawCardFromDeck g64))
(define g66 (changeActivePokemon g65 c6))
(define g67 (useEnergyCard g66 c6 ce-psi))
(define g68 (usePokemonAttack g67 c6 "Bola Sombra" '()))
(displayln (printGame g68 1))

;; T16
(displayln "===== TURNO 16 - Jugador 2 =====")
(define g69 (drawCardFromDeck g68))
(define g70 (useEnergyCard g69 c10 ce-elec))
(define g71 (usePokemonAttack g70 c10 "Electrobola" '()))
(displayln (printGame g71 2))

(displayln "===== ESTADO FINAL =====")
(displayln "--- Vista Jugador 1 ---")
(displayln (printGame g71 1))
(displayln "--- Vista Jugador 2 ---")
(displayln (printGame g71 2))
(displayln "===== FIN DEL SCRIPT DE PRUEBAS =====")
