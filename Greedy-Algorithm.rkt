; Elaborado por Deiber Granados V.
; Implementación de "Algoritmo Voraz" para el juego tic-tac-toe, MxN.

#lang racket
; Importacion de archivo utilizado para la creacion de la matriz
(require "MatrixCreator.rkt")

; Definicion de Simbolos base
(define X-symb 'X)
(define O-symb 'O)
(define tie-symb 'T)

; Asigacion de Puntajes base segun el simbolo
(define X-score 100.0)
(define O-score -100.0)
(define tie-score 0.0)

; El factor por el cual las puntuaciones del juego disminuyen después
; de cada turno. Esto es importante para que la computadora intente
; ganar lo antes posible
(define diminishing-factor 0.9)

(define X-selector argmax)
(define O-selector argmin)

; Se crea y devuelve una lista con cada linea posible de la matriz
(define (make-list-lines m n)
  (add-list-lines (append
                   (horizontalLines m n)
                   (verticalLines m n)))
  (cond ((< m n)
         (add-list-lines (append
                          (diagonalXLine1 m n)
                          (diagonalXLine2 m n))))
        (else (add-list-lines (append
                               (diagonalXLine3 m n)
                               (diagonalXLine4 m n))))
        )list-lines)

; Se crea y devuelve un juego vacío
(define game '#())
(define (make-empty-game n)
  (set! game (make-vector n empty)) game)
(define (set-game-symbol symbol index)
  (set! game (vector-copy-and-replace game index symbol)) game)
(define (set-game derivate-game)
  (set! game derivate-game))

; Conteo de cuántas veces ha jugado el símbolo especificado.
(define (count-plays a-game symb)
  (vector-count (lambda (s) (equal? s symb)) a-game))

; Determina el siguiente jugador al que le toca jugar.
; En esta función se tiene que X siempre es el que comienza.
(define (determine-next-to-play a-game)
  (if (> (count-plays a-game X-symb) (count-plays a-game O-symb)) O-symb X-symb))

; Hace una copia del vector de entrada y reemplaza el elemento
; en "pos" por "v" antes de devolverlo.
(define (vector-copy-and-replace vec pos v)
  (vector-append (vector-take vec pos) (vector v) (vector-drop vec (+ pos 1))))

; Devuelve una copia del jugador de simbolo "symb" en "pos".
(define (game-after-replacing a-game symb pos)
  (vector-copy-and-replace a-game pos symb))

; Devuelve una lista con posibles juegos generados al reemplazar
; "symb" en las posiciones posibles (pos-list).
(define (list-games-after-replacing a-game symb pos-list)
  (map (lambda (i) (game-after-replacing a-game symb i)) pos-list))

; Devuelve una lista con todos los juegos posibles que un jugador
; puede generar en su próximo movimiento.
(define (derive-games-for-player a-game symb)
  (list-games-after-replacing a-game symb (filter (lambda (i) (empty? (vector-ref a-game i))) (range (* columns-number rows-number)))))

; Juegos que pueden derivarse del estado del juego especificado
; en un solo turno.
(define (derive-games a-game)
  (derive-games-for-player a-game (determine-next-to-play a-game)))

; Comprueba que todos los elementos (simbolos) de una lista
; sean iguales
(define (list-equal? lst)
  (define (helper el lst)
    (or (null? lst)
        (and (equal? el (car lst))
             (helper (car lst) (cdr lst)))))
  (or (null? lst)
      (helper (car lst) (cdr lst))))

; Comprueba si las posiciones del vector son símbolos iguales.
(define (equal-symbols? lst)
  ; También se comprueba que estos son símbolos y no otra cosa.
  (and (andmap symbol? lst)
       (list-equal? lst)))

; Llenado de una lista de simbolos segun indices de una linea
(define (make-symbols-list a-vector line)
  (for/list ([i line])
    (vector-ref a-vector i)))

; Evalúa el juego para la lista especificada de combinaciones
; de simbolos, como puede serlo 3 en diagonal. O bien devuelve
; el símbolo del que ya obtuvo lo necesario para ganar ó
; devuelve "vacío" si nadie logró ganar.
(define (game-evaluate-lines a-game lines-of-game)
  (foldl
   (lambda (line current-result)
     (if (empty? current-result)
         ; Intenta encontrar un nuevo resultado si aún
         ; no se ha obtenido la cantidad necesaria de simbolos.
         (if (equal-symbols? (make-symbols-list a-game line))
             (vector-ref a-game (first line))
             empty)
         current-result))
   empty
   lines-of-game))

; Evalúa el juego para los resultados en todas las direcciones
; válidas, devuelve el símbolo del ganador si ya se llega a eso,
; el símbolo de empate o vacío si el juego aún no ha terminado.
(define (game-evaluate a-game)
  (let ([evaluation (game-evaluate-lines a-game list-lines)])    
    (cond
      [(and (empty? evaluation) (game-full? a-game)) tie-symb]
      [else evaluation])))

; Evalúa la puntuación del juego en general.
(define (game-score a-game)
  (let
      ([evaluation (game-evaluate a-game)])
    (cond
      [(equal? evaluation X-symb) X-score]
      [(equal? evaluation tie-symb) tie-score]
      [(empty? evaluation) ((selector-from-player (determine-next-to-play a-game)) identity (map (lambda (g) (* diminishing-factor (game-score g))) (derive-games a-game)))]
      [else O-score])))

; Devuelve si el juego ha sido ganado por un jugador o no.
(define (game-won? a-game)
  (let ([evaluation (game-evaluate a-game)])
    (or (equal? X-symb evaluation) (equal? O-symb evaluation))))

; Devuelve si no hay más espacios vacíos en el tablero.
(define (game-full? a-game)
  (zero? (vector-count empty? a-game)))

; Devuelve si el juego está lleno o no.
(define (game-finished? a-game)
  ; Para que el juego termine, debe haber un ganador ó
  ; que no queden más movimientos.
  (or (game-won? a-game) (game-full? a-game)))

; Devuelve la puntuación del simbolo de un jugador.
(define (selector-from-player player)
  (cond
    [(equal? player X-symb) X-selector]
    [else O-selector]))

; Obtiene el juego óptimo para la computadora.
(define (play a-game)
  (if (not (game-finished? a-game))
      ; Devuelve el juego derivado con la puntuación más alta.
      ((selector-from-player (determine-next-to-play a-game)) (lambda (g) (game-score g)) (derive-games a-game))
      a-game))

; Resuelve el juego dado. Esto resulta en un empate o
; una victoria para cualquier jugador tenía una ventaja en el juego.
(define (solve a-game)
  (if (game-finished? a-game)
      a-game
      (solve (play a-game))))

(provide make-list-lines
         vector-copy-and-replace
         game
         set-game-symbol
         set-game
         game-score
         game-finished?
         game-evaluate
         make-empty-game
         play
         solve)