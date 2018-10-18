#lang racket/gui   ;PRUEBAS UNITARIAS EN LA LINEA 147 "(unix-test n)"
; Importacion de archivos necesarios para la interfaz
(require graphics/graphics)(open-graphics)
(require htdp/matrix)
(require "Greedy-Algorithm.rkt")
(require "MatrixCreator.rkt")
(require "Unit-tests.rkt")

(define PXF 0)
(define PYF 0)
(define index 0)
(define (set-index n)
  (set! index n))
(define flag 1)
(define (set-flag n)
  (set! flag n) flag)

; Creacion de la ventana
(define window open-viewport)
(define (set-window open-view-port)
  (set! window open-view-port))

; Deteccion de juego hecho por la persona
(define (player m n)
  (cond [(equal? flag 0) (println "Come back soon! :)")]
        [else (define click (mouse-click-posn (get-mouse-click window)))
              (set! PXF (con (posn-x click)))
              (set! PYF (con (posn-y click)))
              (playing PXF PYF n m)]))
  
; Cambio de turno y modificacion del juego
(define (playing PXF PYF m n)
  (cond ((or (or (< (* 60 PXF) 60) (> PXF m )) (or (< (* 60 PYF) 60) (> PYF n))) (player m n))
        (else (let ([index (index-from-mouse-click PXF PYF)])
                ; Cambio segun indice de "vacio" por 'X o por 'O en el vector del juego
                (cond [(empty? (vector-ref game index))
                       (set-game-symbol 'X index)
                (cond [(game-finished? game) (restart-or-return-this game)]
                      [else (let ([after-computer-state (play game)])
                              (set-game after-computer-state)
                              (draw)
                              (cond [(game-finished? after-computer-state) (restart-or-return-this after-computer-state)]))])])
                (player m n)))))

(define (index-from-mouse-click-aux x y)
  (cond [(or (> x 7) (> y 4)) -1]
        [else (index-from-mouse-click x y)]))

; Dibujado de simbolos segun matriz de juego
(define (draw)
  (let ([mat (vector->list game)])
  (for ([i (* columns-number rows-number)])
    (draw-in-matrix i (list-ref mat i)))))

; Dibujo de simbolo segun un indice
(define (draw-in-matrix ind symb)
  (for ([i columns-number]) ;Y
    (for ([j rows-number]) ;X
      (set-index (index-from-mouse-click-aux (+ 1 j)(+ 1 i)))
      (cond [(equal? index ind)
             (cond [(equal? symb 'X)
                    (draw-x (+ 1 j) (+ 1 i))]
                   [(equal? symb 'O)
                    (draw-o (+ 1 j) (+ 1 i))])]))))

; Dibujado de "X" en el tablero
(define (draw-x PX PY)
  ((draw-pixmap window) "X.png" (make-posn (+ (* 60 PX) 5) (+ (* 60 PY) 5))))

; Dibujado de "O" en el tablero
(define (draw-o PX PY)
  ((draw-pixmap window) "O.png" (make-posn (+ (* 60 PX) 5) (+ (* 60 PY) 5))))

(define (con x)
  (floor ( / x 60)))

; Se obtiene un indice segun el cuadro al cual se le da clic
(define (index-from-mouse-click x y)
  (cond [(equal? rows-number columns-number)
         ;(println  (- (+ (* columns-number y) x) ( + 1 columns-number) ))
         (- (+ (* columns-number y) x) ( + 1 columns-number))]
        [else (let()
                ;(println  (- (+ (* rows-number y) x) ( + 1 rows-number) ))
                (- (+ (* rows-number y) x) ( + 1 rows-number)))]))

; Verificacion de si el juego ya está finalizado
(define (get-end-game-message game)
  (cond
    [(equal? (game-evaluate game) 'O) "You lost. It's OK, the AI is perfect."]
    [(equal? (game-evaluate game) 'X) "You won?! Whut?!"]
    [else "You tied. Well done."]))

; Reacreacion de juego para volver a jugar
(define (restart-or-return-this a-game)
  (if (should-restart? a-game) (play-again) (close)))

; Pregunta si se quiere volver a jugar
(define (should-restart? a-game)
  (equal? 1 (message-box/custom "The End" (get-end-game-message a-game) "Play Again" "Quit" #f)))

; Funcion encargada de recrear el juego
(define (play-again)
  (make-empty-game (* columns-number rows-number))
  (empty-board))

; Limpieza de la ventana de juego (del tablero)
(define (empty-board)
  (close-viewport window)
  (set-window (open-viewport "TicTacToe" (* 60 (+ 2 rows-number)) (* 60 (+ 2 columns-number))))
  (make-board columns-number rows-number))

; Cierre de la ventana
(define (close)
  (close-viewport window)
  (set-flag 0))

; Llamados a la creacion del tablero
(define (make-board m n)
  (make-column m n 1 1))

; Creacion de filas del tablero
(define (make-row n m rowsN columnsN)
  (cond((equal? (+ 1 m) rowsN))
       (else (let()
               ((draw-rectangle window) (make-posn (* rowsN 60) (* columnsN 60)) 60 60)
               (make-row n m (+ 1 rowsN) columnsN)))))
; Creacion de columnas del tablero
(define (make-column n m columnsN rowsN)
  (cond((equal? (+ 1 n) columnsN))
       (else (let()
               (make-row n m rowsN columnsN)
               ((draw-rectangle window) (make-posn  (* rowsN 60) (* columnsN 60)) 60 60)
               (make-column n m (+ 1 columnsN) rowsN)))))

; Llamado al juego ^_^!
(define (TTT m n)
  (make-empty-game (* m n))
  (make-list-lines m n)
  (set-columns-number m)
  (set-rows-number n)
  (cond ((or (or (<= m 2) (>= m 11)) (or (<= n 2) (>= n 11)))
         (error "Column or Row out of range!"))
        (else( let()
                (set-window (open-viewport "TicTacToe" (* 60 (+ 2 n)) (* 60 (+ 2 m))))
                (make-board m n)
                ; Llamado a realizar una prueba "#" para cargar en el tablero ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                (unitTest 5)
                (player m n)
                ))))

; Conexion con las pruebas unitarias
(define (unitTest n)
  (set-game (unit-test n))
  (draw))