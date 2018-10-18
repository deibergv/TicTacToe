#lang racket

; Numero de columnas
(define columns-number 0)
(define (set-columns-number m)
  (set! columns-number m)
  columns-number)

; Numero de filas
(define rows-number 0)
(define (set-rows-number m)
  (set! rows-number m)
  rows-number)

; Contador para lineas horizontales
(define counterH -1)
(define (incrementH!)
  (set! counterH (add1 counterH))
  counterH)

; Definicion de lineas horizontales
(define (horizontalLines m n)
  (for/list ([i m])
    (for/list ([j n]);
      (incrementH!))))

; Contador para lineas verticales
(define counterV -1)
(define (incrementV!)
  (set! counterV (add1 counterV))
  counterV)

; Definicion de lineas verticales
(define (verticalLines m n)
  (transMat (for/list ([i m])
              (for/list ([j n])
                (incrementV!)))))

; Transposición de una matriz
(define (first mat)
  (cond( (null? mat) '())
       (else
        (cons (car (car mat))
              (first (cdr mat))))))
(define (deleteFirst mat)
  (cond ((null? mat) '())
        (else
         (cons (cdr (car mat))
               (deleteFirst (cdr mat))))))
(define (transMat mat)
  (cond ((null? (car mat)) '())
        (else
         (cons(first mat)
              (transMat(deleteFirst mat))))))

;; Contadores para primer diagonal
;(define counterD1 -1)
;(define (incrementD1!)
;  (set! counterD1 (add1 counterD1)))
;(define counterD2 1)
;(define (incrementD2! n)
;  (incrementD1!)
;  (set! counterD2 (* (+ n 1) counterD1))
;  counterD2)
;
;; Definicion de primer linea diagonal
;(define (diagonal1Line m n)
;    (for/list ([j n])
;      (incrementD2! n)))
;
;; Contadores para segunda diagonal
;(define counterD3 0)
;(define (incrementD3!)
;  (set! counterD3 (add1 counterD3)))
;(define counterD4 0)
;(define (incrementD4! n)
;  (incrementD3!)
;  (set! counterD4 (* (- n 1) counterD3))
;  counterD4)
;
;; Definicion de primer segunda diagonal
;(define (diagonal2Line m n)
;    (for/list ([j n])
;      (incrementD4! n)))

; Lista aux
(define listaAux1 '())
(define (add-listaAux1 n)
  (set! listaAux1 (append listaAux1 (list n)))
  listaAux1)

; Definicion de primeras linea diagonal, cuando N es mayor a M
(define (diagonalXLine1 m n)
    (for ([i (+ (abs (- m n)) 1)])
      (add-listaAux1 (for/list ([j m])
                      (+ (* (+ n 1) j) i)))) listaAux1)

; Lista aux
(define listaAux2 '())
(define (add-listaAux2 n)
  (set! listaAux2 (append listaAux2 (list n)))
  listaAux2)

; Definicion de segundas lineas diagonales, cuando N es mayor a M
(define (diagonalXLine2 m n)
    (for ([i (+ (abs (- m n)) 1)])
      (add-listaAux2 (for/list ([j m])
                      (+ (* (- n 1) j) (- n (+ i 1)))))) listaAux2)

; Lista aux
(define listaAux3 '())
(define (add-listaAux3 n)
  (set! listaAux3 (append listaAux3 (list n)))
  listaAux3)

; Definicion de primeras linea diagonal, cuando M es mayor a N
(define (diagonalXLine3 m n)
    (for ([i (+ (abs (- m n)) 1)])
      (add-listaAux3 (for/list ([j n])
                      (+ (* (+ n 1) j) (* n i))))) listaAux3)

; Lista aux
(define listaAux4 '())
(define (add-listaAux4 n)
  (set! listaAux4 (append listaAux4 (list n)))
  listaAux4)

; Definicion de primeras linea diagonal, cuando M es mayor a N
(define (diagonalXLine4 m n)
    (for ([i (+ (abs (- m n)) 1)])
      (add-listaAux4 (for/list ([j n])
                      (+ (* (- n 1) j) (- (* n (+ i 1)) 1))))) listaAux4)

; Lista de lineas de matriz
(define list-lines '())
(define (add-list-lines numbers-list)
  (set! list-lines (append list-lines numbers-list))
  list-lines)

(provide columns-number
         rows-number
         set-columns-number
         set-rows-number
         horizontalLines
         verticalLines
         ;diagonal1Line
         ;diagonal2Line
         diagonalXLine1
         diagonalXLine2
         diagonalXLine3
         diagonalXLine4
         list-lines
         add-list-lines
         )