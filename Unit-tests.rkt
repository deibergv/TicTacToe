#lang racket

(define (unit-test n)
  (cond [(eq? n 1) ;Test #1, 4x4
                   '#(X O O X
                      X X O O
                      O X O ()
                      X () () ())]
        [(eq? n 2) ;Test #2, otro 4x4
                   '#(X O O X
                     () X X O
                     () O O O
                      X X () ())]
        [(eq? n 3) ;Test #3, 4x3
                   '#(X O O
                      O () ()
                      X X O
                      () X ())]
        [(eq? n 4) ;Test #4, 3x4
                   '#(O X O ()
                      O X () ()
                      X O () X)]
        [(eq? n 5) ;Test #5, 4x5
                   '#(X O () O O
                      X X () X ()
                     () () () () O
                      O () X () ())]
  ))

(provide unit-test)