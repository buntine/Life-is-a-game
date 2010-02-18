;;; Conway's Game of Life
;;; Andrew Buntine, 2010
;;;
;;; The Game of Life, also known simply as Life, is a cellular automaton
;;; devised by the British mathematician John Horton Conway in 1970. It is
;;; the best-known example of a cellular automaton.
;;;
;;; The "game" is a zero-player game, meaning that its evolution is determined
;;; by its initial state, requiring no further input from humans. One
;;; interacts with the Game of Life by creating an initial configuration
;;; and observing how it evolves.
;;;
;;; Read about it on Wikipedia: http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life

(require (lib "graphics.ss" "graphics"))

(define *HORIZ_CELLS* 20)
(define *VERT_CELLS* 10)
(define *CELL_WIDTH* 5)
(define *CELL_HEIGHT* 5)
(define *REFRESH_RATE* .60)

(define (game-width)
  (* *HORIZ_CELLS* *CELL_WIDTH*))

(define (game-height)
  (* *VERT_CELLS* *CELL_HEIGHT*))

;;; Initializes the graphics viewport.
(define (initialize)
  (open-graphics)
  (open-viewport "Conway's Game of Life" (game-width) (game-height)))

;;; Creates an initial seed pattern.
(define (initial-seed)
  (vector
    (make-vector 20 0)
    (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0)
    (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0)
    (vector 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (vector 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (make-vector 20 0)
    (make-vector 20 0)
    (make-vector 20 0)
    (make-vector 20 0)
    (make-vector 20 0)))

;;; Returns the next generation given the current state.
(define (next-generation state)
  (initial-seed))

;;; Renders the universe, depicting the current state.
(define (render-universe state vp)
  (r-u-helper state 0 0 vp))

(define (r-u-helper state x y vp)
  (cond ((>= y *VERT_CELLS*)
          #t)
        ((>= x *HORIZ_CELLS*)
          (r-u-helper state 0 (+ y 1) vp))
        (else
          (let ((health (vector-ref
                          (vector-ref state y)
                          x)))
            (if (= health 1)
              (render-life x y vp)
              (render-death x y vp))
            (r-u-helper state (+ x 1) y vp)))))

(define (render-life x y vp)
  (let ((posn (make-posn
                (* *CELL_WIDTH* x)
                (* *CELL_HEIGHT* y))))
    ((draw-solid-rectangle vp) posn *CELL_WIDTH* *CELL_HEIGHT* "black")))

(define (render-death x y vp)
  (let ((posn (make-posn
                (* *CELL_WIDTH* x)
                (* *CELL_HEIGHT* y))))
    ((draw-solid-rectangle vp) posn *CELL_WIDTH* *CELL_HEIGHT* "white")))


;;; Main game loop.
(define (mainloop state vp)
  (let ((new-state (next-generation state)))
    (render-universe new-state vp)
    (sleep/yield *REFRESH_RATE*)
    (mainloop new-state vp)))

(define (gol)
  (mainloop (initial-seed) (initialize)))
