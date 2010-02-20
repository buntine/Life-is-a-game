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

;;; TODO: Refactor b-s-p-helper
;;;       Enable way to kill app and (close-graphics)
;;;       Write domain-specific wrappers for common procedures.

(require (lib "graphics.ss" "graphics"))

(define *CELL_WIDTH* 5)
(define *CELL_HEIGHT* 5)
(define *REFRESH_RATE* .120)

;;; Given a grid, returns the number of available rows.
(define (rows grid)
  (vector-length grid))

;;; Given a grid, returns the number of available cells
;;; per row.
(define (cells grid)
  (vector-length (vector-ref grid 0)))

;;; Initializes the graphics viewport.
(define (initialize cells rows)
  (open-graphics)
  (open-viewport "Conway's Game of Life"
                 (* cells *CELL_WIDTH*)
                 (* rows *CELL_HEIGHT*)))

;;; Creates a grid and populates it as per the given
;;; seed pattern for the next evolution.
(define (update-seed cells rows seed)
  (build-grid cells (make-vector rows 0) 0 seed))

;;; Builds the full grid as per the supplied
;;; width, height and seed pattern.
(define (build-grid cells rows curr-row seed)
  (vector-set! rows
               curr-row
               (populate-row (make-vector cells 0)
                             0
                             curr-row
                             seed))
  (if (= (vector-length rows) (+ curr-row 1))
    rows
    (build-grid cells rows (+ curr-row 1) seed)))

;;; Populates a given row in the grid given
;;; the initial seed pattern.
(define (populate-row row curr-cell curr-row seed)
  (if (= curr-cell (vector-length row))
    row
    (let ((health (if (member (list curr-cell curr-row) seed) 1 0)))
      (vector-set! row curr-cell health)
      (populate-row row (+ curr-cell 1) curr-row seed))))

;;; Returns the next generation given the current state.
(define (next-generation grid)
  (update-seed (cells grid)
               (rows grid)
               (build-seed-pattern grid)))

;;; Builds a new seed pattern in relation to the current
;;; game state.
(define (build-seed-pattern grid)
  (b-s-p-helper '() grid 0 0))

(define (b-s-p-helper seed state x y)
  (cond ((and (= (+ y 1) (rows state))
              (= x (cells state)))
           seed)
        ((= x (cells state))
           (b-s-p-helper seed state 0 (+ y 1)))
        (else
          (let ((cell (fetch-cell state x y))
               (neighbours (cell-neighbours state x y)))
            (b-s-p-helper (cond ((and (> neighbours 3) (= cell 1)) seed)
                                ((and (< neighbours 2) (= cell 1)) seed)
                                ((and (= neighbours 3) (= cell 0)) (cons (list x y) seed))
                                ((and (> neighbours 1) (> 4 neighbours) (= cell 1)) (cons (list x y) seed))
                                (else seed))
                          state
                          (+ x 1)
                          y)))))

;;; Returns the value of the cell at position x y
;;; in the grid. If out-of-bounds, 0 is returned.
(define (fetch-cell grid x y)
  (if (or (< x 0) (< y 0) (>= x (cells grid)) (>= y (rows grid)))
    0
    (vector-ref (vector-ref grid y) x)))

;;; Returns the number of live neighbours for the
;;; cell at the given coordinates.
(define (cell-neighbours grid x y)
  (let ((neighbours-sum (lambda (row)
                          (+ (fetch-cell grid (- x 1) row)
                             (fetch-cell grid x row)
                             (fetch-cell grid (+ x 1) row)))))
    (+ (fetch-cell grid (- x 1) y)
       (fetch-cell grid (+ x 1) y)
       (neighbours-sum (- y 1))
       (neighbours-sum (+ y 1)))))

;;; Renders the universe, depicting the current state.
(define (render-universe grid vp)
  (r-u-helper grid 0 0 vp))

(define (r-u-helper grid x y vp)
  (cond ((>= y (rows grid)) #t)
        ((>= x (cells grid))
          (r-u-helper grid 0 (+ y 1) vp))
        (else
          (let ((health (fetch-cell grid x y)))
            (render-cell health x y vp)
            (r-u-helper grid (+ x 1) y vp)))))

;;; Renders a cell to be either alive or dead, depending
;;; the the value of 'health'.
(define (render-cell health x y vp)
  (let* ((cw *CELL_WIDTH*)
         (ch *CELL_HEIGHT*)
         (color (if (= health 1)
                  "black"
                  "white"))
         (posn (make-posn (* cw x) (* ch y))))
    ((draw-solid-rectangle vp) posn cw ch color)))

;;; Main game loop.
(define (mainloop grid vp)
  (render-universe grid vp)
  (sleep/yield *REFRESH_RATE*)
  (mainloop (next-generation grid) vp))

;;; Initialization procedure, accepts width, height and
;;; an initial seed pattern in the form of a list of
;;; two-element lists.
;;;
;;; Example: (gol 40 40 '((22 2) (23 2) (22 3) (23 3)))
(define (gol cells rows seed)
  (mainloop (update-seed cells rows seed)
            (initialize cells rows)))
