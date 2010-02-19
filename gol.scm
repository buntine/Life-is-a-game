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

(define *CELL_WIDTH* 5)
(define *CELL_HEIGHT* 5)
(define *REFRESH_RATE* .250)

(define (rows grid)
  (vector-length grid))

(define (cells grid)
  (vector-length (vector-ref grid 0)))

(define (frame-width rows)
  (* rows *CELL_WIDTH*))

(define (frame-height cells)
  (* cells *CELL_HEIGHT*))

;;; Initializes the graphics viewport.
(define (initialize rows cells)
  (open-graphics)
  (open-viewport "Conway's Game of Life"
                 (frame-width rows)
                 (frame-height cells)))

;;; Creates an initial seed pattern.
(define (initial-seed rows cells seed)
  (build-grid (make-vector rows 0) cells 0 seed))

;;; Builds the full grid as per the supplied
;;; width, height and seed pattern.
(define (build-grid rows cells curr-row seed)
  (vector-set! rows
               curr-row
               (populate-row (make-vector cells 0)
                             0
                             curr-row
                             seed))
  (if (= (vector-length rows) (+ curr-row 1))
    rows
    (build-grid rows
                cells
                (+ curr-row 1)
                seed)))

;;; Populates a given row in the grid given
;;; the initial seed pattern.
(define (populate-row row curr-cell curr-row seed)
  (if (= curr-cell (vector-length row))
    row
    (begin
      (vector-set! row
                   curr-cell
                   (if (member (list curr-cell curr-row) seed) 1 0))
      (populate-row row (+ curr-cell 1) curr-row seed))))

;;; Returns the next generation given the current state.
(define (next-generation state)
  (let ((rows (vector-length state))
        (cells (vector-length (vector-ref state 0))))
    (initial-seed rows
                  cells
                  (build-seed-pattern state))))

;;; Builds a new seed pattern in relation to the current
;;; state.
(define (build-seed-pattern state)
  (b-s-p-helper '() state 0 0))

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
                                ((and (>= neighbours 3) (= cell 0)) (cons (list x y) seed))
                                ((and (> neighbours 1) (> 4 neighbours) (= cell 1)) (cons (list x y) seed))
                                (else seed))
                          state
                          (+ x 1)
                          y)))))

;;; Returns the value of the cell at position x y
;;; in the grid.
(define (fetch-cell grid x y)
  (if (or (< x 0) (< y 0) (>= x (cells grid)) (>= y (rows grid)))
    0
    (vector-ref (vector-ref grid y) x)))

;;; Returns the number of live neighbours for the
;;; cell at the given coordinates.
(define (cell-neighbours grid x y)
  (let ((neighbours (list (fetch-cell grid (- x 1) (- y 1))
                    (fetch-cell grid x (- y 1))
                    (fetch-cell grid (+ x 1) (- y 1))
                    (fetch-cell grid (- x 1) (+ y 1))
                    (fetch-cell grid x (+ y 1))
                    (fetch-cell grid (+ x 1) (+ y 1))
                    (fetch-cell grid (- x 1) y)
                    (fetch-cell grid (+ x 1) y))))
    (apply + neighbours)))

;;; Renders the universe, depicting the current state.
(define (render-universe state vp)
  (r-u-helper state 0 0 vp))

(define (r-u-helper state x y vp)
  (let ((rows (vector-length state))
        (cells (vector-length (vector-ref state 0))))
    (cond ((>= y rows)
            #t)
          ((>= x cells)
            (r-u-helper state 0 (+ y 1) vp))
          (else
            (let ((health (vector-ref (vector-ref state y)
                                      x)))
              (if (= health 1)
                (render-cell 'life x y vp)
                (render-cell 'death x y vp))
              (r-u-helper state (+ x 1) y vp))))))

;;; Renders a cell to be either alive or dead, depending
;;; the the value of 'health'.
(define (render-cell health x y vp)
  (let* ((cw *CELL_WIDTH*)
         (ch *CELL_HEIGHT*)
         (color (if (equal? health 'life)
                  "black"
                  "white"))
         (posn (make-posn (* cw x)
                          (* ch y))))
    ((draw-solid-rectangle vp) posn cw ch color)))

;;; Main game loop.
(define (mainloop state vp)
  (render-universe state vp)
  (sleep/yield *REFRESH_RATE*)
  (mainloop (next-generation state) vp))

;;; Initialization procedure, accepts width, height and
;;; an initial seed pattern in the form of a list of
;;; two-element lists.
;;;
;;; Example: (gol 40 40 '((22 2) (23 2) (22 3) (23 3)))
(define (gol rows cells seed)
  (mainloop (initial-seed rows cells seed)
            (initialize rows cells)))
