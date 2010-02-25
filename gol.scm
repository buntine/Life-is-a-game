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
;;;
;;; Usage:
;;;
;;;   (gol 20 20 '((10 10) (11 10) (12 10) (13 10) (14 10) (11 10)))
;;;
;;; Press ESC during game to exit.

(require (lib "graphics.ss" "graphics"))

(define *CELL_WIDTH* 5)
(define *CELL_HEIGHT* 5)
(define *REFRESH_RATE* .050)

(define play #t)

;;; Given a grid, returns the number of available rows.
(define (rows grid)
  (vector-length grid))

;;; Given a grid, returns the number of available cells
;;; per row.
(define (cells grid)
  (vector-length (vector-ref grid 0)))

;;; Initializes the graphics viewport and key-event
;;; bindings.
(define (initialize cells rows)
  (open-graphics)
  (let ((vp (open-viewport "Conway's Game of Life"
                           (* cells *CELL_WIDTH*)
                           (* rows *CELL_HEIGHT*))))
    (set! play #t)
    ((set-on-key-event vp) (lambda (ke v)
                             (if (equal? (key-value ke) 'escape)
                               (set! play #f))))
    vp))

;;; Creates a grid and populates it as per the given
;;; seed pattern for the next evolution.
(define (update-grid cells rows seed)
  (let ((grid (build-grid (new-grid rows) cells 0)))
    (populate grid seed)))
;  (build-grid cells (new-grid rows) 0 seed))

(define (build-grid rows row-width index)
  (if (= (vector-length rows) (+ index 1))
    rows
    (begin
      (vector-set! rows index (new-row row-width))
      (build-grid rows row-width (+ index 1)))))

;;; Builds the full grid as per the supplied
;;; width, height and seed pattern.
;(define (build-grid cells rows index seed)
;  (let ((row (populate-row (new-row cells)
;                           0
;                           index
;                           seed)))
;    (vector-set! rows index row)
;    (if (= (vector-length rows) (+ index 1))
;      rows
;      (build-grid cells rows (+ index 1) seed))))

(define (populate grid seed)
  (if (null? seed)
    grid
    (let ((x (car-x seed))
          (y (car-y seed)))
      (vector-set! (vector-ref grid y) x 1)
      (populate grid (cdr seed)))))

;;; Populates a given row in the grid given
;;; the initial seed pattern.
;(define (populate-row row cell-index row-index seed)
;  (if (= cell-index (vector-length row))
;    row
;    (let ((health (if (member (list cell-index row-index) seed) 1 0)))
;      (vector-set! row cell-index health)
;      (populate-row row (+ cell-index 1) row-index seed))))

;;; Returns the next generation, in the form of the new grid
;;; and seed, given the current state.
(define (next-generation grid seed)
  (let ((new-seed (build-seed-pattern grid seed)))
    (list (update-grid (cells grid)
                       (rows grid)
                       new-seed)
          new-seed)))

;;; Builds a new seed pattern in relation to the current
;;; game state.
(define (build-seed-pattern grid seed)
  (b-s-p-helper '() grid seed))

(define (b-s-p-helper new-seed grid old-seed)
  (if (null? old-seed)
    new-seed
    (let ((x (car-x old-seed))
          (y (car-y old-seed)))
      (b-s-p-helper (append new-seed
                            (inspect-block '() grid (grid-block x y) new-seed))
                    grid
                    (cdr old-seed)))))

;;; Returns a list containing the locations for all neighbouring
;;; cells of the cell at x*y.
(define (grid-block x y)
  `((,(- x 1) ,(- y 1))
    (,x ,(- y 1))
    (,(+ x 1) ,(- y 1))
    (,(- x 1) ,y)
    (,x ,y)
    (,(+ x 1) ,y)
    (,(- x 1) ,(+ y 1))
    (,x ,(+ y 1))
    (,(+ x 1) ,(+ y 1))))

;;; Inspects a nine-cell block and returns a list of x*y for those
;;; that should live on to the next generation.
(define (inspect-block healthy grid cells seed)
  (if (null? cells)
    healthy
    (let* ((x (car-x cells))
           (y (car-y cells))
           (c (fetch-cell grid x y))
           (n (cell-neighbours grid x y))
           (pattern (if (and (not (member (car cells) seed))
                             (cell-lives? c n))
                      (cons (car cells) healthy)
                      healthy)))
      (inspect-block pattern grid (cdr cells) seed))))

;;; Predicate, returns true if the given cell should live
;;; onto the next evolution.
(define (cell-lives? cell neighbours)
  (cond ((and (> neighbours 3) (alive? cell)) #f)
        ((and (< neighbours 2) (alive? cell)) #f)
        ((and (= neighbours 3) (dead? cell)))
        ((and (> neighbours 1) (> 4 neighbours) (alive? cell)))
        (else #f)))
 
;;; Returns the value of the cell at position x y
;;; in the grid. If out-of-bounds, 0 is returned.
(define (fetch-cell grid x y)
  (if (or (< x 0) (< y 0) (>= x (cells grid)) (>= y (rows grid)))
    0
    (vector-ref (vector-ref grid y) x)))

;;; Returns the number of live neighbours for the
;;; cell at the given coordinates.
(define (cell-neighbours grid x y)
  (let ((cell-value (lambda (pos) (fetch-cell grid (car pos) (cadr pos)))))
    (- (apply + (map cell-value (grid-block x y)))
       (fetch-cell grid x y))))

;;; Renders the universe, depicting the current state.
(define (render-universe seed vp)
  (if (null? seed)
    #t
    (let* ((cw *CELL_WIDTH*)
           (ch *CELL_HEIGHT*)
           (x (car-x seed))
           (y (car-y seed))
           (posn (make-posn (* cw x) (* ch y))))
      ((draw-solid-rectangle vp) posn cw ch "black")
      (render-universe (cdr seed) vp))))

;;; True if x*y represents the last cell in the grid.
(define (end-of-grid? grid x y)
  (and (= (+ y 1) (rows grid))
       (= x (cells grid))))

;;; True if x represents the last cell in any row.
(define (end-of-row? grid x)
  (= x (cells grid)))

(define (alive? cell)
  (= cell 1))

(define (dead? cell)
  (= cell 0))

(define (car-x seed)
  (car (car seed)))

(define (car-y seed)
  (cadr (car seed)))

(define (new-row len)
  (make-vector len 0))

(define (new-grid len)
  (new-row len))

;;; Main game loop.
(define (mainloop grid seed vp)
  ((clear-viewport vp))
  (render-universe seed vp)
  (sleep/yield *REFRESH_RATE*)
  (if play
    (apply mainloop `(,@(next-generation grid seed) ,vp))
    (close-graphics)))

;;; Initialization procedure, accepts width, height and
;;; an initial seed pattern in the form of a list of
;;; two-element sublists.
;;;
;;; Example: (gol 40 40 '((22 2) (23 2) (22 3) (23 3)))
(define (gol cells rows seed)
  (mainloop (update-grid cells rows seed)
            seed
            (initialize cells rows)))
