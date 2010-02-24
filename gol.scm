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


;;;; max-x and max-y SHOULD ACTUALLY WORK
;;;; render-universe SHOULD BUILD FROM SEED PATTERN, RECURSIVELY.

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
(define (update-seed cells rows seed)
  (build-grid cells (new-grid rows) 0 seed))

;;; Builds the full grid as per the supplied
;;; width, height and seed pattern.
(define (build-grid cells rows index seed)
  (let ((row (populate-row (new-row cells)
                           0
                           index
                           seed)))
    (vector-set! rows index row)
    (if (= (vector-length rows) (+ index 1))
      rows
      (build-grid cells rows (+ index 1) seed))))

;;; Populates a given row in the grid given
;;; the initial seed pattern.
(define (populate-row row cell-index row-index seed)
  (if (= cell-index (vector-length row))
    row
    (let ((health (if (member (list cell-index row-index) seed) 1 0)))
      (vector-set! row cell-index health)
      (populate-row row (+ cell-index 1) row-index seed))))

;;; Returns the next generation given the current state.
(define (next-generation grid seed)
  (let ((new-seed (build-seed-pattern grid seed)))
    (list (update-seed (cells grid)
                       (rows grid)
                       new-seed)
          new-seed)))

;;; Builds a new seed pattern in relation to the current
;;; game state.
(define (build-seed-pattern grid seed)
  (display (length seed))
  (display ",")
  (b-s-p-helper '() grid seed))

(define (b-s-p-helper new-seed grid old-seed)
  (if (null? old-seed)
    new-seed
    (let ((x (car (car old-seed)))
          (y (cadr (car old-seed))))
      (b-s-p-helper (append new-seed
                            (inspect-block '() grid `((,(- x 1) ,(- y 1))
                                                      (,x ,(- y 1))
                                                      (,(+ x 1) ,(- y 1))
                                                      (,(- x 1) ,y)
                                                      (,x ,y)
                                                      (,(+ x 1) ,y)
                                                      (,(- x 1) ,(+ y 1))
                                                      (,x ,(+ y 1))
                                                      (,(+ x 1) ,(+ y 1))) new-seed))
                    grid
                    (cdr old-seed)))))

(define (inspect-block healthy grid cells seed)
  (if (null? cells)
    healthy
    (let* ((x (car (car cells)))
           (y (cadr (car cells)))
           (c (fetch-cell grid x y))
           (n (cell-neighbours grid x y))
           (pattern (if (and (not (member (car cells) seed)) (cell-lives? c n))
                      (cons (car cells) healthy)
                      healthy)))
      (inspect-block pattern grid (cdr cells) seed))))

;  (cond ((end-of-grid? grid x y) seed)
;        ((end-of-row? grid x)
;          (b-s-p-helper seed grid 0 (+ y 1)))
;        (else
;          (let* ((c (fetch-cell grid x y))
;                 (n (cell-neighbours grid x y))
;                 (pattern (if (cell-lives? c n)
;                            (cons (list x y) seed)
;                            seed)))
;            (b-s-p-helper pattern grid (+ x 1) y)))))

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
  (- (+ (rl-cell grid x (- y 1))
        (rl-cell grid x y)
        (rl-cell grid x (+ y 1)))
     (fetch-cell grid x y)))

;;; A helper function to cell-neighbours. Returns
;;; the combined values of the cells at (x-1)*y, x*y
;;; and x+1*y
(define (rl-cell grid x y)
  (+ (fetch-cell grid (- x 1) y)
     (fetch-cell grid x y)
     (fetch-cell grid (+ x 1) y)))

;;; Renders the universe, depicting the current state.
(define (render-universe seed vp)
  (if (null? seed)
    #t
    (let* ((cw *CELL_WIDTH*)
           (ch *CELL_HEIGHT*)
           (x (car (car seed)))
           (y (cadr (car seed)))
           (posn (make-posn (* cw x) (* ch y))))
      ((draw-solid-rectangle vp) posn cw ch "black")
      (render-universe (cdr seed) vp))))
;  (r-u-helper grid 0 0 vp))

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
  (mainloop (update-seed cells rows seed)
            seed
            (initialize cells rows)))
