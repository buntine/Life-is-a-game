;;; This file contains a few cool seed patterns. Some I
;;; have (re)found myself and others are famous for
;;; various reasons.
;;;
;;; A suggested display size is noted as a comment above
;;; each pattern.

;;; 20 * 20
(define glider '((3 3) (4 3) (5 3) (5 2) (4 1)))

;;; 5 * 5
(define blinker '((2 2) (3 2) (4 2)))

;;; 40 * 25 (or more)
(define gospers-glider-gun '((3 6) (4 6) (3 7) (4 7) (13 6) (13 7) (13 8) (14 5)
                             (14 9) (15 4) (15 10) (16 4) (16 10) (17 7) (18 5)
                             (18 9) (19 6) (19 7) (19 8) (20 7) (23 4) (23 5)
                             (23 6) (24 4) (24 5) (24 6) (25 3) (25 7) (27 2)
                             (27 3) (27 7) (27 8) (37 4) (37 5) (38 4) (38 5)))

;;; 18 * 18
(define koks-galaxy '((4 4) (4 5) (4 6) (4 7) (4 8) (4 9) (4 11) (4 12)
                      (5 4) (5 5) (5 6) (5 7) (5 8) (5 9) (5 11) (5 12)
                      (6 11) (6 12) (7 4) (7 5) (7 11) (7 12) (8 4) (8 5)
                      (8 11) (8 12) (9 4) (9 5) (9 11) (9 12) (10 4)
                      (10 5) (11 4) (11 5) (11 7) (11 8) (11 9) (11 10)
                      (11 11) (11 12) (12 4) (12 5) (12 7) (12 8) (12 9)
                      (12 10) (12 11) (12 12)))

;;; 18 * 14
(define pentadecathlon '((4 5) (5 5) (6 4) (6 6) (7 5) (8 5) (9 5) (10 5)
                         (11 4) (11 6) (12 5) (13 5)))

;;; 20 * 20
(define block-engine '((3 7) (5 6) (5 7) (7 3) (7 4) (7 5) (9 2) (9 3)
                       (9 4) (10 3)))

;;; 20 * 20  (Dies after about 120 evolutions)
(define bunts-exploder '((1 1) (1 2) (2 1) (3 1) (4 1) (4 2) (4 3) (4 4)))
