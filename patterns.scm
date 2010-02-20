;;; This file contains a few cool patterns. Some I have (re)found myself
;;; and others are famous for various reasons.
;;;
;;; Some patterns assume a game size of roughly 40*40.

(define glider '((10 10) (11 10) (12 10) (12 9) (11 8)))

(define blinker '((10 10) (11 10) (12 10)))

(define gospers-glider-gun '((3 6) (4 6) (3 7) (4 7) (13 6) (13 7) (13 8) (14 5)
                             (14 9) (15 4) (15 10) (16 4) (16 10) (17 7) (18 5)
                             (18 9) (19 6) (19 7) (19 8) (20 7) (23 4) (23 5)
                             (23 6) (24 4) (24 5) (24 6) (25 3) (25 7) (27 2)
                             (27 3) (27 7) (27 8) (37 4) (37 5) (38 4) (38 5)))
