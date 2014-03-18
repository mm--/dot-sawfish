;; josh-expand.jl 0.1 -- Functions to shrink and expand windows.
;;
;; Copyright (C) 2013 Joshua Moller-Mara <j.moller-mara@berkeley.ed>
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the same terms as sawfish (the window manager), or at your
;; choice, under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This package mainly allows you to shrink windows, giving a nice
;; effect when windows are manually tiled together.
;; 
;; Here's an example configuration:
;; (bind-keys global-keymap
;; 	   "W-C-Button4-Click" '(expand-window (input-focus) 5)
;; 	   "W-C-Button5-Click" '(expand-window (input-focus) -5)
;; 	   "W-S-C-Button4-Click" '(expand-all 5)
;; 	   "W-S-C-Button5-Click" '(expand-all -5))



;; Todo: Make fraction configurable
;; Make a function that takes a list of directions?

(define (expand-window w amount)
  "Resizes window to half in the direction specified"
  (let* ((frame-dim (window-frame-dimensions w))
	 (dim (window-dimensions w))
	 (pos (window-position w))
	 (dx (- (car frame-dim) (car dim)))
	 (dy (- (cdr frame-dim) (cdr dim)))
	 (x (car pos))
	 (y (cdr pos))
	 (half-wx (round (/ (car frame-dim) 2)))
	 (half-wy (round (/ (cdr frame-dim) 2))))
    (rplaca dim (- (+ (car frame-dim) (* 2 amount)) dx))
    (rplacd dim (- (+ (cdr frame-dim) (* 2 amount)) dy))
    (rplaca pos (- x amount))
    (rplacd pos (- y amount))
    (move-resize-window-to w (car pos) (cdr pos)
			   (car dim) (cdr dim))))

;; (expand-window (select-window) -1)

(define (expand-all amount)
  (mapc (lambda (win) (expand-window win amount))
	(filter (lambda (x) (not (window-outside-viewport-p x))) (workspace-windows))))


(provide 'josh-expand)
