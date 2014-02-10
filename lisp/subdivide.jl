;; subdivide.jl 0.1 -- Functions to resize a window in half.
;;
;; Copyright (C) 2013 Joshua Moller-Mara <j.moller-mara@berkeley.ed>
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the same terms as sawfish (the window manager), or at your
;; choice, under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This package defines various functions for splitting a window in half.
;; 
;; Here's an example configuration.
;; 
;; (bind-keys global-keymap
;; 	   "W-C-S-Left" 'half-window-left
;; 	   "W-C-S-Right" 'half-window-right
;; 	   "W-C-S-Up" 'half-window-up
;; 	   "W-C-S-Down" 'half-window-down)


;; Todo: Make fraction configurable
;; Make a function that takes a list of directions?

(define (half-window w direction)
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
    (if (memq direction '(left right))
	(rplaca dim (- half-wx dx))
      (rplacd dim (- half-wy dy)))
    (cond ((eq direction 'right) (rplaca pos (+ x half-wx)))
	  ((eq direction 'down) (rplacd pos (+ y half-wy))))
    (move-resize-window-to w (car pos) (cdr pos)
			   (car dim) (cdr dim))
    (warp-cursor-to-window w)))

(define (half-window-left w)
  "Resizes window to half it's width and moves it left."
  (half-window w 'left))

(define (half-window-right w)
  "Resizes window to half it's width and moves it right."
  (half-window w 'right))

(define (half-window-up w)
  "Resizes window to half it's height and moves it up."
  (half-window w 'up))

(define (half-window-down w)
  "Resizes window to half it's height and moves it down."
  (half-window w 'down))

(define-command 'half-window-left half-window-left #:spec "%W")
(define-command 'half-window-right half-window-right #:spec "%W")
(define-command 'half-window-up half-window-up #:spec "%W")
(define-command 'half-window-down half-window-down #:spec "%W")

(provide 'subdivide)
