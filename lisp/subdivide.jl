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

(require 'sawfish.wm.util.workarea)
(require 'sawfish.wm.util.rects)

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

(define (opposite-dir direction)
  (cond ((eq direction 'left) 'right)
	((eq direction 'right) 'left)
	((eq direction 'up) 'down)
	((eq direction 'down) 'up)))

(define (subd rect direction #!optional amount pos)
  "Calculates a new rectangle by dividing the rectange either up,
down, left, or right. Amount refers to what fraction we should
take of its width/height, for example to half it we'd use 2. Pos
is used with either right or down, and it refers to where in the
subdivision we'd like to place the rectangle. This is useful for
tiling."
  (let* ((amount (or amount 2))
	 (pos (or pos 1))
	 (width (- (rect-right rect) (rect-left rect)))
	 (height(- (rect-bottom rect) (rect-top rect)))
	 (x (rect-left rect))
	 (y (rect-top rect))
	 (scale-wx (round (/ width amount)))
	 (scale-wy (round (/ height amount)))
	 (weight (nth 4 rect))
	 (opposite (opposite-dir direction)))
    (if (memq direction '(left right))
	(setq width scale-wx)
      (setq height scale-wy))
    (cond ((eq direction 'right) (setq x (+ x (* pos scale-wx))))
	  ((eq direction 'down) (setq y (+ y (* pos scale-wy)))))
    (list x y
	  (+ x width) (+ y height)
	  (if (memq opposite weight)
	      weight
	    (cons opposite weight)))))

(define (master-wind w)
  "A simple tiling scheme with the master window on the left and all non-masters on the right, tiled."
  (true-move w (subd (calculate-workarea) 'left))
  (let* ((rightrect (subd (calculate-workarea) 'right))
	 (others (viewport-windows-except w))
	 (length (apply + (mapcar (lambda (x) 1) others))) ;I have no idea how to get the list length. :( I'm so sorry
	 (pos 0))
    (mapc (lambda (x) (true-move x (subd rightrect 'down length pos))
	    (setq pos (+ 1 pos)))
	  others)))

(define (true-move w rect)
  "I was having problems with move-resize-window-to on account of
not taking into account frame dimensions. This helps with that"
	(let* ((frame-dim (window-frame-dimensions w))
	       (dim (window-dimensions w))
	       (dx (- (car frame-dim) (car dim)))
	       (dy (- (cdr frame-dim) (cdr dim)))
	       (width (- (rect-right rect) (rect-left rect) dx))
	       (height(- (rect-bottom rect) (rect-top rect) dy)))
	  (move-resize-window-to w (rect-left rect) (rect-top rect)
				 width height)))

(define (tree-place x of alt)
  (define (altdir leftorright)
    (if alt (if (eq leftorright 'left) 'left
	      'right)
      (if (eq leftorright 'left) 'up
	      'down)))
  (let ((half (ceiling (/ of 2))))
    (cond ((= of 1) nil)
	  (t (if (<= x half) (cons (altdir 'left) (tree-place x half (not alt)))
	       (cons (altdir 'right) (tree-place (- x half) (- of half) (not alt))))))))

(define (breakme seq rect)
  (if seq
      (breakme (cdr seq) (subd rect (car seq)))
    rect))

(define (viewport-windows-except w)
  (delete-if (lambda (x) (eq w x)) (viewport-windows-filtered)))

(define (viewport-windows-filtered)
  (delete-if (lambda (x) (or (window-ignored-p x)
			     (window-avoided-p x))) (viewport-windows)))

(define (shrink-rect rect amount)
  "Shrink the rectangle by some amount. If there's no amount,
just return the rectangle."
  (if amount
      (let ((weight (nth 4 rect))
	    (half (round (/ amount 2))))
	(list (+ (rect-left rect) (if (memq 'left weight) half amount))
	      (+ (rect-top rect) (if (memq 'up weight) half amount))
	      (- (rect-right rect) (if (memq 'right weight) half amount))
	      (- (rect-bottom rect) (if (memq 'down weight) half amount))
	      weight))
    rect))

(define (binary-tile-master w #!optional pad)
  "A binary tiling scheme, with selected master window on the
left. Optional padding."
  (let ((others (viewport-windows-except w)))
    (if (not others)
	(true-move w (shrink-rect (calculate-workarea) pad))
      (true-move w (shrink-rect (subd (calculate-workarea) 'left) pad))
      (binary-tile (viewport-windows-except w)
		       (subd (calculate-workarea) 'right) pad))))

(define (binary-tile windows rect #!optional pad vertical)
  "A binary tiling scheme."
  (let* ((length (apply + (mapcar (lambda (x) 1) windows))) ;I have no idea how to get the list length. :( I'm so sorry
	 (pos 0))
    (mapcar (lambda (x) (true-move x (shrink-rect (breakme (tree-place (setq pos (+ 1 pos)) length vertical) rect) pad)))
	    windows)))

(provide 'subdivide)
