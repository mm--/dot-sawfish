;; Cycle only windows that are overlapping

(require 'sawfish.wm.commands.x-cycle)
(require 'sawfish.wm.util.rects)

(define (get-intersecting-windows win)
  (let* ((win-rect (car (rectangles-from-windows (list win))))
	 (other-rects (rectangles-from-windows (viewport-windows-except win) (lambda (x) x))))
    (filter windowp
		 (mapcar (lambda (wr)
			   (car (cddddr (rectangle-intersection wr win-rect))))
			 other-rects))))

(define-cycle-command-pair
  'cycle-obscure 'cycle-obscure-backwards
  (lambda (w)
    (cons w (get-intersecting-windows w)))
  #:spec "%W")

;; This doesn't exactly work because once it gets to a group of
;; intersecting windows it'll get "stuck" in them.
(define-cycle-command-pair
  'cycle-obscure-dwim 'cycle-obscure-dwim-backwards
  (lambda (w)
    (let ((intersecting (get-intersecting-windows w)))
      (if intersecting
	  (cons w intersecting)
	(filter-windows window-in-cycle-p))))
  #:spec "%W")

(bind-keys global-keymap
	   "W-TAB" 'cycle-obscure-dwim
	   "W-C-TAB" 'cycle-windows)
