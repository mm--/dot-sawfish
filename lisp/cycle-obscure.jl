;; Cycle only windows that are overlapping

(require 'sawfish.wm.commands.x-cycle)
(require 'sawfish.wm.util.rects)

(define (get-intersecting-windows win)
  (let* ((win-rect (car (rectangles-from-windows (list win))))
	 (other-rects (rectangles-from-windows (viewport-windows-except win) (lambda (x) x)))
	 (intersecting-windows (filter windowp
				       (mapcar (lambda (wr)
						 (car (cddddr (rectangle-intersection wr win-rect))))
					       other-rects))))
    (cons win intersecting-windows)))

(get-intersecting-windows (input-focus))

(define-cycle-command-pair
  'cycle-obscure 'cycle-obscure-backwards
  (lambda (w)
    (get-intersecting-windows w))
  #:spec "%W")

(bind-keys global-keymap
	   "W-TAB" 'cycle-obscure
	   "W-C-TAB" 'cycle-windows)
