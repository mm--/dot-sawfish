(define (move-viewport-and-window w direction)
  "Move a window to another viewport determined by DIRECTION.
Change the current viewport as well."
  (let ((dx (cond ((eq direction 'left) -1)
		  ((eq direction 'right) 1)
		  (t 0)))
	(dy (cond ((eq direction 'up) -1)
		  ((eq direction 'down) 1)
		  (t 0))))
    (move-window-viewport w dx dy)
    (move-viewport dx dy)))

(define (move-viewport-and-window-left w)
  (move-viewport-and-window w 'left))

(define (move-viewport-and-window-right w)
  (move-viewport-and-window w 'right))

(define (move-viewport-and-window-up w)
  (move-viewport-and-window w 'up))

(define (move-viewport-and-window-down w)
  (move-viewport-and-window w 'down))

(define-command 'move-viewport-and-window-left move-viewport-and-window-left #:spec "%W")
(define-command 'move-viewport-and-window-right move-viewport-and-window-right #:spec "%W")
(define-command 'move-viewport-and-window-up move-viewport-and-window-up #:spec "%W")
(define-command 'move-viewport-and-window-down move-viewport-and-window-down #:spec "%W")

(bind-keys global-keymap
	   "W-S-M-Left" 'move-viewport-and-window-left
	   "W-S-M-Right" 'move-viewport-and-window-right
	   "W-S-M-Up" 'move-viewport-and-window-up
	   "W-S-M-Down" 'move-viewport-and-window-down)
