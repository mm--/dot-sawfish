(define (move-window-slightly w direction)
  "Move a window slightly by DIRECTION."
  (let ((pos (window-position w))
	(dx (cond ((eq direction 'left) -10)
		  ((eq direction 'right) 10)
		  (t 0)))
	(dy (cond ((eq direction 'up) -10)
		  ((eq direction 'down) 10)
		  (t 0)))
	(pointerw (query-pointer-window))
	(mx (car (query-pointer)))
	(my (cdr (query-pointer))))
    (when (equal pointerw w)
      (warp-cursor (+ dx mx) (+ dy my)))
    (move-window-to w (+ (car pos) dx) (+ (cdr pos) dy))))

(define (move-window-slightly-left w)
  (move-window-slightly w 'left))

(define (move-window-slightly-right w)
  (move-window-slightly w 'right))

(define (move-window-slightly-up w)
  (move-window-slightly w 'up))

(define (move-window-slightly-down w)
  (move-window-slightly w 'down))

(define-command 'move-window-slightly-left move-window-slightly-left #:spec "%W")
(define-command 'move-window-slightly-right move-window-slightly-right #:spec "%W")
(define-command 'move-window-slightly-up move-window-slightly-up #:spec "%W")
(define-command 'move-window-slightly-down move-window-slightly-down #:spec "%W")

(bind-keys global-keymap
	   "W-S-Left" 'move-window-slightly-left
	   "W-S-Right" 'move-window-slightly-right
	   "W-S-Up" 'move-window-slightly-up
	   "W-S-Down" 'move-window-slightly-down)
