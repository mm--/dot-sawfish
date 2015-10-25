;; Maximize functions with padding
(require 'sawfish.wm.state.maximize)

(define (padded-maximize function)
  "Make a padded version of a maximize function"
  (let ((w (input-focus)))
    (function w)
    (when (window-maximized-p w)
      (true-move w (shrink-rect (window-rect w) 20)))))

(define (maximize-padded-toggle w)
  (maximize-window-toggle w)
  (when (window-maximized-p w)
    (true-move w (shrink-rect (window-rect w) 10))))

(define (window-rect w)
  "Get rectangle that represents window"
  (let* ((frame-dim (window-frame-dimensions w))
	 (pos (window-position w))
	 (left (car pos))
	 (top (cdr pos))
	 (right (+ left (car frame-dim)))
	 (bottom (+ top (cdr frame-dim))))
    (list left top right bottom ())))

( bind-keys max-keymap
	    "W-d" '(padded-maximize maximize-discard)
	    "W-m" '(padded-maximize maximize-window-toggle)
	    "W-v" '(padded-maximize maximize-fill-window-vertically-toggle)
	    "W-h" '(padded-maximize maximize-fill-window-horizontally-toggle)
	    "W-f" '(padded-maximize maximize-fill-window-toggle)
	    "W-u" 'maximize-window-fullscreen-toggle)

(bind-keys global-keymap
	   "W-C-Button1-Click" '(padded-maximize maximize-fill-window-toggle))
