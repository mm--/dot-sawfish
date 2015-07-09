(defvar josh-window-clipboard nil)
(define (josh-copy-window window)
  (setq josh-window-clipboard window)
  (display-message (concat "Copied: " (window-name window)))
  (make-timer (lambda () (display-message nil)) 1))

(define (josh-paste-window)
  (josh-copy-window-here josh-window-clipboard))

(bind-keys global-keymap
	   "W-C-c" '(josh-copy-window (input-focus))
	   "W-C-v" '(josh-paste-window))
