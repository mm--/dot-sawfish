(define (screenshot-lock)
  (system "~/.sawfish/scripts/screenshot-lock.sh &"))

(define-command 'screenshot-lock screenshot-lock)

(bind-keys global-keymap
	   "W-C-l" 'screenshot-lock)

 ;; (current-time-string (current-time) "%F %R:%S ") (window-name win) ".png")
;; (concat (current-time-string (current-time) "%F_%R:%S.png") (window-name win) ".png")

(require 'rep.io.timers)

;; Used to replace characters that make it hard to save a filename
(setq alphanumeric-table "_____________________________________________-._0123456789:__=_?@ABCDEFGHIJKLMNOPQRSTUVWXYZ______abcdefghijklmnopqrstuvwxyz___~_________________________________________________________________________________________________________________________________")

(define (screenshot-window win)
  "Take a screenshot of the window given, and save it in ~/screenshots"
  (let* ((id (format nil "0x%x" (window-id win)))
	 (winname (translate-string (concat (window-name win)) alphanumeric-table))
	 (screenfilename (concat "~/screenshots/" (current-time-string (current-time) "%F_%R:%S") "-" winname ".png")))
    (system (concat "import -window " id " -quality 100 " screenfilename))
    (display-message screenfilename)
    (make-timer (lambda () (display-message nil)) 1)
    ))

(define-command 'screenshot-window screenshot-window #:spec "%W")

(bind-keys global-keymap
	   "W-C-s" 'screenshot-window)

;; (format nil "0x%x" (window-id (input-focus)))

;; (list-x-properties (input-focus))
;; (display-message nil)

(provide 'test-screenshot)
