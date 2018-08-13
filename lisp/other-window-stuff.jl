(define (monitor-window-title win)
  "Monitor the title of a window, alerting if it changed. Handy for monitoring long-running processes in the background."
  (system (format nil "~/.sawfish/scripts/notify-window-title-changed.sh 0x%x &" (window-id win))))

(define (kill-private-browsing)
  "Kill private browsing windows. Handy since cookies don't get
erased in Firefox until all private windows are closed."
  (mapc delete-window
	(filter (lambda (x) (string-match "Private Browsing" (window-name x)))
		(managed-windows))))

(define other-stuff-menu
  `(("Kill _private browsing" (kill-private-browsing))
    ("_Monitor title" (monitor-window-title (input-focus)))))

(bind-keys global-keymap
	   "W-o" '(popup-menu other-stuff-menu))
