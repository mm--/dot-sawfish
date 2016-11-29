(define (record-window win)
  "Attempt to record the window using recordmydesktop, and save it in ~/screenshots"
  (let* ((id (format nil "0x%x" (window-id win)))
	 (winname (translate-string (concat (window-name win)) alphanumeric-table))
	 (screenfilename (concat "~/screenshots/" (current-time-string (current-time) "%F_%H%M%S") ".ogv")))
    ;; Removed using "screenfilename" for now. It doesn't work sometimes with asterisks
    (system (concat "~/.sawfish/scripts/record-window.sh " id " " screenfilename " &"))))


(bind-keys global-keymap
	   "W-S-s" '(record-window (input-focus)))
