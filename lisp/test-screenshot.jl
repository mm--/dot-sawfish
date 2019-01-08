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

(define (screenshot-desktop)
  "Take a screenshot of the whole viewport, and save it in ~/screenshots"
  (let* ((screenfilename (concat "~/screenshots/" (current-time-string (current-time) "%F_%R:%S") "-desktop.png")))
    (system (concat "import -window root -quality 100 " screenfilename))
    (display-message screenfilename)
    (make-timer (lambda () (display-message nil)) 1)
    ))

(define (screenshot-scrot)
  "Take a screenshot using scrot, and save it in ~/screenshots"
  (let* ((screenfilename (concat "~/screenshots/" (current-time-string (current-time) "%F_%R:%S") "-scrot.png")))
    (display-message (concat "Take screenshot for " screenfilename))
    (system (concat "/usr/bin/scrot -s " screenfilename " &"))
    (make-timer (lambda () (display-message nil)) 1)))

(define-command 'screenshot-window screenshot-window #:spec "%W")

(define (screenshot-clipboard)
  (system "~/.sawfish/scripts/screenshot-clipboard.sh &"))

(define screenshot-menu
  '(("_w - Screenshot window" (screenshot-window (input-focus)))
    ("_s - Screenshot scrot" (screenshot-scrot))
    ("_c - Screenshot clipboard" (screenshot-clipboard))
    ("_d - Screenshot desktop" (screenshot-desktop))
    ("_o - Screenshot OCR to clipboard" (system "~/.sawfish/scripts/screenshot-ocr-clipboard.sh &"))))

(define screenshot-keymap (make-keymap))
(bind-keys screenshot-keymap
	   "w" 'screenshot-window
	   "r" 'screenshot-scrot
	   "c" '(screenshot-clipboard)
	   "d" '(screenshot-desktop)
	   "W-s" 'screenshot-window
	   "o" '(system "~/.sawfish/scripts/screenshot-ocr-clipboard.sh &"))

(bind-keys global-keymap
	   "W-C-s" '(popup-menu screenshot-menu)
	   ;; Conflicts with record-window
	   ;; "w-S-s" '(screenshot-scrot)
	   "W-s" screenshot-keymap)

;; (format nil "0x%x" (window-id (input-focus)))

;; (list-x-properties (input-focus))
;; (display-message nil)

(provide 'test-screenshot)
