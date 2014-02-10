(define (dictator-paste)
  "Send Ctrl-V to Dictator and press Space to start it."
  (let ((win (get-window-by-name "Dictator.*" #:regex t)))
    (when win
      (synthesize-event "C-v" win)
      (set-input-focus win))))
(define-command 'dictator-paste dictator-paste)

;; (synthesize-event "a" (get-window-by-name "Dictator.*" #:regex t))
;; (synthesize-event "space" (get-window-by-name "Event Tester" #:regex t))

(require 'rep.io.timers)
(require 'sawfish.wm.commands.shrink-yank)

;; Maybe only make it shrink if it's in the way?
;; Add a hook to the display of windows?
;; Save initial size and when it closes, resize it?
(define (dictator-popup-and-shrink)
  (let ((curwin (input-focus)))
    ( system "~/.sawfish/scripts/dictator-clip.sh &" )
    (make-timer (lambda () (shrink-window-right curwin)) 1)))
(define-command 'dictator-popup-and-shrink dictator-popup-and-shrink)

(define read-keymap (make-keymap))
( bind-keys read-keymap
	    "d" '( system "~/.sawfish/scripts/dictator-clip.sh &" )
	    "D" 'dictator-paste
	    "p" 'dictator-popup-and-shrink
	    "y" '( system "~/.sawfish/scripts/youtube-mplayer.sh &" )
	    "Y" '( system "~/.sawfish/scripts/youtube-mplayer-med.sh &" )
	    )

(bind-keys global-keymap
	   "W-k" read-keymap)

(provide 'dictator-scratch)
