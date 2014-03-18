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

(define (espeak wpm)
  (system (concat "pgrep espeak && pkill espeak || xclip -o | espeak -a 200 -v english-us -s " (number->string wpm) " &")))

(define espeak-menu
  '(("_1 - 100 WPM" (espeak 100))
    ("_2 - 200 WPM" (espeak 200))
    ("_3 - 250 WPM" (espeak 250))
    ("_4 - 300 WPM" (espeak 300))
    ("_5 - 350 WPM" (espeak 350))
    ("Fast Spea_k"  (espeak 390))
    ("Pa_use" (system "pkill -sigstop espeak"))
    ("Cont_inue" (system "pkill -sigcont espeak"))
    ()
    ("_Dictator" dictator-paste)
    ("Dictator _popup" dictator-popup-and-shrink)
    ("_youtube player" ( system "~/.sawfish/scripts/youtube-mplayer.sh &" ))
    ("_Youtube medium" ( system "~/.sawfish/scripts/youtube-mplayer-med.sh &" ))
    ))

(define read-keymap (make-keymap))
;; ( bind-keys read-keymap
;; 	    "d" '( system "~/.sawfish/scripts/dictator-clip.sh &" )
;; 	    "D" 'dictator-paste
;; 	    "p" 'dictator-popup-and-shrink
;; 	    "y" '( system "~/.sawfish/scripts/youtube-mplayer.sh &" )
;; 	    "Y" '( system "~/.sawfish/scripts/youtube-mplayer-med.sh &" )
;; 	    )
;; (bind-keys global-keymap
;; 	   "W-k" read-keymap)

(bind-keys global-keymap
	   "W-k" '(popup-menu espeak-menu))

(provide 'dictator-scratch)
