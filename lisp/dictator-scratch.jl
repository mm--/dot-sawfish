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
  (system (concat "~/.sawfish/scripts/espeak.sh " (number->string wpm) " &")))

(define (copy-and-espeak)
  (synthesize-event "C-c" (input-focus))
  (espeak 450)
  (synthesize-event "Button1-Click1" (input-focus)))

(define espeak-menu
  '(("_1 - 100 WPM" (espeak 100))
    ("_2 - 200 WPM" (espeak 200))
    ("_3 - 250 WPM" (espeak 250))
    ("_4 - 300 WPM" (espeak 300))
    ("_5 - 350 WPM" (espeak 350))
    ("_6 - 400 WPM" (espeak 400))
    ("_7 - 500 WPM" (espeak 500))
    ("_8 - 600 WPM" (espeak 600))
    ("_9 - 700 WPM" (espeak 700))
    ("Fast Spea_k"  (espeak 390))
    ("_Goose espeak" ( system "~/.sawfish/scripts/goose-espeak.sh &" ))
    ()
    ("_Volume" (popup-menu volume-menu))
    ()
    ("j_etzt" ( system "~/.sawfish/scripts/jetzt-clip.sh &" ))
    ("_Dictator" ( system "~/.sawfish/scripts/dictator-clip.sh &" ))
    ("Dictator _Url Goose" ( system "~/.sawfish/scripts/goose-dictator.sh &" ))
    ("Dictator popup" dictator-popup-and-shrink)
    ("_Youtube player" ( system "xterm -e ~/.sawfish/scripts/youtube-mplayer.sh &" ))
    ("Youtube _medium" ( system "xterm -e ~/.sawfish/scripts/youtube-mplayer.sh -m &" ))
    ("Youtube Dow_nload" ( system "xterm -e ~/.sawfish/scripts/youtube-mplayer.sh -d &" ))
    ("Youtube Download _first" ( system "xterm -e ~/.sawfish/scripts/youtube-mplayer.sh -d -f &" ))
    ("Youtu_be download medium" ( system "xterm -e ~/.sawfish/scripts/youtube-mplayer.sh -m -d &" ))
    ("Youtube _Just Download" ( system "xterm -e ~/.sawfish/scripts/youtube-mplayer.sh -j &" ))
    ()
    ("Dragon clipboard _image" (system "xterm -e ~/.sawfish/scripts/dragon-url.sh &"))
    ()
    ("_Pass" ( system "xterm -e ~/.sawfish/scripts/pass.sh &" ))
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
	   "W-k" '(popup-menu espeak-menu)
	   "W-S-k" '(copy-and-espeak))

(provide 'dictator-scratch)
