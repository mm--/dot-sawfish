;; This file is to control various music players.
;; Controls:
;; - video mplayer
;; - MPD
;; - Pianobar (through ~/.config/pianobar/ctl)
;; echo -n 'p' > ~/.config/pianobar/ctl

;; Get the password
;; The file is of the form "MPD_HOST=PASS@localhost " with no newline
(setq mpdpassfile "~/.sawfish/mpdpassword")
(setq mpdpass
      (if (file-exists-p mpdpassfile)
	  (let* ((file (open-file mpdpassfile 'read))
		 (pass (read-line file)))
	    (close-file file)
	    pass)
	nil))

(defmacro musicctl (name mplayerdo pianodo mpcdo)
  `(define (,(intern (concat name "-mplayer-or-mpd")))
    (let ((win (get-window-by-class "MPlayer")))
      (if win
	  (synthesize-event ,mplayerdo win)
	(if (not (eq (system ,(concat "~/.sawfish/scripts/pianobar-ctl.sh " pianodo)) 0))
	    (system (concat mpdpass ,(concat "mpc " mpcdo " &"))))))))

(musicctl "play" "SPACE" "p" "toggle")
(musicctl "next" ">" "n" "next")
(musicctl "prev" "<" "q" "prev")
(define-command 'play-mplayer-or-mpd play-mplayer-or-mpd)
(define-command 'next-mplayer-or-mpd next-mplayer-or-mpd)
(define-command 'prev-mplayer-or-mpd prev-mplayer-or-mpd)


(define (mplayer-or-mpd-ff)
  "If there's an MPlayer, fast forward 10 seconds
  Otherwise, fast forward the MPD song."
  (let ((win (get-window-by-class "MPlayer")))
    (if win
	(synthesize-event "Right" win)
      (system (concat mpdpass "mpc seek +00:00:10 &")))))
(define-command 'mplayer-or-mpd-ff mplayer-or-mpd-ff)

(define (mplayer-or-mpd-rw)
  "If there's an MPlayer, rewind 10 seconds
  Otherwise, rewind the MPD song."
  (let ((win (get-window-by-class "MPlayer")))
    (if win
	(synthesize-event "Left" win)
      (system (concat mpdpass "mpc seek -00:00:10 &")))))
(define-command 'mplayer-or-mpd-rw mplayer-or-mpd-rw)

(define (mplayer-speed-up)
  "Speed up mplayer"
  (let ((win (get-window-by-class "MPlayer")))
    (if win
	(synthesize-event "]" win))))
(define-command 'mplayer-speed-up mplayer-speed-up)

(define (mplayer-slow-down)
  "Slow down mplayer"
  (let ((win (get-window-by-class "MPlayer")))
    (if win
	(synthesize-event "[" win))))
(define-command 'mplayer-slow-down mplayer-slow-down)

(define (mplayer-normal-speed)
  "Normal speed mplayer"
  (let ((win (get-window-by-class "MPlayer")))
    (if win
	(synthesize-event "BackSpace" win))))
(define-command 'mplayer-normal-speed mplayer-normal-speed)

;; Volume keybindings
(bind-keys global-keymap
	   "W-]" '(system "amixer set Master 2%+ &")
	   "W-[" '(system "amixer set Master 2%- &")
	   "W-{" 'mplayer-or-mpd-rw
	   "W-}" 'mplayer-or-mpd-ff
	   "W-C-]" 'mplayer-speed-up
	   "W-C-[" 'mplayer-slow-down
	   "W-C-BackSpace" 'mplayer-normal-speed
	   "XF86AudioRaiseVolume" '(system "amixer set Master 2%+ &")
	   "XF86AudioLowerVolume" '(system "amixer set Master 2%- &")
	   "XF86AudioPlay" 'play-mplayer-or-mpd
	   "XF86AudioPrev" 'prev-mplayer-or-mpd
	   "XF86AudioNext" 'next-mplayer-or-mpd)
