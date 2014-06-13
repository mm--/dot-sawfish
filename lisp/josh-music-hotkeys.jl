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

(defmacro musicctl (name mplayerdo pianodo mpcdo mplayercommand)
  `(define (,(intern (concat name "-mplayer-or-mpd")))
    (let ((win (get-window-by-class "MPlayer")))
      (if win
	  (synthesize-event ,mplayerdo win)
	(if (not (eq (system ,(concat "~/.sawfish/scripts/pianobar-ctl.sh " pianodo " " mplayercommand)) 0))
	    (system (concat mpdpass ,(concat "mpc " mpcdo " &"))))))))

(musicctl "play" "SPACE" "p" "toggle" "pause")
(musicctl "next" "Return" "n" "next" "pt_step 1 1")
(musicctl "prev" "<" "q" "prev" "pt_step -1 1")
(define-command 'play-mplayer-or-mpd play-mplayer-or-mpd)
(define-command 'next-mplayer-or-mpd next-mplayer-or-mpd)
(define-command 'prev-mplayer-or-mpd prev-mplayer-or-mpd)

(musicctl "stop" "q" "q" "stop" "quit")
(define-command 'stop-mplayer-or-mpd stop-mplayer-or-mpd)

(musicctl "ff" "Right" "i" "seek +00:00:10 " "seek +5")
(musicctl "rw" "Left" "i" "seek -00:00:10 " "seek -5")
(define-command 'mplayer-or-mpd-ff ff-mplayer-or-mpd)
(define-command 'mplayer-or-mpd-rw rw-mplayer-or-mpd)

(musicctl "speed-up" "]" "i" "stats" "speed_incr 0.10")
(musicctl "slow-down" "[" "i" "stats" "speed_incr -0.10")
(musicctl "speed-reset" "BackSpace" "i" "stats" "speed_set 1.00")
(define-command 'mplayer-speed-up speed-up-mplayer-or-mpd)
(define-command 'mplayer-slow-down slow-down-mplayer-or-mpd)
(define-command 'mplayer-normal-speed speed-reset-mplayer-or-mpd)

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
	   "S-XF86AudioPlay" 'stop-mplayer-or-mpd
	   "XF86AudioPrev" 'prev-mplayer-or-mpd
	   "XF86AudioNext" 'next-mplayer-or-mpd)


