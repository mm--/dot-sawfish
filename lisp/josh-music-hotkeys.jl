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

(defmacro musicctl (name mplayerdo pianodo mpcdo mpvcommand)
  `(define (,(intern (concat name "-mplayer-or-mpd")))
     (let ((win (or (get-window-by-class "mplayer2")
		    (get-window-by-class "mpv"))))
      (if win
	  (synthesize-event ,mplayerdo win)
	(if (not (eq (system ,(concat "~/.sawfish/scripts/pianobar-ctl.sh " pianodo " " mpvcommand)) 0))
	    (system (concat mpdpass ,(concat "mpc " mpcdo " &"))))))))

(defmacro volumectl (name mplayerdo mpvcommand amount)
  `(define (,(intern (concat name "-mpv")))
     (let ((win (or (get-window-by-class "mplayer2")
		    (get-window-by-class "mpv"))))
      (if win
	  (synthesize-event ,mplayerdo win)
	(if (not (eq (system ,(concat "~/.sawfish/scripts/mpv-volume-ctl.sh " mpvcommand)) 0))
	    (system (concat "pactl set-sink-volume 0 " ,amount " &")))))))

(musicctl "play" "SPACE" "p" "toggle" "cycle pause")
(musicctl "next" "Return" "n" "next" "playlist_next 1")
(musicctl "prev" "<" "q" "prev" "playlist_prev 1")
(define-command 'play-mplayer-or-mpd play-mplayer-or-mpd)
(define-command 'next-mplayer-or-mpd next-mplayer-or-mpd)
(define-command 'prev-mplayer-or-mpd prev-mplayer-or-mpd)

(musicctl "stop" "q" "q" "stop" "quit")
(define-command 'stop-mplayer-or-mpd stop-mplayer-or-mpd)

(musicctl "ff" "Right" "i" "seek +00:00:10 " "seek +5")
(musicctl "rw" "Left" "i" "seek -00:00:10 " "seek -5")
(define-command 'mplayer-or-mpd-ff ff-mplayer-or-mpd)
(define-command 'mplayer-or-mpd-rw rw-mplayer-or-mpd)

(musicctl "speed-up" "]" "i" "stats" "add speed 0.10")
(musicctl "slow-down" "[" "i" "stats" "add speed -0.10")
(musicctl "speed-reset" "BackSpace" "i" "stats" "set speed 1.00")
(define-command 'mplayer-speed-up speed-up-mplayer-or-mpd)
(define-command 'mplayer-slow-down slow-down-mplayer-or-mpd)
(define-command 'mplayer-normal-speed speed-reset-mplayer-or-mpd)

(volumectl "volume-up" "0" "add volume 2" "+2%")
(volumectl "volume-down" "9" "add volume -2" "-2%")

;; Volume keybindings
(bind-keys global-keymap
	   "W-]" '(volume-up-mpv)
	   "W-[" '(volume-down-mpv)
	   "W-{" 'mplayer-or-mpd-rw
	   "W-}" 'mplayer-or-mpd-ff
	   "W-C-]" 'mplayer-speed-up
	   "W-C-[" 'mplayer-slow-down
	   "W-C-BackSpace" 'mplayer-normal-speed
	   "XF86AudioRaiseVolume" '(system "pactl set-sink-volume 0 +2% &")
	   "XF86AudioLowerVolume" '(system "pactl set-sink-volume 0 -2% &")
	   "XF86AudioMute" '(system "pactl set-sink-mute 0 toggle &")
	   "XF86AudioPlay" 'play-mplayer-or-mpd
	   "S-XF86AudioPlay" 'stop-mplayer-or-mpd
	   "XF86AudioPause" 'stop-mplayer-or-mpd
	   "XF86AudioPrev" 'prev-mplayer-or-mpd
	   "XF86AudioNext" 'next-mplayer-or-mpd)
