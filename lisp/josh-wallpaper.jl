(defmacro add-to-list (listname newbeginning)
  "Add NEWBEGINNING to the beginning of LISTNAME"
  `(setq ,listname (append (list ,newbeginning) ,listname)))

(defvar josh-wallpaper ())
(defvar josh-current-wallpaper ())
(define (josh-set-wallpaper-workspace wallpaper workspace)
  "Set the wallpaper for a workspace"
  (add-to-list josh-wallpaper (cons workspace wallpaper)))

(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/DzVsyZE.png" 0)
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/isle-of-sky.png" 1)
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/2048.png" 2)
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/fG00q2M.png" 3)
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/YzbQWzH.png" 4)
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/fjaðrárgljúfur.png" 5)
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/k0DyX6Y.png" 6)
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/StGeorgeWharf-MarkSpokeos.png" 7)
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/ZDCCvIQ.png" 8)
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/G5ushKr.png" 9)
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/ibFcFuc.png" 10)
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/PlMvMqN.png" 11)
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/UFeXPN5.png" 12)

(defvar josh-default-wallpaper "/home/jm3/.fvwm/wallpaper/ice-cube-berg-wallpaper.png")

(define (josh-wallpaper-change)
  (let ((wallpaper (or (cdr (assoc current-workspace josh-wallpaper))
		       josh-default-wallpaper)))
    (when wallpaper
      (setq josh-current-wallpaper wallpaper)
      (system (concat "feh --bg-fill " wallpaper " &")))))

(add-hook 'enter-workspace-hook josh-wallpaper-change)
(add-hook 'after-initialization-hook josh-wallpaper-change)

(define (josh-lockscreen)
  "Lock the screen"
  (if josh-current-wallpaper
      (system (concat "~/.sawfish/scripts/lockscreen-run.sh i3lock -d -i " josh-current-wallpaper " &" ))
    (system  "~/.sawfish/scripts/lockscreen-run.sh i3lock -d -c 101010 &")))

(define-command 'josh-lockscreen josh-lockscreen)

(define (josh-fingerprint-lockscreen)
  "Lock the screen, fingerprint deactivate"
  (system (concat "xterm -e ~/.sawfish/scripts/lockscreen-run.sh ~/.sawfish/scripts/fingerprint-lock.sh " josh-current-wallpaper " &" )))

(define-command 'josh-fingerprint-lockscreen josh-fingerprint-lockscreen)

(bind-keys global-keymap
	   "W-l" 'josh-lockscreen
	   "W-M-S-l" 'josh-fingerprint-lockscreen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspace letters to numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar josh-workspace-map
  '(("a" . 13)
    ("b" . 14)
    ("c" . 15)
    ("d" . 16)
    ("e" . 17)
    ("f" . 18)
    ("g" . 19)
    ("h" . 20)
    ("i" . 21)
    ("j" . 22)
    ("k" . 23)
    ("l" . 24)
    ("m" . 25)
    ("n" . 26)
    ("o" . 27)
    ("p" . 28)
    ("q" . 29)
    ("r" . 30)
    ("s" . 31)
    ("t" . 32)
    ("u" . 33)
    ("v" . 34)
    ("w" . 35)
    ("x" . 36)
    ("y" . 37)
    ("z" . 38)
    ("0" . 0)
    ("1" . 1)
    ("2" . 2)
    ("3" . 3)
    ("4" . 4)
    ("5" . 5)
    ("6" . 6)
    ("7" . 7)
    ("8" . 8)
    ("9" . 9)))

(define (josh-workspace-menu-gen)
  "Generate a menu doing ACTION for each member of BINDINGS."
  (mapcar
   (lambda (x) (let* ((letter (car x))
		      (desknum (cdr x))
		      (numwindows (length (workspace-windows desknum))))
		 (cons (concat "_" letter (if (> numwindows 0)
					      (format nil " - %d" numwindows)
					    ""))
		       (list (list 'select-workspace desknum)))))
   josh-workspace-map))

(define (josh-workspace-num letter)
  "Get a workspace number from a letter"
  (cdr (assoc letter josh-workspace-map)))

(bind-keys global-keymap
	   "W-d"  '(popup-menu (josh-workspace-menu-gen)))

(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/hov5xGE.png" (josh-workspace-num "p"))
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/giantscauseway-wallpaper.png" (josh-workspace-num "q"))
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/ice-cube-berg-wallpaper.png" (josh-workspace-num "a"))
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/oeqiwuN-wallpaper.png" (josh-workspace-num "b"))
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/LEfcStC-wallpaper.png" (josh-workspace-num "c"))
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/KaPoFDj-wallpaper.png" (josh-workspace-num "e"))
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/16669224462_7e0cfb502f_h-wallpaper.png" (josh-workspace-num "f"))
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/sDv4w4L.png" (josh-workspace-num "u"))
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/macintosh-plus-wallpaper.png" (josh-workspace-num "m"))
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/lain1-wallpaper.png" (josh-workspace-num "l"))
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/neurons-wallpaper.png" (josh-workspace-num "n"))
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/1442647589767-wallpaper.png" (josh-workspace-num "r"))
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/tekkon_kinkreet_by_imperial-boy-wallpaper.png" (josh-workspace-num "t"))
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/1446657777432-wallpaper.png" (josh-workspace-num "h"))
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/x4eMDNY-wallpaper.png" (josh-workspace-num "a"))
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/bjZGHN2-wallpaper.png" (josh-workspace-num "g"))
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/bryce_canyon_under_snow_by_bendianna-d9iq1ce-wallpaper.png" (josh-workspace-num "s"))
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/K90hfvO-wallpaper.png" (josh-workspace-num "w"))
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/FoQZls3-wallpaper.png" (josh-workspace-num "z"))
