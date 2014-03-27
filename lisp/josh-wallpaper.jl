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
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/iceland.png" 3)
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/YzbQWzH.png" 4)
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/BonsaiRock.png" 5)
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/k0DyX6Y.png" 6)
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/MIpllFc.png" 7)
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/ZDCCvIQ.png" 8)
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/G5ushKr.png" 9)
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/ibFcFuc.png" 10)
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/oVwpXLD.png" 11)
(josh-set-wallpaper-workspace "/home/jm3/.fvwm/wallpaper/UFeXPN5.png" 12)


(define (josh-wallpaper-change)
  (let ((wallpaper (cdr (assoc current-workspace josh-wallpaper))))
    (when wallpaper
      (setq josh-current-wallpaper wallpaper)
      (system (concat "fvwm-root -r " wallpaper " &")))))

(add-hook 'enter-workspace-hook josh-wallpaper-change)

(define (josh-lockscreen)
  "Lock the screen"
  (if josh-current-wallpaper
      (system (concat "i3lock -d -i " josh-current-wallpaper " &" ))
    (system  "i3lock -d -c 101010 &")))

(define-command 'josh-lockscreen josh-lockscreen)

(define (josh-fingerprint-lockscreen)
  "Lock the screen, fingerprint deactivate"
  (system (concat "~/.sawfish/scripts/fingerprint-lock.sh " josh-current-wallpaper " &" )))

(define-command 'josh-fingerprint-lockscreen josh-fingerprint-lockscreen)

(bind-keys global-keymap
	   "W-l" 'josh-lockscreen
	   "W-M-S-l" 'josh-fingerprint-lockscreen)
