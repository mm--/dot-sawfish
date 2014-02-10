;; dmg-gpsy.jl 0.2 -- mode to quickly grow, pack, shrink or yank a window
;;
;; Copyright (C) 2009 Daniel M. German <dmg@uvic.ca>
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the same terms as sawfish (the window manager), or at your
;; choice, under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; By default 'dmg-gpsy-mode is bound to M-C-g. 
;; 
;; Once in gpsy mode you are in a state mode: by default pack, but you
;; can press 'g', 'p', 's', or 'y' to change to the grow-, pack-,
;; shrink- or yank- modes.
;; 
;; At any time you can press Up, Left, Right, Down, to grow-, pack-,
;; shrink- or yank- the current window (according to the current state).
;;
;; You can always press ESC, or Ctrl-g to abort.
;;
;; Known Bugs:
;;    shrink and yank functions are now working.
;; 
;;	2010-01-01  dmg  <dmg@mn.cs.uvic.ca>
;;
;;	* Added support for ctrl-p, ctrl-b, ctrl-n, ctrl-f in addition to arrows


	
(require 'sawfish.wm.keymaps)

(require 'sawfish.wm.commands.grow-pack)
(require 'sawfish.wm.commands.shrink-yank)

(defun dmg-gpsy-read-event ()
  (throw 'dmg-gpsy-read (event-name (current-event)))
  )

(defun dmg-gpsy-mode (w)
  "Let user pick the last window used from a class with incremental search and return that window."
  (interactive "%W")
  (display-message "GPSY ... grow mode (press g, p, s, or y to change), use arrows to affect window")
  (when (grab-keyboard)
    (unwind-protect
        (let* ((override-keymap '(keymap))
               (input "")
               (key "")
               (mode "p") ; default mode is grow
               )
          (add-hook 'unbound-key-hook dmg-gpsy-read-event)
          (catch 'exit-gpsy-mode
            (while t
              (setq key
                    (catch 'dmg-gpsy-read
                      (recursive-edit)))
              (cond 
               ((or (equal key "C-g")
                    (equal key "A-g")
		    (equal key "RET")
                    (equal key "ESC"))
                (throw 'exit-gpsy-mode nil))
               ((equal key "g")
                (display-message "grow mode...")
                (setq mode key)
                )
               ((equal key "p")
                (display-message "pack mode...")
                (setq mode key)
                )
               ((equal key "s")
                (display-message "shink mode...")
                (setq mode key)
                )
               ((equal key "y")
                (display-message "yank mode...")
                (setq mode key)
                )
               ((or (equal key "Up") (equal key "C-p"))
                (cond
                 ((equal mode "g")
                  (grow-window-up w)
                  (display-message "Growing up")
                  )
                 ((equal mode "p")
                  (display-message "Packing up")
                  (pack-window-up w)
                  )
                 ((equal mode "y")
                  (display-message "Yanking up")
                  (yank-window-up w)
                  )
                 ((equal mode "s")
                  (display-message "Shrink up")
                  (shrink-window-up w)
                  )
                 ) ; cond
                ) ; cond option
               ((or (equal key "Down") (equal key "C-n"))
                (cond
                 ((equal mode "g")
                  (display-message "Growing down")
                  (grow-window-down w)
                  )
                 ((equal mode "p")
                  (display-message "Packing down")
                  (pack-window-down  w)
                  )
                 ((equal mode "y")
                  (display-message "Yanking down")
                  (yank-window-down w)
                  )
                 ((equal mode "s")
                  (display-message "Shrinking down")
                  (shrink-window-down w)
                  )
                 ) ; cond
                ) ;  cond option
               ((or (equal key "Left") (equal key "C-b"))
                (cond
                 ((equal mode "g")
                  (display-message "Growing left")
                  (grow-window-left w)
                  )
                 ((equal mode "p")
                  (display-message "Packing left")
                  (pack-window-left w)
                  )
                 ((equal mode "y")
                  (display-message "Yanking left")
                  (yank-window-left w)
                  )
                 ((equal mode "s")
                  (display-message "Shriking left")
                  (shrink-window-left w)
                  )
                 ) ; cond
                ); cond option
               ((or (equal key "Right") (equal key "C-f"))
                (cond
                 ((equal mode "g")
                  (display-message "Growing right")
                  (grow-window-right w)
                  )
                 ((equal mode "p")
                  (display-message "Packing right")
                  (pack-window-right w)
                  )
                 ((equal mode "y")
                  (display-message "Yanking right")
                  (yank-window-right w)
                  )
                 ((equal mode "s")
                  (display-message "Shriking right")
                  (shrink-window-right w)
                  )
                 ) ; cond
                ) ; cond option
               ) ; cond
              ) ; while
            ); catch
          ) ; let
      (remove-hook 'unbound-key-hook dmg-gpsy-read-event)
      (display-message nil)
      (ungrab-keyboard)))
  ) ; defun



(bind-keys global-keymap "M-C-g" 'dmg-gpsy-mode)

(bind-keys global-keymap "W-g" 'dmg-gpsy-mode)

(provide 'dmg-gpsy)
