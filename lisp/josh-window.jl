;; Bring a window from wherever it is to here.
;; Create functions to hide windows.
;; Create functions to create windows if they don't exist
;; Rename this when I think of a better name

;; Get window and copy it to this workspace
(define (josh-copy-window-here window)
  "Take a window, copy it to this workspace and move it here."
  (copy-to-next-workspace window 0 nil) ;0 spaces right
  (move-window-to-current-viewport window))

(define (josh-show window #!optional sticky nofocus)
  "Copy it to this workspace, raise and focus"
  (when (window-id window)
    (josh-copy-window-here window)
    (show-window window)
    (raise-window window)
    (when sticky (make-window-sticky window))
    (unless nofocus (set-input-focus window))))

(setq josh-hide-list ())
(defvar josh-junk-workspace 12)

(define (josh-copy-window-to-junk window)
  "Copy to junk workspace."
  (copy-window-to-workspace window (car (window-workspaces window)) josh-junk-workspace))


(define (josh-delete-assoc-eq key val list)
  "Delete all association lists that have a KEY of VAL"
  (delete-if (lambda (x) (eq (cdr (assoc key x)) val)) list))

(define (josh-hide window)
  "Copy it to the junk workspace so we don't close it. Then remove it from this workspace."
  (let ((had-focus (equal window (input-focus))))
    (setq josh-hide-list (cons `((window . ,window)
				 (sticky . ,(window-sticky-p window)))
			       (josh-delete-assoc-eq 'window window josh-hide-list)))
    (make-window-unsticky window)
    (josh-copy-window-to-junk window)
    (delete-window-instance window)
    (if had-focus
	(set-input-focus (query-pointer-window)))))

(define-command 'josh-hide josh-hide #:spec "%w")

(define (josh-show-hide window #!optional sticky nofocus)
  "Show it if it's not focused. Hide it if it is."
  (if (and  (or (window-sticky-p window) (window-in-workspace-p window current-workspace))
	    (not (window-outside-viewport-p window))
	    (not (window-obscured window)))
      (josh-hide window)
    (josh-show window sticky nofocus)))

(define (josh-unhide)
  "Unhide a recently hidden window"
  (let* ((winassoc (car josh-hide-list))
	 (win (cdr (assoc 'window winassoc)))
	 (sticky (cdr (assoc 'sticky winassoc))))
    (setq josh-hide-list (cdr josh-hide-list))
    (josh-show win sticky)))

(define (josh-unhide-menu)
  "Generate a menu of recently hidden windows."
  (mapcar (lambda (x) (let ((win (cdr (assoc 'window x))))
			(cons (window-name win) (list (list 'josh-show-hide win)))))
	  (delete-if-not (lambda (x) (window-id (cdr (assoc 'window x)))) josh-hide-list)))

(define (josh-show-or-exec regex prog #!key match-class)
  "Like jump-or-exec, but copy the window here instead of us moving there."
  (let ((wind (if match-class
		    (get-window-by-class regex #:regex t)
		  (get-window-by-name regex #:regex t))))
      (cond ((windowp wind) (josh-show-hide wind))
	    ;; Exec "prog"
	    ((stringp prog) (system (concat prog "&")))
	    ((functionp prog) (funcall prog))
	    ((commandp prog) (call-command prog))
	    (t (user-eval prog)))))

(define (josh-show-hide-video #!optional sticky)
  (let ((w (or (get-window-by-class "mplayer2" #:regex t)
	       (get-window-by-class "mpv" #:regex t))))
    (when w
      (josh-show-hide w sticky)
      (set-input-focus (query-pointer-window))))) ;Don't automatically focus

(bind-keys global-keymap
	   ;; "W-n" '(josh-show-or-exec "NCMPC" "xterm -title NCMPC ncmpc" t)
	   "W-y" '(josh-show-hide-video)
	   "W-C-y" '(josh-show-hide-video t))

(bind-keys global-keymap
	   "W-h" 'josh-hide
	   "W-C-h" '(josh-unhide)
	   "W-H"  '(popup-menu (josh-unhide-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bind letters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar josh-window-alist nil)

(define (josh-menu-gen action bindings)
  "Generate a menu doing ACTION for each member of BINDINGS."
  (mapcar
   (lambda (x) (let* ((apair (assoc x josh-window-alist))
		      (awin (cdr (assoc 'window (cdr apair))))
		      (winsticky (cdr (assoc 'sticky (cdr apair)))))
		 (cons (concat "_" x
			       (when (and awin (window-id awin)) (concat " -> " (window-name awin))))
		       (cond ((eq action 'bind) (list (list 'josh-bind-window x (input-focus))))
			     ((eq action 'show) (list (list 'josh-show awin winsticky)))
			     ((eq action 'show-hide) (list (list 'josh-show-hide awin winsticky)))))))
   (if (eq action 'bind) bindings
     (filter (lambda (x) (let* ((apair (assoc x josh-window-alist))
				(awin (cdr (assoc 'window (cdr apair)))))
			   (and awin (window-id awin))))
	     bindings))))

;; (defmacro add-to-list (listname newbeginning)
;;     "Add NEWBEGINNING to the beginning of LISTNAME"
;;     (list 'setq listname (list 'append (list 'list newbeginning) listname)))

(defmacro add-to-list (listname newbeginning)
  "Add NEWBEGINNING to the beginning of LISTNAME"
  `(setq ,listname (append (list ,newbeginning) ,listname)))

(define (josh-bind-window binding window)
  "Bind WINDOW to BINDING in josh-window-alist"
  ;; (display-message (concat "Binding " binding " to " (window-name window)))
  (add-to-list josh-window-alist (cons binding `((window . ,window)
						 (sticky . ,(window-sticky-p window))))))

(bind-keys global-keymap
	   "W-C-2" '(display-message "Hello"))

(defvar josh-bindings (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

(define (josh-show-hide-number num #!optional nofocus)
    "Show or hide the window bound to number NUM, respecting its sticky flag."
    (let* ((apair (assoc num josh-window-alist))
	   (awin (cdr (assoc 'window (cdr apair))))
	   (winsticky (cdr (assoc 'sticky (cdr apair)))))
      (josh-show-hide awin winsticky nofocus)))

(define (josh-bind-numbers)
  "Bind josh-show and josh-bind-window to numbers."
  (mapc (lambda (i) (bind-keys global-keymap
			       (concat "W-" i) `(josh-show-hide-number ,i)
			       (concat "W-C-" i) `(josh-show-hide-number ,i t)))
	(list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))

(bind-keys global-keymap
	   "W-C-q" '(popup-menu (josh-menu-gen 'bind josh-bindings (input-focus)))
	   "W-q" '(popup-menu (josh-menu-gen 'show-hide josh-bindings)))

(josh-bind-numbers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go to window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar josh-go-to-window-alist nil)
(defvar josh-go-to-previous-cell (cons nil nil))

(define (josh-go-to-menu-gen action bindings)
  "Generate a menu doing ACTION for each member of BINDINGS."
  (mapcar
   (lambda (x) (let* ((apair (assoc x josh-go-to-window-alist))
		      (aworkspace (cadr apair))
		      (awin (caddr apair)))
		 (cons (concat "_" x
			       (when (and awin (window-id awin)) (concat " -> " (window-name awin))))
		       (cond ((eq action 'bind) (list (list 'josh-go-to-bind x current-workspace (input-focus))))
			     ((eq action 'goto) (list (list 'josh-go-to aworkspace awin)))))))
   (if (eq action 'bind) bindings
     (filter (lambda (x) (let* ((awin (caddr (assoc x josh-go-to-window-alist))))
			   (and awin (window-id awin))))
	     bindings))))

(define (josh-go-to-bind binding workspace window)
  "Bind WORKSPACE and WINDOW to BINDING in josh-go-to-window-alist"
  ;; (display-message (concat "Binding " binding " to " (window-name window)))
  (add-to-list josh-go-to-window-alist (list binding workspace window)))

(define (josh-go-to workspace window)
  "Go to WORKSPACE and WINDOW"
  (setq josh-go-to-previous-cell (cons current-workspace (input-focus)))
  (select-workspace workspace)
  (display-window-without-focusing window)
  (unless (equal (query-pointer-window) window)
    (let* ((frame-dim (window-frame-dimensions window))
	   (pos (window-position window))
	   (x (car pos))
	   (y (cdr pos))
	   (halfwidth (round (/ (car frame-dim) 2)))
	   (halfheight (round (/ (cdr frame-dim) 2))))
      ;; (display-message (concat (prin1-to-string halfwidth) " " (prin1-to-string halfheight)))
      (warp-cursor (+ x halfwidth) (+ y halfheight))))
  (input-focus window))

(define (josh-go-to-previous)
  "Go to the previous window before last goto command."
  (let* ((theworkspace (car josh-go-to-previous-cell))
	 (thewindow (cdr josh-go-to-previous-cell)))
    (when (and theworkspace thewindow)
      (josh-go-to theworkspace thewindow))))

(bind-keys global-keymap
	   "W-C-g" '(popup-menu (josh-go-to-menu-gen 'bind josh-bindings (input-focus)))
	   "W-g" '(popup-menu (josh-go-to-menu-gen 'goto josh-bindings))
	   "W-S-g" '(josh-go-to-previous))

;;;;;;;;;;
;; Make it easier to go to the previously focused window
;; (Useful since one of my wireless keyboards doesn't have a mouse)

;; Holds the workspace and the window
(defvar josh-current-cell (cons nil nil))
(defvar josh-previous-cell (cons nil nil))

(define (push-previous-window w)
  "When focusing on a window, remember the last window."
  (let* ((newcell (cons current-workspace (input-focus))))
    (unless (equal newcell josh-current-cell)
      (setq josh-previous-cell josh-current-cell)
      (setq josh-current-cell newcell))))

(add-hook 'focus-in-hook push-previous-window)

(define (josh-go-to-previous-2)
  "Go to the previous window."
  (let* ((theworkspace (car josh-previous-cell))
	 (thewindow (cdr josh-previous-cell)))
    (when (and theworkspace thewindow)
      (josh-go-to theworkspace thewindow))))

(bind-keys global-keymap
	   "W-C-p" '(josh-go-to-previous-2))

;; (remove-hook 'focus-in-hook push-previous-window)
