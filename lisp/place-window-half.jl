(define (josh-other-windows-intersecting-rect win rect)
  (let* ((other-rects (rectangles-from-windows (viewport-windows-except win) (lambda (x) x))))
    (filter windowp
	    (mapcar (lambda (wr)
		      (car (cddddr (rectangle-intersection wr rect))))
		    other-rects))))

(define (place-window-half win)
  (let* ((left-rect (shrink-rect (subd (calculate-workarea) 'left) 20))
	 (right-rect (shrink-rect (subd (calculate-workarea) 'right) 20)))
    (cond ((not (josh-other-windows-intersecting-rect win left-rect))
	   (true-move win left-rect))
	  ((not (josh-other-windows-intersecting-rect win right-rect))
	   (true-move win right-rect))
	  (t ((placement-mode 'top-left) win)))))

(define-placement-mode 'half-viewport place-window-half #:for-normal t)

(define (place-window-full-or-half win)
  (let* ((full-rect (shrink-rect (calculate-workarea) 20)))
    (if (josh-other-windows-intersecting-rect win full-rect)
	((placement-mode 'half-viewport) win)
      (true-move win full-rect))))

(define-placement-mode 'full-or-half-viewport place-window-full-or-half #:for-normal t)

(require 'sawfish.wm.frames)
(define (josh-set-emacs-win win binding style)
  (set-frame-style win style)
  (true-move win (shrink-rect (subd (calculate-workarea) 'left) 20))
  (josh-bind-window binding win))

(setq josh-emacs-bind-styles
      '(("3" . candido)
	("2" . absolute-e)
	("4" . Elberg-tabbed)
	("5" . microGUI)))

(define (place-emacs-half win)
  "Place emacs in the half-viewport position. Also make sure we
set the frame correctly if some window-key bindings don't exist."
  (let* ((unbound-emacs (car (filter (lambda (binding)
				       (let ((thewin (cdr (assoc 'window (assoc binding josh-window-alist)))))
					 (not (when thewin (window-mapped-p thewin)))))
				   (mapcar car josh-emacs-bind-styles))))
       (unbound-style (cdr (assoc unbound-emacs josh-emacs-bind-styles))))
  (if unbound-emacs
      (josh-set-emacs-win win unbound-emacs unbound-style)
    (place-window-half win))))

(define-placement-mode 'emacs-half-viewport place-emacs-half #:for-normal t)

( add-window-matcher '( ( WM_CLASS . "^Chromium/chromium$" ) )
		     '( ( place-mode . full-or-half-viewport ) ))

( add-window-matcher '( ( WM_CLASS . "^Emacs/emacs$" ) )
		     '( ( place-mode . emacs-half-viewport ) ))

( add-window-matcher '( ( WM_CLASS . "Firefox" )
			( WM_WINDOW_ROLE . "browser") )
		     '( ( place-mode . full-or-half-viewport ) ))
