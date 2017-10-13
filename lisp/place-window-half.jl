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

( add-window-matcher '( ( WM_CLASS . "^Chromium/chromium$" ) )
		     '( ( place-mode . full-or-half-viewport ) ))

( add-window-matcher '( ( WM_CLASS . "^Emacs/emacs$" ) )
		     '( ( place-mode . half-viewport ) ))

( add-window-matcher '( ( WM_CLASS . "Firefox" ) )
		     '( ( place-mode . full-or-half-viewport ) ))
