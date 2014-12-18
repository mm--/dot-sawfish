(require 'sawfish.wm.prg.compton)

(define (toggle-opacity-viewport)
  "Toggle all transparencies of windows. Either set at 100 or nothing."
  (let ((wins (viewport-windows-filtered)))
	(if (delete-if-not identity
			   (mapcar (lambda (x) (not (window-get x 'opacity))) wins))
	    (mapc (lambda (x) (opacity-increment x 100)) wins)
	  (progn (mapc (lambda (x) (window-put x 'opacity ())
			 (window-opacity x)) wins)
		 (sync-server)))))

(define (remove-transparency w)
  (window-put (input-focus) 'opacity ())
  (window-opacity w))

(define (opacity-increment window inc)
  "Increment opacity of WINDOW by INC"
  (define (get-opacity w)
    (* (/ 4294967294.00 100) w))
  (let* ((opacity (or (window-get window 'opacity) 100))
	 (newopacity (min (max (+ opacity inc) 0) 100)))
    (window-put window 'opacity newopacity)
    (set-x-property (window-frame-id window) '_NET_WM_WINDOW_OPACITY (make-vector 1 (get-opacity newopacity)) 'CARDINAL 32)
    (sync-server)))

(provide 'opacity-tools)
