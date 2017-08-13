;; Make a function for swapping two windows
;; Ideally, swap the focused window with the next focused window?

(defvar josh-swap-windows-start-win nil
  "Window we're swapping")

(define (josh-sw-focus-swap focusedwin)
  (when josh-swap-windows-start-win
    (let* ((rects (rectangles-from-windows (list josh-swap-windows-start-win focusedwin) identity))
	   (start-rect (car rects))
	   (end-rect (cadr rects)))
      (josh-swap-stack josh-swap-windows-start-win focusedwin)
      (setq josh-swap-windows-start-win nil)
      (true-move (nth 4 start-rect) end-rect)
      (true-move (nth 4 end-rect) start-rect))))

(define (josh-swap-stack win1 win2)
  (restack-windows (mapcar (lambda (y)
      				 (cond
      				  ((equal y win1) win2)
      				  ((equal y win2) win1)
      				  (t y)))
      			     (let ((vwf (viewport-windows-filtered)))
      			       (filter (lambda (x) (member x vwf)) (mapped-stacking-order))))))

;; (remove-hook 'focus-in-hook josh-sw-focus-swap)
;; (setq focus-in-hook (cdr focus-in-hook))
;; focus-in-hook
(add-hook 'focus-in-hook josh-sw-focus-swap)

(define (josh-swap-window-interactively w)
  (let ((other-windows (viewport-windows-except (input-focus))))
    (setq josh-swap-windows-start-win w)
    (when (= (length other-windows) 1)
      (josh-sw-focus-swap (car other-windows)))))

(define-command 'josh-swap-window-interactively
  josh-swap-window-interactively #:spec "%W")
