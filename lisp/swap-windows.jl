;; Make a function for swapping two windows
;; Ideally, swap the focused window with the next focused window?

(defvar josh-swap-windows-start-win nil
  "Window we're swapping")

(define (josh-sw-focus-swap focusedwin)
  (when josh-swap-windows-start-win
    (let* ((rects (rectangles-from-windows (list josh-swap-windows-start-win focusedwin) identity))
	   (start-rect (car rects))
	   (end-rect (cadr rects)))
      (true-move (nth 4 start-rect) end-rect)
      (true-move (nth 4 end-rect) start-rect)
      (setq josh-swap-windows-start-win nil))))

;; (remove-hook 'focus-in-hook josh-sw-focus-swap)
(add-hook 'focus-in-hook josh-sw-focus-swap)

(define (josh-swap-window-interactively w)
  (let ((other-windows (viewport-windows-except (input-focus))))
    (setq josh-swap-windows-start-win w)
    (when (= (length other-windows) 1)
      (josh-sw-focus-swap (car other-windows)))))

(define-command 'josh-swap-window-interactively
  josh-swap-window-interactively #:spec "%W")
