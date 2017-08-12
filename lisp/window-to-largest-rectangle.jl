;; Largest rectangle?

;; Oh, edges are different from rectangles
(require 'sawfish.wm.util.edges)

(define (josh-move-window-largest-rect w)
  (let* ((head (window-head-any-viewport w))
	 (viewport (window-viewport w))
	 (avoid-by-default t)
	 (avoided (avoided-windows w))
	 (edges (get-visible-window-edges
		 #:with-ignored-windows t
		 #:windows avoided
		 #:include-heads (list head)
		 #:viewport viewport))
	 (max-rect (largest-rectangle-from-edges edges
						 #:avoided avoided
						 #:head (window-head-any-viewport w))))
    (true-move w (shrink-rect max-rect 20))))

(define-command 'josh-move-window-largest-rect
  josh-move-window-largest-rect #:spec "%W")

(bind-keys window-keymap
	   "W-C-Button2-click" 'josh-move-window-largest-rect)
