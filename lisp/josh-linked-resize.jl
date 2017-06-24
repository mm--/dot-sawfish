;; Linked resizing of windows
;; Resizing one window adjusts neighboring ones
;; Josh Moller-Mara 2017

(defvar linked-resizing-is-active nil
  "Are we also moving other edges while resizing?")

(defvar linked-resize-other-starting-rects nil
  "Other windows and their rectangles while resizing")

(defvar linked-resize-win-starting-rect nil
  "Resize window and its rectangles before resizing")

(defvar josh-lr-final-resize-windows nil
  "Set this if we only resize other windows at the end of
  interactive resizing. This is useful if many resizes are drawn slowly.")

(defvar josh-lr-padding-distance 80
  "Edges within this distance are linked resized.")

(define (josh-lr-before-resize resizewin)
  (setq linked-resize-other-starting-rects
	(rectangles-from-windows (viewport-windows-except resizewin) identity))
  (setq linked-resize-win-starting-rect (car (rectangles-from-windows (list resizewin) identity))))

(define (josh-lr-after-resize resizewin)
  (when josh-lr-final-resize-windows
    (mapc (lambda (rect)
	    (true-move (nth 4 rect) rect))
	  josh-lr-final-resize-windows))
  (setq linked-resizing-is-active nil
	josh-lr-final-resize-windows nil))

(define (josh-within x y range)
  (<= (abs (- x y)) range))

;; linked-resize-win-starting-rect
;; linked-resize-other-starting-rects

;; (remove-hook 'before-resize-hook josh-lr-before-resize)
(add-hook 'before-resize-hook josh-lr-before-resize)
;; (remove-hook 'after-resize-hook josh-lr-after-resize)
(add-hook 'after-resize-hook josh-lr-after-resize)

(define (josh-linked-resize-rects-adjust oldvert newvert oldhoriz newhoriz)
  (mapc (lambda (rect)
	  (true-move (nth 4 rect) rect))
	(filter identity
		(mapcar (lambda (rect)
			  (let ((right (rect-right rect))
				(left (rect-left rect))
				(top (rect-top rect))
				(bottom (rect-bottom rect))
				(changed nil))
			    (and oldhoriz
				 (or (if (josh-within right oldhoriz josh-lr-padding-distance)
					 (setq right (+ right (- newhoriz oldhoriz))))
				     (if (josh-within left oldhoriz josh-lr-padding-distance)
					 (setq left (+ left (- newhoriz oldhoriz)))))
				 (setq changed t))
			    (and oldvert
				 (or (if (josh-within top oldvert josh-lr-padding-distance)
					 (setq top (+ top (- newvert oldvert))))
				     (if (josh-within bottom oldvert josh-lr-padding-distance)
					 (setq bottom (+ bottom (- newvert oldvert)))))
				 (setq changed t))
			    (when changed
			      (list left top right bottom (nth 4 rect)))))
			linked-resize-other-starting-rects))))

;; This version only adjusts windows at the end
;; (define (josh-linked-resize-rects-adjust oldvert newvert oldhoriz newhoriz)
;;   (setq josh-lr-final-resize-windows
;; 	(filter identity
;; 		(mapcar (lambda (rect)
;; 			  (let ((right (rect-right rect))
;; 				(left (rect-left rect))
;; 				(top (rect-top rect))
;; 				(bottom (rect-bottom rect))
;; 				(changed nil))
;; 			    (and oldhoriz
;; 				 (or (if (josh-within right oldhoriz josh-lr-padding-distance)
;; 					 (setq right (+ right (- newhoriz oldhoriz))))
;; 				     (if (josh-within left oldhoriz josh-lr-padding-distance)
;; 					 (setq left (+ left (- newhoriz oldhoriz)))))
;; 				 (setq changed t))
;; 			    (and oldvert
;; 				 (or (if (josh-within top oldvert josh-lr-padding-distance)
;; 					 (setq top (+ top (- newvert oldvert))))
;; 				     (if (josh-within bottom oldvert josh-lr-padding-distance)
;; 					 (setq bottom (+ bottom (- newvert oldvert)))))
;; 				 (setq changed t))
;; 			    (when changed
;; 				(list left top right bottom (nth 4 rect)))))
;; 			linked-resize-other-starting-rects))))

(define (josh-lr-move-other-edges resizewin)
  (when linked-resizing-is-active
    (let (oldvert
	  newvert
	  oldhoriz
	  newhoriz)
      (cond
       ((memq 'bottom move-resize-moving-edges)
	(setq oldvert (rect-bottom linked-resize-win-starting-rect)
	      newvert (+ (rect-bottom linked-resize-win-starting-rect)
			 (- move-resize-height move-resize-old-height))))
       ((memq 'top move-resize-moving-edges)
	(setq oldvert (rect-top linked-resize-win-starting-rect)
	      newvert move-resize-y)))
      (cond
       ((memq 'right move-resize-moving-edges)
	(setq oldhoriz (rect-right linked-resize-win-starting-rect)
	      newhoriz (+ (rect-right linked-resize-win-starting-rect)
			  (- move-resize-width move-resize-old-width))))
       ((memq 'left move-resize-moving-edges)
	(setq oldhoriz (rect-left linked-resize-win-starting-rect)
	      newhoriz move-resize-x)))
      ;; (display-message (format nil "%s %s %s %s" oldvert newvert oldhoriz newhoriz))
      (josh-linked-resize-rects-adjust oldvert newvert oldhoriz newhoriz))))

;; (setq while-resizing-hook nil)
(add-hook 'while-resizing-hook josh-lr-move-other-edges)

;; (setq linked-resizing-is-active t)

(define (linked-resize-window-interactively w)
  (setq linked-resizing-is-active t)
  (resize-window-interactively w))

(require 'sawfish.wm.commands.move-resize)

(define-command 'linked-resize-window-interactively
  linked-resize-window-interactively #:spec "%W")
  
;; (define (josh-resize-other-edges resizewin)
;;   (if josh-resize-other-edges
;;       (display-message (format nil "Resize edges: %s\n%s - %s"
;; 			       move-resize-moving-edges
;; 			       move-resize-x
;; 			       (rect-left linked-resize-win-starting-rect)))))

;; (setq while-resizing-hook nil)
;; (add-hook 'while-resizing-hook josh-resize-other-edges)
