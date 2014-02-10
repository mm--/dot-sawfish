;; focus-in-hook
;; focus-out-hook

(define window-logging nil)

(define %wlog-proc nil)

(setq %wlog-proc (make-process))
(start-process %wlog-proc "/home/jm3/.sawfish/scripts/logger.sh")

;; (let
;;     ((process (make-process)))
;;   (start-process process "ls" "-s"))

;; (setq output (make-string-output-stream))
;; (setq test-proc (make-process output))
;; (start-process test-proc "/home/jm3/.sawfish/scripts/logger.sh")

;; (get-output-stream-string output)
;; (setq output2 (make-string-input-stream
;; 		    (get-output-stream-string output)))
;; (read output2)
;; (read (process-output-stream test-proc))

;; (kill-process %wlog-proc)
;; (accept-process-output)
;; (process-output-stream test-proc)

;; (output-stream-p (process-output-stream test-proc))

;; (processp %wlog-proc)
;; (process-prog %wlog-proc)
;; (process-running-p %wlog-proc)
;; (active-processes)

;; (input-stream-p %wlog-proc)

;; (process-output-stream %wlog-proc)

(define (window-logger title)
  "Takes a title of the new window.
Does something with it. Like logs it."
  (when window-logging
    (when (process-running-p %wlog-proc)
      (write %wlog-proc (concat title "\n")))))

(define (window-logger-focus w)
  "What to do when focused."
  (window-logger (window-name w)))

(define (window-logger-unfocus w)
  "What to do when unfocused."
  (window-logger "UNFOCUS"))

(define (window-logger-changed-property w prop state)
  "What to do when state changed."
  (if (and (memq prop '(WM_NAME _NET_WM_NAME))
	   (eq w (input-focus)))
      (window-logger (window-name w))))

(add-hook 'property-notify-hook window-logger-changed-property)
(remove-hook 'property-notify-hook window-logger-changed-property)

;; (setq property-notify-hook nil)
(setq window-logging t)
(add-hook 'focus-in-hook window-logger-focus)
(add-hook 'focus-out-hook window-logger-unfocus)
(remove-hook 'focus-out-hook window-logger-unfocus)
(remove-hook 'focus-in-hook window-logger-focus)

(window-name (select-window))

(window-logger (window-name (select-window)))


(define (lockscreen)
  "Lock the screen"
  (window-logger "")
  ( system "i3lock -d -c 008b8b &" ))
(define-command 'lockscreen lockscreen)
