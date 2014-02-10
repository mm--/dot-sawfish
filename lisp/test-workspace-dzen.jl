;; focus-in-hook
;; focus-out-hook

(define workspace-logging t)

(define %wlog-proc nil)

(setq %wlog-proc (make-process))
(start-process %wlog-proc "/home/jm3/.sawfish/scripts/workspace-switch.sh")

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

(define (workspace-switch-log)
  "When we switch workspaces, make a note"
  (when workspace-logging
    (when (process-running-p %wlog-proc)
      (write %wlog-proc (concat "REPLACE:_DESKNUM:" (prin1-to-string current-workspace) "\n")))))

(add-hook 'enter-workspace-hook workspace-switch-log)
