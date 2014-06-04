(define workspace-logging t)

(define (workspace-switch-log)
  "When we switch workspaces, make a note"
  (when workspace-logging
    (system (concat "~/.sawfish/scripts/workspace-switch.sh " (prin1-to-string current-workspace) " &"))))

(add-hook 'enter-workspace-hook workspace-switch-log)
