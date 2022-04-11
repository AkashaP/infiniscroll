(defvar scroll-pre-jump-back-hook (list))
(defvar scroll-post-jump-back-hook (list))

(defcustom infiniscroll-commands
  '(handle-select-window handle-switch-frame
    scroll-up scroll-down
    scroll-up-command scroll-down-command
    scroll-bar-toolkit-scroll mwheel-scroll
    scroll-other-window scroll-other-window-down
    scroll-bar-scroll-up scroll-bar-scroll-down scroll-bar-drag)
  "Commands handled by infiniscroll mode.
infiniscroll mode will try to restore the original position of
`point' after executing a sequence of any of these commands."
  :type 'hook)

(defvar-local infiniscroll-restore-ctype cursor-type)

(defun infiniscroll-post-command-logic () 
  ;; Make cursor visible or invisible
  (let* ((bfs (window-parameter (selected-window) 'infiniscroll-buffer))
         (pt (cl-loop for m in bfs
                   if (eq (marker-buffer m) (current-buffer))
                   do (cl-return m))))
    (if (markerp pt)
        (if (not (pos-visible-in-window-p pt (selected-window))) 
            (setf cursor-type nil)
          (setf cursor-type infiniscroll-restore-ctype)))))

(defun infiniscroll-pre-command-logic ()
  (let* ((bfs (window-parameter (selected-window) 'infiniscroll-buffer))
         (pt (cl-loop for m in bfs
                   if (eq (marker-buffer m) (current-buffer))
                   do (cl-return m))))
    (if (markerp pt)
        (if (eq this-command 'self-insert-command) 
            (progn
              (setf cursor-type infiniscroll-restore-ctype)
              (if (pos-visible-in-window-p pt (selected-window))
                  (setf (marker-position pt) (point))
                
                ;; Restore the position of the scroll point 
                (map nil #'funcall scroll-pre-jump-back-hook)
                (setf (point) pt)
                (map nil #'funcall scroll-post-jump-back-hook)
                ;; we sneakily forget old buffers here and stuff after jumping back. Could be done whenever.
                (set-window-parameter (selected-window) 'infiniscroll-buffer
                                      (cl-loop for m in bfs
                                            if (buffer-live-p (marker-buffer m))
                                            collect m 
                                            else do (setf (marker-buffer m) nil)))))

          ;; Make cursor visible or invisible
          (unless (member this-command infiniscroll-commands)
            (setf (marker-position pt) (point)))
          (if (not (pos-visible-in-window-p pt (selected-window))) 
              (setf cursor-type nil)
            (setf cursor-type infiniscroll-restore-ctype
                  (point) pt))
          ;; (message "%s movetime" this-command)
          )
      (let ((marker (point-marker)))
        (set-marker-insertion-type marker t)
        (set-window-parameter (selected-window) 'infiniscroll-buffer (cons marker bfs))))))

(define-minor-mode infiniscroll-mode
    ""
  :global t
  :init-value nil
  :keymap nil
  (if infiniscroll-mode 
      (progn
        (add-hook 'pre-command-hook #'infiniscroll-pre-command-logic)
        (add-hook 'pre-command-hook #'infiniscroll-post-command-logic))
    (setq cursor-type infiniscroll-restore-ctype) ; just in case
    (remove-hook 'pre-command-hook #'infiniscroll-pre-command-logic)
    (remove-hook 'pre-command-hook #'infiniscroll-post-command-logic)))

(provide 'infiniscroll)
