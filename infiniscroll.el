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

(defun infiniscroll-not-comint-buffer ()
  "comint has their own scroll restore system usually.
this would just interfere or something."
  (derived-mode-p 'comint-mode))

(defun infiniscroll-not-sexp-command ()
  (not (member last-command '(forward-sexp backward-sexp forward-char backward-char))))

(defcustom infiniscroll-buffer-ignore-fns 
  '(minibufferp infiniscroll-not-comint-buffer)
  "Predicates that run to tell infiniscroll which (current-buffer)s not to run in"
  :type 'hook)

(defvar-local infiniscroll-restore-ctype cursor-type)
(defvar-local infiniscroll-touched nil)

(defun infiniscroll-purge ()
  (interactive)
  (cl-loop for b in (buffer-list)
        do (with-current-buffer b
             (if infiniscroll-touched
                 (setq cursor-type infiniscroll-restore-ctype))))
  (cl-loop for w in (window-list)
        if (window-parameter w 'infiniscroll-buffer)
        do (set-window-parameter w 'infiniscroll-buffer (list))))

(defun infiniscroll-post-command-logic () 
  (if (notany #'funcall infiniscroll-buffer-ignore-fns) 
      ;; Make cursor visible or invisible
      (let* ((bfs (window-parameter (selected-window) 'infiniscroll-buffer))
             (pt (cl-loop for m in bfs
                       if (eq (marker-buffer m) (current-buffer))
                       do (cl-return m))))
        (when (markerp pt)
          ;; (message "%s %s %s %s %s" last-command this-command (marker-position pt) (point) (not (member this-command infiniscroll-commands)))
          (if (and (not (member this-command infiniscroll-commands))
                   ;; This has to be here because somehow its effect affects infiniscroll-pre-command-logic restoration somehow
                   ;; without it, stuff like end-of-buffer will cause a scroll restore before a self-insert-command
                   (not (member last-command infiniscroll-commands)))
              ;; (setf pt (point))
              (setf (marker-position pt) (point))
            (if (pos-visible-in-window-p pt (selected-window)) 
                (setf cursor-type infiniscroll-restore-ctype)
              (setf cursor-type nil
                    infiniscroll-touched t)))))))

(defun infiniscroll-pre-command-logic ()
  (if (notany #'funcall infiniscroll-buffer-ignore-fns) 
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
            (set-window-parameter (selected-window) 'infiniscroll-buffer (cons marker bfs)))))))

(define-minor-mode infiniscroll-mode
    ""
  :global t
  :init-value nil
  :keymap nil
  (infiniscroll-purge)
  (if infiniscroll-mode 
      (progn
        (add-hook 'pre-command-hook #'infiniscroll-pre-command-logic)
        (add-hook 'pre-command-hook #'infiniscroll-post-command-logic))
    (setq cursor-type infiniscroll-restore-ctype) ; just in case
    (remove-hook 'pre-command-hook #'infiniscroll-pre-command-logic)
    (remove-hook 'pre-command-hook #'infiniscroll-post-command-logic)))

(provide 'infiniscroll)
