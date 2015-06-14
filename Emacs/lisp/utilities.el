;;; utilities.el

(require 'column-marker)
(require 'filladapt)
(require 'framemove)
(require 'revbufs)
(require 'tramp)
(require 'uniquify)
(require 'whitespace)

(defun das-kill-buffers-with-prefix (prefix match-what kill-modified)
  (mapc (lambda (this-buffer)
          (when (funcall match-what this-buffer)
            (when (string-prefix-p prefix (funcall match-what this-buffer))
              (unless (and (not kill-modified) (buffer-modified-p this-buffer))
                (kill-buffer this-buffer)))))
        (buffer-list)))

(defun das-kill-buffers-by-directory (dir-name)
  (interactive "Gprefix: ")
  (das-kill-buffers-with-prefix (expand-file-name dir-name)
                                   'buffer-file-name
                                   nil))

(defun das-kill-buffers-by-directory-unsafe (dir-name)
  (interactive "Gprefix: ")
  (das-kill-buffers-with-prefix dir-name
                                   'buffer-file-name
                                   t))

(defun das-kill-magit-grep-buffers ()
  (interactive)
  (das-kill-buffers-with-prefix "*Magit Grep*" 'buffer-name t))

(defun das-kill-tramp-buffers ()
  (interactive)
  (das-kill-buffers-with-prefix "*tramp/" 'buffer-name t))

(defun ktb ()
  (interactive)
  (das-kill-buffers-with-prefix "*tramp/" 'buffer-name t))

(defun das-get-selected-thing-or-region ()
  (if (use-region-p)
      (buffer-substring (region-beginning) (region-end))
    (thing-at-point 'symbol)))

(defun das-tmux-switch-to-directory ()
  "Open the current directory in the current tmux window."
  (interactive)
  (let ((dir default-directory))
    (call-process "tmux" nil nil nil "send-keys" (concat "cd " dir) "Enter")))

(defvar *default-tt-frame* nil)

(defun das-set-default-frame ()
  "Set the current frame as the 'default' frame for emacsclient open operations"
  (interactive)
  (setq *default-tt-frame* (selected-frame)))

(defun das-server-switch-hook ()
  (interactive)
  (let ((target-window (frame-selected-window *default-tt-frame*))
        (target-buffer (current-buffer)))
    (if (not (eql (selected-frame) *default-tt-frame*))
        (progn (bury-buffer)
               (set-window-buffer target-window target-buffer)
               (select-frame-set-input-focus *default-tt-frame*)))))

(defun das-edit-text (title)
  (interactive "sTitle: ")
  (let ((file-name (concat das-thoughts-directory title "."
                           (format-time-string "%d-%m-%Y-%H-%M")
                           ".rst")))
    (find-file file-name)
    (visual-line-mode)
    (flyspell-mode)))

(defun das-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(require 'cite)

(defun das-quote-block ()
  (interactive)
  (cite-cite))

(defun das-initialize-utilities ()
  (windmove-default-keybindings)
  (setq framemove-hook-into-windmove t)
  (add-hook 'server-switch-hook 'das-server-switch-hook)
  (das-set-default-frame)
  (server-start))
