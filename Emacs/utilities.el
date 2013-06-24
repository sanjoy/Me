;;; utilities.el

(require 'column-marker)
(require 'filladapt)
(require 'framemove)
(require 'revbufs)
(require 'tramp)
(require 'uniquify)
(require 'whitespace)

(defun sanjoy-kill-buffers-by-directory (dir-name)
  (interactive "DDirectory: ")
  (setq dir-name (expand-file-name dir-name))
  (mapc (lambda (this-buffer)
          (when (and dir-name (buffer-file-name this-buffer))
            (when (string-prefix-p dir-name (buffer-file-name this-buffer))
              (unless (buffer-modified-p this-buffer)
                (kill-buffer this-buffer)))))
        (buffer-list)))

(defun sanjoy-get-selected-thing-or-region ()
  (if (use-region-p)
      (buffer-substring (region-beginning) (region-end))
    (thing-at-point 'symbol)))

(defun sanjoy-tmux-switch-to-directory ()
  "Open the current directory in the current tmux window."
  (interactive)
  (let ((dir default-directory))
    (call-process "tmux" nil nil nil "send-keys" (concat "cd " dir) "Enter")))

(defvar *default-tt-frame* nil)

(defun sanjoy-set-default-frame ()
  "Set the current frame as the 'default' frame for emacsclient open operations"
  (interactive)
  (setq *default-tt-frame* (selected-frame)))

(defun sanjoy-server-switch-hook ()
  (interactive)
  (let ((target-window (frame-selected-window *default-tt-frame*))
        (target-buffer (current-buffer)))
    (if (not (eql (selected-frame) *default-tt-frame*))
        (progn (bury-buffer)
               (set-window-buffer target-window target-buffer)
               (select-frame-set-input-focus *default-tt-frame*)))))

(defun sanjoy-edit-text (title)
  (interactive "sTitle: ")
  (let ((file-name (concat sanjoy-thoughts-directory title "."
                           (format-time-string "%d-%m-%Y-%H-%M")
                           ".rst")))
    (find-file file-name)
    (longlines-mode)
    (flyspell-mode)))

(defun sanjoy-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(require 'cite)

(defun sanjoy-quote-block ()
  (interactive)
  (cite-cite))

(defun sanjoy-initialize-utilities ()
  (windmove-default-keybindings)
  (setq framemove-hook-into-windmove t)
  (add-hook 'server-switch-hook 'sanjoy-server-switch-hook)
  (sanjoy-set-default-frame)
  (server-start))
