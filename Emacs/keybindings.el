;;; keybindings.el

(defun sanjoy-initialize-keybindings ()
  (global-unset-key (kbd "<insert>"))
  (global-set-key (kbd "C-c C-k") 'sanjoy-kill-buffers-by-directory)
  (global-set-key (kbd "C-c C-f") 'flyspell-mode)
  (global-set-key (kbd "C-c C-b") 'browse-url-at-point)
  (global-set-key (kbd "C-c r") 'revert-buffer)
  (global-set-key (kbd "C-c s") 'sanjoy-grep-find)
  (global-set-key (kbd "\C-x\C-a\C-a") 'magit-status)
  (global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
  (global-set-key (kbd "C-c t") 'sanjoy-edit-text)
  (global-set-key (kbd "C-c d") 'sanjoy-tmux-switch-to-directory)
  (global-set-key (kbd "C-c e") 'sanjoy-eval-and-replace)
  (global-set-key (kbd "C-c C-h") 'man))
