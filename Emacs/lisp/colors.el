;;; colors.el

(add-to-list 'custom-theme-load-path "~/.emacs.d/third-party/zenburn-emacs/")

(defun das-install-color-theme ()
  (load-theme 'zenburn t nil))
