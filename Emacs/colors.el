;;; colors.el

(add-to-list 'custom-theme-load-path "~/.emacs.d/third-party/zenburn-emacs/")

(defun sanjoy-install-color-theme ()
  (load-theme 'zenburn t nil))
