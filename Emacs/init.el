(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/third-party")

(load "static-configuration.el")
(sanjoy-clear-up-ui)
(sanjoy-everything-in-utf8)
(sanjoy-initialize-fonts)
(sanjoy-initialize-package)
(sanjoy-miscellaneous-settings)
(sanjoy-set-special-directories)
(sanjoy-unprotect-commands)
(sanjoy-initialize-global-hooks)

(load "colors.el")
(sanjoy-install-color-theme)

(load "modes-and-styles.el")
(sanjoy-initialize-modes)  
(sanjoy-initialize-styles)

(sanjoy-initialize-agda-mode)
(sanjoy-initialize-c-mode)
(sanjoy-initialize-ido-mode)
(sanjoy-initialize-lisp-mode)
(sanjoy-initialize-haskell-mode)
(sanjoy-initialize-rust-mode)
(sanjoy-initialize-tex-mode)
(sanjoy-initialize-magit-mode)
(sanjoy-initialize-p4-mode)
(sanjoy-initialize-slime)

(load "utilities.el")
(sanjoy-initialize-utilities)

(load "keybindings.el")
(sanjoy-initialize-keybindings)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-include-dirs (quote ("/Users/sanjoy/Code/agda-stdlib/src" ".")))
 '(agda2-program-name "/Users/sanjoy/Library/Haskell/bin/agda")
 '(haskell-mode-hook (quote ((lambda nil (interactive) (setq show-trailing-whitespace t) (local-set-key (kbd "C-c C-c") (quote comment-region)) (local-set-key (kbd "C-c C-u") (quote uncomment-region)) (column-marker-1 80)) turn-on-haskell-indentation turn-on-haskell-doc-mode interactive-haskell-mode)))
 '(w3-default-homepage "http://google.com"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-datatype-face ((t (:foreground "light blue"))))
 '(agda2-highlight-function-face ((t (:foreground "light blue"))))
 '(agda2-highlight-module-face ((t (:foreground "light green"))))
 '(agda2-highlight-number-face ((t (:foreground "light green"))))
 '(agda2-highlight-postulate-face ((t (:foreground "light blue"))))
 '(agda2-highlight-primitive-face ((t (:foreground "light blue"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "light blue"))))
 '(agda2-highlight-record-face ((t (:foreground "light blue"))))
 '(agda2-highlight-symbol-face ((t (:foreground "DarkOrange3"))))
 '(moinmoin-url ((t (:foreground "lightblue" :height 1.0))))
 '(moinmoin-url-title ((t (:foreground "lightblue" :underline t)))))
