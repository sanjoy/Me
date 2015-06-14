(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/third-party"))

(load "static-configuration.el")
(das-clear-up-ui)
(das-everything-in-utf8)
(das-initialize-fonts)
(das-initialize-package)
(das-miscellaneous-settings)
(das-set-special-directories)
(das-unprotect-commands)
(das-initialize-global-hooks)

(load "colors.el")
(das-install-color-theme)

(load "modes-and-styles.el")
(das-initialize-modes)  
(das-initialize-styles)

; (das-initialize-agda-mode)
(das-initialize-c-mode)
(das-initialize-ido-mode)
(das-initialize-lisp-mode)
(das-initialize-haskell-mode)
(das-initialize-rust-mode)
(das-initialize-magit-mode)
(das-initialize-p4-mode)
;(das-initialize-slime)
(das-initialize-tramp-mode)

(load "utilities.el")
(das-initialize-utilities)

(load "keybindings.el")
(das-initialize-keybindings)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-include-dirs (quote ("/Users/sanjoy/Code/agda-stdlib/src" ".")))
 '(agda2-program-name "/Users/sanjoy/Library/Haskell/bin/agda")
 '(ggtags-enable-navigation-keys nil)

 '(haskell-mode-hook
   (quote ((lambda nil
           (setq show-trailing-whitespace t) 
             (local-set-key (kbd "C-c C-c")  (quote comment-region)) 
             (local-set-key (kbd "C-c C-u") (quote uncomment-region)) 
             (column-marker-1 80)) 
           turn-on-haskell-indentation 
           turn-on-haskell-doc-mode 
           interactive-haskell-mode)))

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
