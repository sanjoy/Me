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
(sanjoy-initialize-tex-mode)
(sanjoy-initialize-magit-mode)
(sanjoy-initialize-p4-mode)

(load "utilities.el")
(sanjoy-initialize-utilities)

(load "keybindings.el")
(sanjoy-initialize-keybindings)
