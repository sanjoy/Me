;;; static-configuration.el

(defun das-clear-up-ui ()
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (setq ring-bell-function #'ignore))

(defun das-everything-in-utf8 ()
  (prefer-coding-system                   'utf-8)
  (set-language-environment               "utf-8")
  (set-default-coding-systems             'utf-8)
  (setq file-name-coding-system           'utf-8)
  (setq default-buffer-file-coding-system 'utf-8)
  (setq coding-system-for-write           'utf-8)
  (set-keyboard-coding-system             'utf-8)
  (set-terminal-coding-system             'utf-8)
  (set-clipboard-coding-system            'utf-8)
  (set-selection-coding-system            'utf-8)
  (setq default-process-coding-system     '(utf-8 . utf-8))
  (add-to-list 'auto-coding-alist         '("." . utf-8)))

(defun das-miscellaneous-settings ()
  (setq exec-path (append exec-path '("/usr/local/bin")))
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq-default blink-matching-delay .25
                browse-url-browser-function 'browse-url-generic
                browse-url-generic-program "google-chrome"
                column-number-mode t compilation-scroll-output t
                inhibit-startup-message t max-lisp-eval-depth
                12000 uniquify-buffer-name-style 'reverse
                vc-follow-symlinks t x-select-enable-clipboard t
                ispell-program-name "/usr/local/bin/ispell"))

(defun das-set-special-directories ()
  (setq-default das-source-directory "/Users/sanjoy/Code/"
                das-thoughts-directory "~/Documents/Thoughts/"))

(defun das-unprotect-commands ()
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil))

(defun das-set-default-font ()
  (let ((default-font-name "-*-monaco-medium-r-*--10-*-*-*-*-*-*-*"))
    (set-default-font default-font-name)))

(defun das-initialize-fonts ()
  (das-set-default-font)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (select-frame frame)
              (das-set-default-font))))

(defun das-initialize-package ()
  (require 'package)
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")))
  (package-initialize))

(defun das-import-path ()
  (exec-path-from-shell-initialize))

(defun das-initialize-global-hooks ()
  (setq confirm-kill-emacs 'yes-or-no-p))
