(server-start)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/themes")

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require 'cc-mode)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'color-theme)
(require 'pastebin)
(require 'whitespace)
(require 'filladapt)
(require 'tramp)
(require 'slime)
(require 'magit)
(require 'paredit)
(require 'org-install)
(require 'quack)
(require 'mingus)
(require 'auto-complete)
(require 'bookmark)
(require 'cl)
(require 'revbufs)
(require 'xml-rpc)
(require 'weblogger)
(require 'w3m-load)
(require 'framemove)
(require 'google-c-style)
(load "~/.emacs.d/quilt.el")

(setq-default inhibit-startup-message t
	      font-lock-maximum-decoration t
	      require-final-newline t
	      resize-minibuffer-frame t
	      column-number-mode t
	      transient-mark-mode t
	      next-line-add-newlines nil
	      blink-matching-paren t
	      blink-matching-delay .25
	      vc-follow-symlinks t
	      indent-tabs-mode t
	      tab-width 5
	      c-basic-offset 5
	      edebug-trace t
	      fill-adapt-mode t
          max-lisp-eval-depth 12000
          +expanded-home-directory+ "/home/sanjoy")

(setq url-proxy-services '(("http" . "proxy.work.com:911")))

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-c i")   'imenu)
(global-set-key (kbd "C-x t")   'flyspell-mode)

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)

(setq-default c-basic-offset 5
           tab-width 5
           indent-tabs-mode t
           auto-newline 0)

(add-hook 'c-mode-common-hook
          '(lambda ()
             (define-key c-mode-map "\C-m" 'newline-and-indent)
			 (c-toggle-auto-newline)))

(add-hook 'lisp-mode-hook
          '(lambda ()
             (define-key lisp-mode-map "\C-m" 'newline-and-indent)
			 (paredit-mode +1)
			 (setq indent-tabs-mode nil)))

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (define-key lisp-mode-map "\C-m" 'newline-and-indent)
			 (paredit-mode +1)
			 (setq indent-tabs-mode nil)))

(add-hook 'lisp-interaction-mode-hook 
		  '(lambda ()
			 (paredit-mode +1)
			 (setq indent-tabs-mode nil)))

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('

(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
	(read-kbd-macro paredit-backward-delete-key) nil))

(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)


(define-key c-mode-base-map "\C-m" 'c-context-line-break)

;; Everything in UTF-8

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
(add-to-list 'auto-coding-alist         '("." . utf-8))

;; Color theme

(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))

(setq my-color-themes (list 'color-theme-billw 'color-theme-jsc-dark 
                            'color-theme-sitaramv-solaris 'color-theme-resolve
                            'color-theme-classic 'color-theme-jonadabian-slate
                            'color-theme-kingsajz 'color-theme-shaman
                            'color-theme-subtle-blue 'color-theme-snowish
                            'color-theme-sitaramv-nt 'color-theme-wheat))

;; Set up SLIME

(setq inferior-lisp-program "/usr/bin/sbcl")
(add-to-list 'load-path "~/Documents/Code/Lisp/SLIME")
(require 'slime)
(slime-setup)

;; Set up ORG Mode

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(add-hook 'org-mode-hook 'turn-on-font-lock)

;; RCIRC

; General settings
 (setq rcirc-server-alist '(("82.96.64.4"   :nick "sanjoyd" :full-name "Sanjoy Das")))

(defun irc ()
  (interactive)
  (rcirc-connect "82.96.64.4"    "6667" "sanjoyd")
  (rcirc-track-minor-mode))

(add-hook 'rcirc-mode-hook
		  '(lambda ()
			 (flyspell-mode)))

; Logging
(setq rcirc-log-flag "t"
      rcirc-log-directory "~/.emacs.d/rcirc-log")

(defun kill-mode-buffers (&optional mode)
  "Kill all buffers of this major mode.
   With optional argument MODE, all buffers in major mode MODE are killed
   instead."
  (interactive (list (when current-prefix-arg (ted-read-major-mode))))
  (setq mode (or mode major-mode))
  (when (or current-prefix-arg
            (y-or-n-p (format "Really kill all %s buffers? " mode)))
    (mapc (lambda (buffer)
            (when (with-current-buffer buffer
                    (eq major-mode mode))
              (kill-buffer buffer)))
          (buffer-list))))

(defun rcirc-kill-all-buffers ()
  (interactive)
  (kill-mode-buffers 'rcirc-mode))

(eval-after-load 'rcirc
  '(defun-rcirc-command reconnect (arg)
     "Reconnect the server process."
     (interactive "i")
     (unless process
       (error "There's no process for this target"))
     (let* ((server (car (process-contact process)))
            (port (process-contact process :service))
            (nick (rcirc-nick process))
            channels query-buffers)
       (dolist (buf (buffer-list))
         (with-current-buffer buf
           (when (eq process (rcirc-buffer-process))
             (remove-hook 'change-major-mode-hook
                          'rcirc-change-major-mode-hook)
             (if (rcirc-channel-p rcirc-target)
                 (setq channels (cons rcirc-target channels))
               (setq query-buffers (cons buf query-buffers))))))
       (delete-process process)
       (rcirc-connect server port nick
                      rcirc-default-user-name
                      rcirc-default-user-full-name
                      channels))))

; Nick Colors
(eval-after-load 'rcirc '(require 'rcirc-color))

;; C Mode

(setq-default tab-width 4)
(setq cua-auto-tabify-rectangles nil)

(defadvice align (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice align-regexp (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice indent-relative (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice indent-according-to-mode (around smart-tabs activate)
  (let ((indent-tabs-mode indent-tabs-mode))
    (if (memq indent-line-function
              '(indent-relative
                indent-relative-maybe))
        (setq indent-tabs-mode nil))
    ad-do-it))

(defmacro smart-tabs-advice (function offset)
  (defvaralias offset 'tab-width)
  `(defadvice ,function (around smart-tabs activate)
     (cond
      (indent-tabs-mode
       (save-excursion
         (beginning-of-line)
         (while (looking-at "\t*\\( +\\)\t+")
           (replace-match "" nil nil nil 1)))
       (setq tab-width tab-width)
       (let ((tab-width fill-column)
             (,offset fill-column))
         ad-do-it))
      (t
       ad-do-it))))

(smart-tabs-advice c-indent-line c-basic-offset)
(smart-tabs-advice c-indent-region c-basic-offset)

(add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1)))

;; Saner file and buffer exploring

(ido-mode t)

(setq 
 ido-use-filename-at-point nil ; don't use filename at point (annoying)
 ido-use-url-at-point nil      ; don't use url at point (annoying)
 ido-enable-flex-matching t    ; be flexible
 ido-max-prospects 6           ; don't spam my minibuffer
 ido-confirm-unique-completion nil ; don't wait for RET with unique completion
 ; Always open buffers and files in the current window
 ido-default-file-method 'selected-window
 ido-default-buffer-method 'selected-window)

;; Auto-completion

(global-auto-complete-mode t)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; Set up w3m-el

(setq browse-url-browser-function 'w3m-browse-url)

(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

(global-set-key "\C-xm" 'browse-url-at-point)
(setq w3m-use-cookies t)

;; No backup files

(setq make-backup-files nil)

;; Save the annoying C-x o's
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

;; I use this to edit flex / bison files
(defun crystal-bison-flex-settings ()
  (interactive)
  (c-toggle-syntactic-indentation)
  (c-toggle-electric-mode)
  (setq indent-tabs-mode nil))

;; Magit

(global-set-key (kbd "\C-x\C-a") 'magit-status)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(defun insert-backslash ()
  (interactive)
  (insert " \\")
  (newline-and-indent))

(setq *prev-ret-binding* nil)

(defun crystal-write-c-macro ()
  (interactive)
  (if (null *prev-ret-binding*)
      (progn
        (setq *prev-ret-binding* (lookup-key c-mode-map (kbd "RET")))
        (define-key c-mode-map (kbd "RET")
               'insert-backslash))
    (progn
      (define-key c-mode-map (kbd "RET")
        *prev-ret-binding*)
      (setq *prev-ret-binding* nil))))

(defun prefix-p (prefix input)
  (let ((len (length prefix)))
    (cond ((< (length input)
              len)
           nil)
          (t (equal (substring input 0 len)
                  prefix)))))

(setq +google-directories+
      '("/home/sanjoy/Source/v8/"))

(defun is-google-dir (list-iter file-name)
  (if (null list-iter)
      nil
    (if (prefix-p (car list-iter)
                  file-name)
        t
      (is-google-dir (cdr list-iter)
                     file-name))))

(defun google-c-style-hook ()
  (when (string-match ".*\\.\\(cc\\|h\\|c\\)" (buffer-file-name))
    (when (is-google-dir +google-directories+ (buffer-file-name))
      (google-set-c-style))))

(add-hook 'find-file-hooks 'google-c-style-hook)

(defun crystal-kill-buffers-by-directory (dir-name)
  (interactive "DDirectory: ")
  (if (prefix-p "~" dir-name)
      (setq dir-name (concat +expanded-home-directory+ (substring dir-name 1))))
  (mapc (lambda (this-buffer)
          (when (buffer-file-name this-buffer)
            (when (prefix-p dir-name (buffer-file-name this-buffer))
              (unless (buffer-modified-p this-buffer)
                (kill-buffer this-buffer)))))
        (buffer-list)))
