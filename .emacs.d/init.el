(server-start)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/themes")

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require 'ansi-color)
(require 'auto-complete)
(require 'bookmark)
(require 'cc-mode)
(require 'cl)
(require 'color-theme)
(require 'column-marker)
(require 'ffap)
(require 'filladapt)
(require 'flex-mode)
(require 'framemove)
(require 'go-mode-load)
(require 'google-c-style)
(require 'magit)
(require 'mingus)
(require 'org-install)
(require 'paredit)
(require 'pastebin)
(require 'php-mode)
(require 'quack)
(require 'quilt-mode)
(require 'recentf)
(require 'revbufs)
(require 'saveplace)
(require 'slime)
(require 'tramp)
(require 'uniquify)
(require 'w3m-load)
(require 'weblogger)
(require 'whitespace)
(require 'xml-rpc)

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
          rcirc-authinfo-file-name (expand-file-name "~/.rcirc-authinfo"))

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-c i")   'imenu)
(global-set-key (kbd "C-x t")   'flyspell-mode)

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)

(setq-default c-basic-offset 5
           tab-width 5
           indent-tabs-mode t
           auto-newline 0)

(setq c-offsets-alist '((innamespace . 0)))

(add-hook 'c-mode-common-hook
          '(lambda ()
             (define-key c-mode-map "\C-m" 'newline-and-indent)
			 (c-toggle-auto-newline)
             (setq c-backslash-max-column 80)))

(add-hook 'c-mode-common-hook '(lambda ()
                                 (interactive)
                                 (column-marker-1 80)))

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
(setq rcirc-server-alist
      '(("irc.freenode.net" :nick "sanjoyd" :full-name "Sanjoy Das")))

(rcirc-track-minor-mode 1)

(add-hook 'rcirc-mode-hook
          '(lambda ()
             (flyspell-mode)
             (set (make-local-variable 'scroll-conservatively)
                  8192)))

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

(defun rcirc-load-authinfo ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents-literally rcirc-authinfo-file-name)
    (goto-char (point-min))
    (setq rcirc-authinfo (read (current-buffer)))))

(defun irc ()
  (interactive)
  (rcirc-load-authinfo)
  (rcirc-connect "irc.freenode.net" "6667" "sanjoyd"))

(defun rcirc-kill-all-buffers ()
  (interactive)
  (kill-mode-buffers 'rcirc-mode))

(defun crystal-rcirc-dance ()
  (mapc (lambda (c)
          (insert (concat "/me dances :D" (char-to-string c) "-<"))
          (rcirc-send-input)
          (sleep-for 0 500)) "/|\\|"))

(eval-after-load 'rcirc
  '(defun-rcirc-command dance (arg)
     "Dance."
     (interactive "i")
     (crystal-rcirc-dance)))

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
  (setq dir-name (expand-file-name dir-name))
  (mapc (lambda (this-buffer)
          (when (buffer-file-name this-buffer)
            (when (prefix-p dir-name (buffer-file-name this-buffer))
              (unless (buffer-modified-p this-buffer)
                (kill-buffer this-buffer)))))
        (buffer-list)))

;; Mingus

(defmacro crystal-kill-mingus-after-use (function-to-call)
  `(lambda ()
     (interactive)
     (funcall ',function-to-call)
     (let ((mingus-buffer (get-buffer "*Mingus*")))
       (unless (eql mingus-buffer (current-buffer))
         (kill-buffer mingus-buffer)))))

;; Music + Coding = :D

(global-set-key (kbd "<end>")    (crystal-kill-mingus-after-use mingus-pause))
(global-set-key (kbd "<prior>")  (crystal-kill-mingus-after-use mingus-prev))
(global-set-key (kbd "<next>")   (crystal-kill-mingus-after-use mingus-next))
(global-set-key (kbd "<f9>")     'mingus)

;; Set up "email mode".

(defun crystal-email-mode ()
  (interactive)
  (longlines-mode)
  (flyspell-mode))

;; Thunderbird external editor extension opens emails as files with 
;; the extension eml
(add-to-list 'auto-mode-alist '("\\.eml\\'" . crystal-email-mode))

(setq +thoughts-directory+ "~/Documents/Thoughts/")
(setq +notes-directory+ "~/Documents/Notes/")

(defun crystal-edit-text ()
  (interactive)
  (let ((file-name (concat +thoughts-directory+ (format-time-string "%d-%m-%Y-%H-%M"))))
    (find-file file-name)
    (longlines-mode)
    (flyspell-mode)))

(defun crystal-edit-note ()
  (interactive)
  (let ((file-name (concat +notes-directory+ (format-time-string "%d-%m-%Y-%H-%M"))))
    (find-file file-name)
    (org-mode)))

(global-set-key (kbd "C-c t")
                'crystal-edit-text)

(global-set-key (kbd "C-c n")
                'crystal-edit-note)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(global-unset-key (kbd "<insert>"))

(defun crystal-save-current-directory ()
  "Save the current directory to the file ~/.emacs.d/current-directory"
  (interactive)
  (let ((dir default-directory))
    (with-current-buffer (find-file-noselect "~/.emacs.d/current-directory")
      (delete-region (point-min) (point-max))
      (insert (concat dir "\n"))
      (save-buffer)
      (kill-buffer (current-buffer)))))

(global-set-key (kbd "<M-f9>") 'crystal-save-current-directory)
