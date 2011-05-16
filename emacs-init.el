(server-start)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/emacs-codepad")

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require 'ansi-color)
(require 'auto-complete)
(require 'cc-mode)
(require 'cl)
(require 'color-theme)
(require 'column-marker)
(require 'epa-file)
(require 'ffap)
(require 'filladapt)
(require 'framemove)
(require 'go-mode-load)
(require 'google-c-style)
(require 'haskell-mode)
(require 'llvm-mode)
(require 'magit)
(require 'mingus)
(require 'org-install)
(require 'paredit)
(require 'php-mode)
(require 'quack)
(require 'quilt-mode)
(require 'recentf)
(require 'revbufs)
(require 'saveplace)
(require 'slime)
(require 'tablegen-mode)
(require 'tbemail)
(require 'tramp)
(require 'twittering-mode)
(require 'uniquify)
(require 'w3m-load)
(require 'whitespace)

(set-default-font "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")

(setq-default 
 blink-matching-delay .25
 blink-matching-paren t
 c-basic-offset 5
 c-macro-prompt-flag t
 code-directory "/home/sanjoy/code/"
 column-number-mode t
 edebug-trace t
 fill-adapt-mode t
 font-lock-maximum-decoration t
 indent-tabs-mode t
 inhibit-startup-message t
 max-lisp-eval-depth 12000
 next-line-add-newlines nil
 rcirc-authinfo-file-name (expand-file-name "~/.rcirc-authinfo")
 rcirc-default-user-full-name "Sanjoy Das"
 rcirc-default-user-name "sanjoyd"
 require-final-newline t
 resize-minibuffer-frame t
 src-directory  "/home/sanjoy/src/"
 tab-width 5
 transient-mark-mode t
 twittering-use-master-password t
 vc-follow-symlinks t
 )

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
             (setq c-backslash-max-column 79)))

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

;; Set up SLIME

(setq inferior-lisp-program "/usr/bin/sbcl")
(add-to-list 'load-path     (concat code-directory "/Practice/LISP/SLIME"))
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
      '(("irc.freenode.net" :nick "sanjoyd" :full-name "Sanjoy Das"
         :channels ("##geekbhaat" "##klug" "#v8" "#ucombinator" "#haskell" "##c" "##cc" "##workingset"))
        ("irc.oftc.net"     :nick "sanjoyd" :full-name "Sanjoy Das"
         :channels ("#llvm"))
        ("127.0.0.1"        :nick "sanjoy"  :full-name "Sanjoy Das")))

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

(rcirc-load-authinfo)

(defun rcirc-kill-all-buffers ()
  (interactive)
  (kill-mode-buffers 'rcirc-mode))

(defun my-rcirc-dance ()
  (mapc (lambda (c)
          (insert (concat "/me dances :D" (char-to-string c) "-<"))
          (rcirc-send-input)
          (sleep-for 0 500)) "/|\\|"))

(eval-after-load 'rcirc
  '(defun-rcirc-command dance (arg)
     "Dance."
     (interactive "i")
     (my-rcirc-dance)))

                                        ; Nick Colors
(eval-after-load 'rcirc '(require 'rcirc-color))

;; Auto away
(defvar rcirc-auto-away-server-regexps
  '("freenode" "oftc"))

(defvar rcirc-auto-away-after 1800) ;; Auto-away after this many seconds

(defvar rcirc-auto-away-reason "AFK.") ;; Reason sent to server when auto-away

(defun rcirc-auto-away ()
  (message "Auto away activated.")
  (rcirc-auto-away-1 rcirc-auto-away-reason)
  (add-hook 'post-command-hook 'rcirc-auto-unaway))

(defun rcirc-auto-away-1 (reason)
  (let ((regexp (mapconcat (lambda (x) (concat "\\(" x "\\)")) 
                           rcirc-auto-away-server-regexps "\\|")))
    (dolist (process (rcirc-process-list))
      (when (string-match regexp (process-name process))
        (rcirc-send-string process (concat "AWAY :" reason))))))

(defun rcirc-auto-unaway ()
  (remove-hook 'post-command-hook 'rcirc-auto-unaway)
  (rcirc-auto-away-1 ""))

(run-with-idle-timer rcirc-auto-away-after t 'rcirc-auto-away)

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

;; No backup files

(setq make-backup-files nil)

;; Save the annoying C-x o's

(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

;; Magit

(global-set-key (kbd "\C-x\C-a") 'magit-status)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(defun insert-backslash ()
  (interactive)
  (insert " \\")
  (newline-and-indent))

(setq *prev-ret-binding* nil)

(c-add-style "llvm.org"
             '((fill-column . 80)
               (c++-indent-level . 2)
               (c-basic-offset . 2)
               (indent-tabs-mode . nil)
               (c-offsets-alist . ((innamespace 0)))))

(setq +style-directories+
      (list (cons (concat src-directory "v8/")   "Google")
            (cons (concat src-directory "llvm/") "llvm.org")))

(defun safe-str-match (a b)
  (if (or (null a)
          (null b))
      nil
    (string-match a b)))

(defun my-get-style (list-iter file-name)
  (if (null list-iter)
      nil
    (if (safe-str-match (car (car list-iter))
                        file-name)
        (cdr (car list-iter))
      (my-get-style (cdr list-iter)
                    file-name))))

(defun my-c-style ()
  (let ((style (my-get-style +style-directories+ (buffer-file-name))))
    (when style
      (c-set-style style))))

(add-hook 'c-mode-common-hook 'my-c-style)

(defun my-kill-buffers-by-directory (dir-name)
  (interactive "DDirectory: ")
  (setq dir-name (expand-file-name dir-name))
  (mapc (lambda (this-buffer)
          (when (buffer-file-name this-buffer)
            (when (safe-str-match dir-name (buffer-file-name this-buffer))
              (unless (buffer-modified-p this-buffer)
                (kill-buffer this-buffer)))))
        (buffer-list)))

(global-set-key (kbd "C-c C-k")
                'my-kill-buffers-by-directory)

;; Mingus

(defmacro my-kill-mingus-after-use (function-to-call)
  `(lambda ()
     (interactive)
     (funcall ',function-to-call)
     (let ((mingus-buffer (get-buffer "*Mingus*")))
       (unless (eql mingus-buffer (current-buffer))
         (kill-buffer mingus-buffer)))))

;; Music + Coding = :D

(global-set-key (kbd "<end>")    (my-kill-mingus-after-use mingus-pause))
(global-set-key (kbd "<prior>")  (my-kill-mingus-after-use mingus-prev))
(global-set-key (kbd "<next>")   (my-kill-mingus-after-use mingus-next))
(global-set-key (kbd "<f9>")     'mingus)

;; Set up "email mode".
(defun my-email-additions ()
  (interactive)
  (flyspell-mode)
  (longlines-mode))

(add-hook 'tbemail-mode-hook 'my-email-additions)

(setq +thoughts-directory+ "~/rest/writeups/")

(defun write-mode ()
  (interactive)
  (html-mode)
  (longlines-mode)
  (flyspell-mode))

(defun my-edit-text (title)
  (interactive "sTitle: ")
  (let ((file-name (concat +thoughts-directory+ title "." (format-time-string "%d-%m-%Y-%H-%M"))))
    (find-file file-name)
    (write-mode)))

(global-set-key (kbd "C-c t")
                'my-edit-text)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(global-unset-key (kbd "<insert>"))

(defun my-tmux-switch-to-directory ()
  "Open the current directory in the current tmux window."
  (interactive)
  (let ((dir default-directory))
    (call-process "tmux" nil nil nil "send-keys" (concat "cd " dir) "Enter")))

(global-set-key (kbd "C-c d") 'my-tmux-switch-to-directory)

;; CodePad.org integration

(autoload 'codepad-paste-region "codepad" "Paste region to codepad.org." t)
(autoload 'codepad-paste-buffer "codepad" "Paste buffer to codepad.org." t)
(autoload 'codepad-fetch-code "codepad" "Fetch code from codepad.org." t)


;; Auto-encryption / decryption

(epa-file-enable)

;; Make windmove work in org-mode:

(add-hook 'org-shiftup-final-hook    'windmove-up)
(add-hook 'org-shiftleft-final-hook  'windmove-left)
(add-hook 'org-shiftdown-final-hook  'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))
