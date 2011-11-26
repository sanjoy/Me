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
(require 'rebase-mode)
(require 'revbufs)
(require 'slime)
(require 'tablegen-mode)
(require 'tbemail)
(require 'timeclock)
(require 'tramp)
(require 'twittering-mode)
(require 'uniquify)
(require 'whitespace)

;;   GENERAL
;; -------------------------------------------------------------------

;;; Variables which shall not be customized further.

(setq-default 
 blink-matching-delay .25
 blink-matching-paren t
 c-basic-offset 5
 c-macro-prompt-flag t
 c-offsets-alist '((innamespace . 0))
 code-directory "/home/sanjoy/code/"
 column-number-mode t
 default-font-name "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"
 edebug-trace t
 fill-adapt-mode t
 font-lock-maximum-decoration t
 indent-tabs-mode nil
 inhibit-startup-message t
 max-lisp-eval-depth 12000
 next-line-add-newlines nil
 rcirc-authinfo-file-name (expand-file-name "~/.rcirc-authinfo")
 rcirc-default-user-full-name "Sanjoy Das"
 rcirc-default-user-name "sanjoyd"
 require-final-newline t
 resize-minibuffer-frame t
 src-directory  "/home/sanjoy/src/"
 tab-width 8
 transient-mark-mode t
 twittering-use-master-password t
 vc-follow-symlinks t)

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

;;; I keep entering insert mode accidentally
(global-unset-key (kbd "<insert>"))

;;; Set the correct font at startup ... 
(set-default-font default-font-name)

;;; ... and on creation of new frames
(add-hook 'after-make-frame-functions '(lambda (frame)
                                         (interactive)
                                         (select-frame frame)
                                         (set-default-font default-font-name)))

;;; Misc. functions and keybindings
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

(global-set-key (kbd "C-c r")
                'revert-buffer)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;;; Auto-encryption / decryption
(epa-file-enable)

(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))

;;; Timeclock
(global-set-key (kbd "C-c qi") 'timeclock-in)
(global-set-key (kbd "C-c qo") 'timeclock-out)
(global-set-key (kbd "C-c qs") 'timeclock-status-string)


;;   LISP MODE
;; -------------------------------------------------------------------

(defun my-lisp-hook (interactive-p)
  (if (not interactive-p)
      (define-key lisp-mode-map (kbd "RET") 'newline-and-indent))
  (paredit-mode +1)
  (setq indent-tabs-mode nil))

(add-hook 'lisp-mode-hook '(lambda () (my-lisp-hook nil)))
(add-hook 'emacs-lisp-mode-hook '(lambda () (my-lisp-hook nil)))
(add-hook 'lisp-interaction-mode-hook '(lambda () (my-lisp-hook t)))

;;; Set up SLIME
(setq inferior-lisp-program "/usr/bin/sbcl --noinform --no-linedit")
(add-to-list 'load-path (concat code-directory "/Practice/LISP/SLIME"))
(slime-setup)


;;   WINDMOVE
;; -------------------------------------------------------------------

(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)


;;   ORG MODE
;; -------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)

;;; Windmove under Org mode
(add-hook 'org-shiftup-final-hook    'windmove-up)
(add-hook 'org-shiftleft-final-hook  'windmove-left)
(add-hook 'org-shiftdown-final-hook  'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;;   RCIRC
;; -------------------------------------------------------------------

;;; General settings
(setq rcirc-server-alist
      '(("irc.freenode.net" :nick "sanjoyd" :full-name "Sanjoy Das"
         :channels ("##geekbhaat" "#klug-devel" "#v8" "#ucombinator" "#haskell" "##c"
                    "##cc" "##workingset" "#lisp" "##categorytheory"))
        ("irc.oftc.net" :nick "sanjoyd" :full-name "Sanjoy Das"
         :channels ("#llvm"))
        ("127.0.0.1" :nick "sanjoy"  :full-name "Sanjoy Das")))

;;; Notifications in the modeline
(rcirc-track-minor-mode 1)

(add-hook 'rcirc-mode-hook
          '(lambda ()
             (flyspell-mode)
             (set (make-local-variable 'scroll-conservatively)
                  8192)))

;;; Logging everything
(setq rcirc-log-flag "t"
      rcirc-log-directory "~/.emacs.d/rcirc-log")

;;; Kill all RCIRC buffers with one command
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

;;; Load password
(defun rcirc-load-authinfo ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents-literally rcirc-authinfo-file-name)
    (goto-char (point-min))
    (setq rcirc-authinfo (read (current-buffer)))))
(rcirc-load-authinfo)

;;; :D
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

;;; Nick Colors
(eval-after-load 'rcirc '(require 'rcirc-color))

;;; Auto away
(defvar rcirc-auto-away-server-regexps
  '("freenode" "oftc"))

;;;; Auto away after this many seconds
(defvar rcirc-auto-away-after 1800)

;;;; Reason sent to server when auto-away
(defvar rcirc-auto-away-reason "Saving kittens.")
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


;;   IDO BUFFER & FILE MANAGEMENT
;; -------------------------------------------------------------------

(ido-mode t)

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

;;; Basic settings
(setq 
;;;; Don't use filename at point (annoying)
 ido-use-filename-at-point nil
;;;; Don't use url at point (annoying)
 ido-use-url-at-point nil
;;;; Be flexible
 ido-enable-flex-matching t
;;;; Don't spam minibuffer
 ido-max-prospects 6
;;;; Don't wait for RET with unique completion
 ido-confirm-unique-completion nil
;;;; Always open buffers and files in the current window
 ido-default-file-method 'selected-window
 ido-default-buffer-method 'selected-window)

;;; Auto-completion
(global-auto-complete-mode t)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;;; No backup files
(setq make-backup-files nil)


;;   MAGIT
;; -------------------------------------------------------------------
(global-set-key (kbd "\C-x\C-a") 'magit-status)


;;   HASKELL MODE
;; -------------------------------------------------------------------

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook '(lambda ()
                                (interactive)
                                (setq show-trailing-whitespace t)))


;;   C & C++ MODE
;; -------------------------------------------------------------------

(c-add-style "llvm.org"
             '((fill-column . 80)
               (c++-indent-level . 2)
               (c-basic-offset . 2)
               (indent-tabs-mode . nil)
               (c-offsets-alist . ((innamespace 0)))))

(setq +style-directories+
      (mapcar (lambda (x)
                (cons (concat src-directory (car x) "/") (cdr x)))
              '(("v8" . (lambda ()
                          (c-set-style "Google")))
                ("gofrontend" . (lambda ()
                                  (c-set-style "Google")))
                ("gpython" . (lambda ()
                               (c-set-style "Google")))
                ("llvm" . (lambda ()
                            (c-set-style "llvm.org")))
                ("dragonegg" . (lambda ()
                                 (c-set-style "llvm.org")))
                ("gdb" . (lambda ()
                           (c-set-style "gnu")))
                ("gcc" . (lambda ()
                           (c-set-style "gnu")))
                ("Snippets" . (lambda ()
                                (c-set-style "Google")))
                ("webkit" . (lambda ()
                              (column-marker-1 -1)
                              (setq show-trailing-whitespace nil))))))

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
    (if style
      (funcall style))))

(defun my-c-mode-common-hook ()
  (c-toggle-auto-newline -1)
  (c-set-offset 'inextern-lang 0)
  (column-marker-1 80)
  (define-key c-mode-map (kbd "RET") 'newline-and-indent)
  (define-key c++-mode-map (kbd "RET") 'newline-and-indent)
  (setq c-backslash-max-column 79)
  (flyspell-prog-mode)
  (setq show-trailing-whitespace t)
  (my-c-style))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(global-set-key (kbd "C-c i") 'imenu)

;;; Display the name of the current function at the top of the window
(which-func-mode)

(delete (assoc 'which-func-mode mode-line-format) mode-line-format)

(setq which-func-header-line-format
      '(which-func-mode
        ("" which-func-format)))

(defadvice which-func-ff-hook (after header-line activate)
  (when which-func-mode
    (delete (assoc 'which-func-mode mode-line-format) mode-line-format)
    (setq header-line-format which-func-header-line-format)))

(custom-set-faces
 '(which-func ((t (:foreground "green")))))

;;; For M-x compile
(setq compilation-scroll-output t)

;;   MINGUS
;; -------------------------------------------------------------------

(defmacro my-kill-mingus-after-use (function-to-call)
  `(lambda ()
     (interactive)
     (funcall ',function-to-call)
     (let ((mingus-buffer (get-buffer "*Mingus*")))
       (unless (eql mingus-buffer (current-buffer))
         (kill-buffer mingus-buffer)))))

(global-set-key (kbd "<f9>")     'mingus)

(custom-set-variables
 '(mingus-mode-line-show-status nil))


;;   EMAIL MODE
;; -------------------------------------------------------------------

(defun my-email-additions ()
  (interactive)
  (flyspell-mode))

(add-hook 'tbemail-mode-hook 'my-email-additions)


;;   PLAIN OLD TEXT
;; -------------------------------------------------------------------

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

(defun my-tmux-switch-to-directory ()
  "Open the current directory in the current tmux window."
  (interactive)
  (let ((dir default-directory))
    (call-process "tmux" nil nil nil "send-keys" (concat "cd " dir) "Enter")))

(global-set-key (kbd "C-c d") 'my-tmux-switch-to-directory)


;;   CODEPAD.ORG INTEGRATION
;; -------------------------------------------------------------------

;;; Does not work very well under the proxy
(autoload 'codepad-paste-region "codepad" "Paste region to codepad.org." t)
(autoload 'codepad-paste-buffer "codepad" "Paste buffer to codepad.org." t)
(autoload 'codepad-fetch-code "codepad" "Fetch code from codepad.org." t)


;;   FRAMES
;; -------------------------------------------------------------------

(defvar *default-tt-frame* nil)

(defun my-set-default-frame ()
  "Set the current frame as the 'default' frame for emacsclient open operations"
  (interactive)
  (setq *default-tt-frame* (selected-frame)))

(defun my-server-switch-hook ()
  (interactive)
  (let ((target-window (frame-selected-window *default-tt-frame*))
        (target-buffer (current-buffer)))
    (if (not (eql (selected-frame) *default-tt-frame*))
        (progn (bury-buffer)
               (set-window-buffer target-window target-buffer)
               (select-frame-set-input-focus *default-tt-frame*)))))

(add-hook 'server-switch-hook 'my-server-switch-hook)
