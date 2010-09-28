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
(require 'color-theme-subdued)
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
	      fill-adapt-mode t)

;; I don't need the list of buffers.

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

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

(color-theme-subdued)

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
(setq rcirc-server-alist '(("irc.freenode.net" :nick "sanjoyd" :full-name "Sanjoy Das")
                           ("irc.gimp.org"     :nick "sanjoyd" :full-name "Sanjoy Das")))

(defun irc ()
  (interactive)
  (rcirc-connect "irc.freenode.net" "6667" "sanjoyd")
  (rcirc-connect "irc.gimp.org"     "6667" "sanjoyd"))

(add-hook 'rcirc-mode-hook
		  '(lambda ()
			 (flyspell-mode)))

; Logging
(setq rcirc-log-flag "t"
      rcirc-log-directory "~/.emacs.d/rcirc-log")

(setq rcirc-authinfo
	  '(("freenode" nickserv "sanjoyd" "yojnas")))


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
 ido-case-fold  t                   ; be case-insensitive
 ido-use-filename-at-point nil      ; don't use filename at point (annoying)
 ido-use-url-at-point nil           ; don't use url at point (annoying)
 ido-enable-flex-matching t         ; be flexible
 ido-max-prospects 6                ; don't spam my minibuffer
 ido-confirm-unique-completion nil) ; don't wait for RET with unique completion


;; Auto-completion

(global-auto-complete-mode t)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; Easy bookmarks

(defvar af-current-bookmark nil)

(defun af-bookmark-make-name ()
  "Makes a bookmark name from the buffer name and cursor position"
  (concat (buffer-name (current-buffer))
          "/" (number-to-string (point))))

(defun af-bookmark-toggle ()
  "Removes a bookmark if it exists, create one if it doesnt exist"
  (interactive)
  (let ((bm-name (af-bookmark-make-name)))
    (if (bookmark-get-bookmark bm-name t)
        (progn (bookmark-delete bm-name)
               (message "bookmark removed"))
      (progn (bookmark-set bm-name)
             (setf af-current-bookmark bm-name)
             (message "bookmark set")))))

(defun af-bookmark-cycle (i)
  "Cycle through bookmarks by i.  'i' should be 1 or -1"
  (if bookmark-alist
      (progn (unless af-current-bookmark
               (setf af-current-bookmark (first (first bookmark-alist))))
             (let ((cur-bm (assoc af-current-bookmark bookmark-alist)))
               (setf af-current-bookmark
                     (if cur-bm
                         (first (nth (mod (+ i (position cur-bm bookmark-alist))
                                          (length bookmark-alist))
                                     bookmark-alist))
                       (first (first bookmark-alist))))
               (bookmark-jump af-current-bookmark)
               ;; Update the position and name of the bookmark.  We
               ;; only need to do this when the bookmark has changed
               ;; position, but lets go ahead and do it all the time
               ;; anyway.
               (bookmark-set-position af-current-bookmark (point))
               (let ((new-name (af-bookmark-make-name)))
                 (bookmark-set-name af-current-bookmark new-name)
                 (setf af-current-bookmark new-name))))
    (message "There are no bookmarks set!")))

(defun af-bookmark-cycle-forward ()
  "find the next bookmark in the bookmark-alist"
  (interactive)
  (af-bookmark-cycle 1))

(defun af-bookmark-cycle-reverse ()
  "find the next bookmark in the bookmark-alist"
  (interactive)
  (af-bookmark-cycle -1))

(defun af-bookmark-clear-all()
  "clears all bookmarks"
  (interactive)
  (setf bookmark-alist nil))

(global-set-key [(control f2)]  'af-bookmark-toggle)
(global-set-key [f2]  'af-bookmark-cycle-forward)
(global-set-key [(shift f2)]  'af-bookmark-cycle-reverse)
(global-set-key [(control shift f2)]  'af-bookmark-clear-all)

(add-hook 'weblogger-start-edit-entry-hook
		  (lambda()
			(flyspell-mode 1)
			(flyspell-buffer)   ; spell check the fetched post
			(auto-fill-mode -1)
			(visual-line-mode 1)))

(add-hook 'weblogger-publish-entry-hook
		  (lambda()
			(when visual-line-mode
			  (visual-line-mode -1))
			;; tabs might spoil code indentation
			(untabify (point-min) (point-max))))

(add-hook 'weblogger-publish-entry-end-hook
		  (lambda()
			(visual-line-mode 1)))

(setq
  weblogger-config-alist '(("default" "http://www.playingwithpointers.com/xmlrpc.php" "admin" "yojnassanjoy" "1")))

;; Set up w3m-el

(setq browse-url-browser-function 'w3m-browse-url)

(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

(global-set-key "\C-xm" 'browse-url-at-point)
(setq w3m-use-cookies t)

;; No backup files

(setq make-backup-files nil)

;; Save the annoying C-x o's
(windmove-default-keybindings)
