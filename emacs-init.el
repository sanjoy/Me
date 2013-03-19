(server-start)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/themes")

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require 'cc-mode)
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
(require 'org-install)
(require 'paredit)
(require 'php-mode)
(require 'rebase-mode)
(require 'revbufs)
(require 'slime)
(require 'tablegen-mode)
(require 'tramp)
(require 'uniquify)
(require 'whitespace)

(load-file "/home/sanjoy/.emacs.d/ProofGeneral/generic/proof-site.el")

;;   General
;; -------------------------------------------------------------------

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

(global-set-key (kbd "C-c C-f") 'flyspell-mode)
(global-set-key (kbd "C-c C-b") 'browse-url-at-point)

(global-set-key (kbd "C-c r")
                'revert-buffer)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;;; Auto-encryption / decryption
(epa-file-enable)

;;; I'm old enough to use downcase-region
(put 'downcase-region 'disabled nil)


(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_FULLSCREEN" 0)))


(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(defun my-find-selected ()
  (if (use-region-p)
      (buffer-substring (region-beginning) (region-end))
    (thing-at-point 'symbol)))

(defun my-grep-find (term)
  (interactive
   (list (read-string "Search for: " (my-find-selected))))
  (grep-find (concat "find . -type f -print0 | \"xargs\" -0 -e grep -I -nH -e "
                     term)))

(global-set-key (kbd "C-c s") 'my-grep-find)

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

(global-set-key (kbd "\C-x\C-a\C-a") 'magit-status)

;;   My color scheme
;; -------------------------------------------------------------------

(defun color-theme-ultra-mild (&optional preview)
  (interactive)
  (color-theme-install
   '(color-theme-ultra-mild
     ((foreground-color . "#aaaaaa")
      (background-color . "#000000")
      (mouse-color . "black")
      (cursor-color . "medium turquoise")
      (border-color . "black")
      (background-mode . dark))

     (default ((t (nil))))

     (modeline ((t (:foreground "white" :background "gray15"))))
     (modeline-buffer-id ((t (:foreground "white" :background "gray15"))))
     (modeline-mousable ((t (:foreground "white" :background "gray15"))))
     (modeline-mousable-minor-mode ((t (:foreground "white" :background "gray15"))))

     (highlight ((t (:foreground "white" :background "gray15"))))

     (bold ((t (:bold t))))
     (italic ((t (:italic t))))
     (bold-italic ((t (:bold t :italic t))))
     (underline ((t (:underline t))))
     (fixed ((t (:bold t))))
     (excerpt ((t (:italic t))))

     (region ((t (:foreground "white" :background "#004060"))))
     (zmacs-region ((t (:foreground "white" :background "#004060"))))
     (secondary-selection ((t (:background "paleturquoise"))))

     (font-lock-builtin-face ((t (:foreground "#9999d6"))))
     (font-lock-keyword-face ((t (:foreground "#9999d6"))))

     (font-lock-comment-face ((t (:foreground "#995500"))))
     (font-lock-constant-face ((t (:foreground "#aaaaaa"))))
     (font-lock-function-name-face ((t (:foreground "#aaaaaa"))))
     (font-lock-type-face ((t (:foreground "#aaaaaa"))))
     (font-lock-variable-name-face ((t (:foreground "#aaaaaa"))))

     (font-lock-preprocessor-face ((t (:foreground "#d1b2c2"))))
     (font-lock-string-face ((t (:foreground "#b87094" :background "#000a1f"))))
     (font-lock-warning-face ((t (:foreground "red" :bold t))))

     (highlight-changes-face ((t (:foreground "red"))))
     (highlight-changes-delete-face ((t (:foreground "red" :underline t))))

     (makefile-space-face ((t (:background "#8a002e"))))

     (flyspell-incorrect-face ((t (:foreground "#b84d4d" :bold t :underline t))))
     (flyspell-duplicate-face ((t (:foreground "#b84d4d" :bold t :underline t)))))))

(color-theme-initialize)
(color-theme-ultra-mild)

;;   Lisp Mode
;; -------------------------------------------------------------------

(defun my-lisp-hook (interactive-p)
  (if (not interactive-p)
      (define-key lisp-mode-map (kbd "RET") 'newline-and-indent))
  (paredit-mode +1)
  (setq indent-tabs-mode nil))

(add-hook 'lisp-mode-hook '(lambda () (my-lisp-hook nil)))
(add-hook 'emacs-lisp-mode-hook '(lambda () (my-lisp-hook nil)))
(add-hook 'lisp-interaction-mode-hook '(lambda () (my-lisp-hook t)))

(setq inferior-lisp-program "/usr/bin/sbcl --noinform --no-linedit")
(add-to-list 'load-path (concat code-directory "/Practice/LISP/SLIME"))
(slime-setup)


;;   Org Mode
;; -------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;;; Windmove under Org mode
(add-hook 'org-shiftup-final-hook    'windmove-up)
(add-hook 'org-shiftleft-final-hook  'windmove-left)
(add-hook 'org-shiftdown-final-hook  'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)


;;   Ido buffer & file management
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
 ido-confirm-unique-completion t
;;;; Always open buffers and files in the current window
 ido-default-file-method 'selected-window
 ido-default-buffer-method 'selected-window)

;;; No backup files
(setq make-backup-files nil)



;;   Haskell Mode
;; -------------------------------------------------------------------

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook '(lambda ()
                                (interactive)
                                (setq show-trailing-whitespace t)))


;;   C & C++ Mode
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
                ("llvm" . (lambda ()
                            (c-set-style "llvm.org")))
                ("gdb" . (lambda ()
                           (c-set-style "gnu")))
                ("gcc" . (lambda ()
                           (c-set-style "gnu")))
                ("jato" . (lambda ()
                            (setq tab-width 2)))
                ("phoenixfs" . (lambda ()
                                 (c-set-style "linux"))))))

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
  (setq show-trailing-whitespace t)
  (c-set-style "Google")
  (my-c-style)
  (setq c-progress-interval 1)
  (subword-mode 1)
  (font-lock-mode t))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(global-set-key (kbd "C-c i") 'imenu)

;;; For M-x compile
(setq compilation-scroll-output t)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(agda2-highlight-datatype-face ((t (:foreground "light blue"))))
 '(agda2-highlight-function-face ((t (:foreground "light blue"))))
 '(agda2-highlight-module-face ((t (:foreground "cyan"))))
 '(agda2-highlight-postulate-face ((t (:foreground "light blue"))))
 '(agda2-highlight-primitive-face ((t (:foreground "light blue"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "light blue"))))
 '(agda2-highlight-record-face ((t (:foreground "light blue"))))
 '(which-func ((t (:foreground "green")))))

;;   Writeups
;; -------------------------------------------------------------------

(setq +thoughts-directory+ "~/rest/writeups/")

(defun write-mode ()
  (interactive)
  (longlines-mode)
  (flyspell-mode))

(defun my-edit-text (title)
  (interactive "sTitle: ")
  (let ((file-name (concat +thoughts-directory+ title "."
                           (format-time-string "%d-%m-%Y-%H-%M")
                           ".rst")))
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


;;   Frames
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
(put 'upcase-region 'disabled nil)

(my-set-default-frame)

;; TeX Mode
;; -------------------------------------------------------------------

(defun tag-word-or-region (tag)
  (interactive)
  (let (pos1 pos2 bds)
    (if (and transient-mark-mode mark-active)
        (progn
          (goto-char (region-end))
          (insert "}")
          (goto-char (region-beginning))
          (insert (concat "\\" tag "{")))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (if (null bds)
            (progn
              (insert (concat "\\" tag "{}"))
              (backward-char))
          (progn
            (goto-char (cdr bds))
            (insert "}")
            (goto-char (car bds))
            (insert (concat "\\" tag "{"))))))))

(defun tex-mode-hook ()
  (interactive)
  (local-set-key (kbd "C-c C-l C-i")
                 (lambda ()
                   (interactive)
                   (tag-word-or-region "textit")))
  (local-set-key (kbd "C-c C-l C-c")
                 (lambda ()
                   (interactive)
                   (tag-word-or-region "texttt"))))

(add-hook 'TeX-mode-hook 'tex-mode-hook)


(setq x-select-enable-clipboard t)
(custom-set-variables
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify)))
