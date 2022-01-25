(setq custom-file "~/.emacs.d/custom-file.el")
(load custom-file)

(defun utils/kill-buffers-with-prefix (prefix match-what kill-modified)
  (mapc (lambda (this-buffer)
          (when (funcall match-what this-buffer)
            (when (string-prefix-p prefix (funcall match-what this-buffer))
              (unless (and (not kill-modified) (buffer-modified-p this-buffer))
                (kill-buffer this-buffer)))))
        (buffer-list)))

(defun utils/read-string-from-file (file-path)
  "Return FILE-PATH's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun utils/rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun setup-packages ()
  (require 'package)

  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (require 'use-package-ensure)
  (setq use-package-always-ensure t)

  (use-package clang-format
    :config
    (global-set-key [C-M-q] 'clang-format-region))

  (use-package exec-path-from-shell
    :if (memq window-system '(mac ns))
    :config (exec-path-from-shell-initialize))

  (use-package flyspell
    :config
    (global-set-key (kbd "C-c C-f") 'flyspell-mode)
    (defun fm ()
      (interactive)
      (flyspell-mode)))

  (use-package git-modes)

  (use-package ido
    :config
    (setq ido-use-filename-at-point nil
	  ido-use-url-at-point nil
	  ido-enable-flex-matching t
	  ido-max-prospects 6
	  ido-confirm-unique-completion t
	  ido-default-file-method 'selected-window
	  ido-default-buffer-method 'selected-window
	  make-backup-files nil)
    (ido-mode t)
    (global-set-key (kbd "C-x C-b") 'ido-switch-buffer))

  (use-package magit
    :config
    (global-set-key (kbd "C-x C-a C-a") 'magit-status))

  (use-package paredit)

  (use-package revbufs
    :load-path "third-party/")

  (use-package tablegen-mode
    :load-path "third-party/")

  (use-package try)

  (use-package zenburn-theme
    :config (load-theme 'zenburn t))

  (use-package google-c-style
    :config (add-hook 'c-mode-common-hook 'google-set-c-style))

  (use-package multiple-cursors
    :config
    (require 'multiple-cursors)
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (global-set-key (kbd "C-\\") 'mc/mark-next-like-this)
    (setq mc/always-run-for-all t))

  (use-package eglot
    :config
    (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
    (add-hook 'c-mode-hook 'eglot-ensure)
    (add-hook 'c++-mode-hook 'eglot-ensure)
    (add-hook 'eglot--managed-mode-hook
	      (lambda ()
		(flymake-mode -1)
		(eldoc-mode -1))))

  (use-package company
    :config
    (add-hook 'eglot--managed-mode-hook
	      (lambda ()
		(company-mode)))))

(setup-packages)

(defun setup-basic-configuration ()
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (global-font-lock-mode t)
  (setq blink-matching-delay .25
        browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "/usr/bin/google-chrome"
        column-number-mode t
        compilation-scroll-output t
        confirm-kill-emacs 'yes-or-no-p
        exec-path (append exec-path '("/usr/local/bin"))
        inhibit-startup-message t
        max-lisp-eval-depth 12000
        recenter-redisplay nil
        ring-bell-function #'ignore
        uniquify-buffer-name-style 'reverse
        vc-follow-symlinks t
        x-select-enable-clipboard t))

(setup-basic-configuration)

(defun setup-font-configuration ()
  (set-frame-font "Monaco-10")
  (when (load "~/.emacs.d/custom-font-configuration.el" t)))

(setup-font-configuration)

(defun setup-builtin-mode-hooks ()
  (add-hook 'text-mode-hook
            (lambda ()
              (setq fill-column 9999999)
              (flyspell-mode)))
  (add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
  (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
  (add-hook 'c-mode-common-hook (lambda () (subword-mode))))

(setup-builtin-mode-hooks)

(defun setup-kill-buffers-by-directory ()
  (defun kill-buffers-by-directory (dir-name)
    (interactive "Gprefix: ")
    (utils/kill-buffers-with-prefix (expand-file-name dir-name)
				    'buffer-file-name
				    nil))

  (global-set-key (kbd "C-c C-k") 'kill-buffers-by-directory))

(setup-kill-buffers-by-directory)

(defun setup-emacsclient ()
  (defvar *default-emacsclient-frame* nil)

  (defun sdf ()
    (interactive)
    (setq *default-emacsclient-frame* (selected-frame)))

  (add-hook 'server-switch-hook
	    (lambda ()
	      (let ((target-window (frame-selected-window *default-emacsclient-frame*))
		    (target-buffer (current-buffer)))
		(if (not (eql (selected-frame) *default-emacsclient-frame*))
		    (progn (bury-buffer)
			   (set-window-buffer target-window target-buffer)
			   (select-frame-set-input-focus *default-emacsclient-frame*))))))

  (sdf)
  (server-start))

(setup-emacsclient)

(defun setup-edit-text ()
  (defun das-edit-text (title)
    (interactive "sTitle: ")
    (let ((file-name
	   (concat "~/Documents/Thoughts/"
		   (if (string-equal "" title) "noname" title)
		   (format-time-string ".%d-%m-%Y-%H-%M")
		   ".rst")))
      (find-file file-name)
      (visual-line-mode)
      (flyspell-mode)))
  (global-set-key (kbd "C-c t") 'das-edit-text))

(setup-edit-text)

(defun setup-keybindings ()
  (if (memq window-system '(mac ns))
      (setq mac-command-modifier 'meta))
  (global-unset-key (kbd "<insert>"))
  (global-set-key (kbd "C-c C-b") 'browse-url-at-point))

(setup-keybindings)

(defun set-window-config ()
  (set-window-fringes nil 0 0)
  (set-frame-position nil 0 -24)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (when (fboundp 'windmove-default-keybindings)
    (windmove-default-keybindings)))

(set-window-config)
(put 'downcase-region 'disabled nil)
