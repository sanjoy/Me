;;; modes-and-styles.el

(add-to-list 'load-path "~/.emacs.d/third-party/git-modes/")
(add-to-list 'load-path "~/.emacs.d/third-party/magit/")

(defun sanjoy-initialize-modes ()
  (require 'cc-mode)
  (require 'git-commit-mode)
  (require 'gitconfig-mode)
  (require 'gitignore-mode)
  (require 'llvm-mode)
  (require 'magit)
  (require 'paredit)
  (require 'rebase-mode)
  (require 'tablegen-mode)
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate"))))

(defun sanjoy-initialize-styles ()
  (require 'google-c-style)
  (c-add-style "llvm.org"
               '((fill-column . 80)
                 (c++-indent-level . 2)
                 (c-basic-offset . 2)
                 (indent-tabs-mode . nil)
                 (c-offsets-alist . ((innamespace 0))))))

(defun sanjoy-lisp-hook (interactive-p)
  (if (not interactive-p)
      (define-key lisp-mode-map (kbd "RET") 'newline-and-indent))
  (paredit-mode +1)
  (setq indent-tabs-mode nil))

(defun sanjoy-initialize-lisp-mode ()
  (add-hook 'lisp-mode-hook '(lambda () (sanjoy-lisp-hook nil)))
  (add-hook 'emacs-lisp-mode-hook '(lambda () (sanjoy-lisp-hook nil)))
  (add-hook 'lisp-interaction-mode-hook '(lambda () (sanjoy-lisp-hook t))))

(defun sanjoy-initialize-haskell-mode ()
  (add-hook 'after-init-hook
            (lambda ()
              (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
              (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
              (add-hook 'haskell-mode-hook
                        '(lambda ()
                           (interactive)
                           (setq show-trailing-whitespace t)
                           (local-set-key (kbd "C-c C-c") 'comment-region)
                           (local-set-key (kbd "C-c C-u") 'uncomment-region))))))


(setq +sanjoy-c-project-directories+
      (mapcar (lambda (x)
                (cons (concat sanjoy-source-directory (car x) "/") (cdr x)))
              '(("v8" . (lambda ()
                          (c-set-style "Google")))
                ("llvm" . (lambda ()
                            (c-set-style "llvm.org")))
                ("gdb" . (lambda ()
                           (c-set-style "gnu")))
                ("gcc" . (lambda ()
                           (c-set-style "gnu")))
                ("phoenixfs" . (lambda ()
                                 (c-set-style "linux"))))))

(defun sanjoy-get-project-style (file-name)
  (assoc-default file-name +sanjoy-c-project-directories+ 'string-prefix-p))

(defun sanjoy-c-style ()
  (if buffer-file-name
      (let ((style (sanjoy-get-project-style (buffer-file-name))))
        (if style (funcall style)))))

(defun sanjoy-c-mode-common-hook ()
  (setq-default
   c-basic-offset 5
   c-macro-prompt-flag t
   c-offsets-alist '((innamespace . 0)))
  (c-toggle-auto-newline -1)
  (c-set-offset 'inextern-lang 0)
  (column-marker-1 80)
  (define-key c-mode-map (kbd "RET") 'newline-and-indent)
  (define-key c++-mode-map (kbd "RET") 'newline-and-indent)
  (setq c-backslash-max-column 79)
  (setq show-trailing-whitespace t)
  (c-set-style "Google")
  (sanjoy-c-style)
  (setq c-progress-interval 1)
  (subword-mode 1)
  (font-lock-mode t))

(defun sanjoy-initialize-c-mode ()
  (add-hook 'c-mode-common-hook 'sanjoy-c-mode-common-hook))

(defun sanjoy-tag-word-or-region (tag)
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
                   (sanjoy-tag-word-or-region "textit")))
  (local-set-key (kbd "C-c C-l C-c")
                 (lambda ()
                   (interactive)
                   (sanjoy-tag-word-or-region "texttt"))))

(defun sanjoy-initialize-tex-mode ()
  (add-hook 'TeX-mode-hook 'tex-mode-hook))

(defun sanjoy-initialize-ido-mode ()
  (setq ido-use-filename-at-point nil
        ido-use-url-at-point nil
        ido-enable-flex-matching t
        ido-max-prospects 6
        ido-confirm-unique-completion t
        ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window
        make-backup-files nil)
  (ido-mode t))

(defun sanjoy-initialize-agda-mode ()
  (custom-set-variables '(agda2-highlight-face-groups 'conor)))
