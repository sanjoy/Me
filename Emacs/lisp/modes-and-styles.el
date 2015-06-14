;;; modes-and-styles.el

(add-to-list 'load-path "~/.emacs.d/third-party/git-modes/")
(add-to-list 'load-path "~/.emacs.d/third-party/magit/")

(defun das-initialize-modes ()
  (require 'cc-mode)
  (require 'git-commit-mode)
  (require 'gitconfig-mode)
  (require 'gitignore-mode)
  (require 'haskell-mode)
  (require 'llvm-mode)
  (require 'magit)
  (require 'paredit)
  (require 'rebase-mode)
  (require 'tablegen-mode))

(defun das-initialize-styles ()
  (require 'google-c-style)
  (c-add-style "llvm.org"
               '((fill-column . 80)
                 (c++-indent-level . 2)
                 (c-basic-offset . 2)
                 (indent-tabs-mode . nil)
                 (c-offsets-alist . ((innamespace 0))))))

(defun das-lisp-hook (interactive-p)
  (if (not interactive-p)
      (define-key lisp-mode-map (kbd "RET") 'newline-and-indent))
  (paredit-mode +1)
  (setq indent-tabs-mode nil))

(defun das-initialize-lisp-mode ()
  (add-hook 'lisp-mode-hook (lambda () (das-lisp-hook nil)))
  (add-hook 'emacs-lisp-mode-hook (lambda () (das-lisp-hook nil)))
  (add-hook 'lisp-interaction-mode-hook (lambda () (das-lisp-hook t))))

(defun das-initialize-haskell-mode ()
  (add-hook 'after-init-hook
            (lambda ()
              (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
              (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
              (add-hook 'haskell-mode-hook
                        (lambda ()
                          (interactive)
                          (setq show-trailing-whitespace t)
                          (local-set-key (kbd "C-c C-c") 'comment-region)
                          (local-set-key (kbd "C-c C-u") 'uncomment-region)
                          (column-marker-1 80))))))


(setq +das-c-project-directories+
      (mapcar (lambda (x)
                (cons (concat das-source-directory (car x) "/") (cdr x)))
              '(("v8" . (lambda ()
                          (c-set-style "Google")))
                ("llvm" . (lambda ()
                            (column-marker-1 80)
                            (c-set-style "llvm.org")))
                ("llvm.git" . (lambda ()
                                (column-marker-1 80)
                                (c-set-style "llvm.org")))
                ("llvm.svn" . (lambda ()
                                (column-marker-1 80)
                                (c-set-style "llvm.org")))
                ("gdb" . (lambda ()
                           (c-set-style "gnu")))
                ("gcc" . (lambda ()
                           (c-set-style "gnu"))))))

(global-set-key (kbd "M-<") (lambda () (interactive) (goto-char (point-min))))
(global-set-key (kbd "M->") (lambda () (interactive) (goto-char (point-max))))

(defun das-get-project-style (file-name)
  (assoc-default file-name +das-c-project-directories+ 'string-prefix-p))

(defun das-c-style ()
  (if buffer-file-name
      (let ((style (das-get-project-style (buffer-file-name))))
        (if style (funcall style)))))

(defun das-initialize-tramp-mode ()
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(defun das-c-mode-common-hook ()
  (setq-default
   c-basic-offset 5
   c-macro-prompt-flag t
   c-offsets-alist '((innamespace . 0)))
  (c-toggle-auto-newline -1)
  (show-paren-mode)
  (ggtags-mode)
  (global-set-key (kbd "M-<") (lambda () (interactive) (goto-char (point-min))))
  (global-set-key (kbd "M->") (lambda () (interactive) (goto-char (point-max))))
  (c-set-offset 'inextern-lang 0)
  (define-key c-mode-map (kbd "RET") 'newline-and-indent)
  (define-key c++-mode-map (kbd "RET") 'newline-and-indent)
  (define-key c-mode-map (kbd "TAB") 'indent-for-tab-command)
  (define-key c++-mode-map (kbd "TAB") 'indent-for-tab-command)
  (setq c-backslash-max-column 79)
  (setq show-trailing-whitespace t)
  (c-set-style "Google")
  (das-c-style)
  (setq c-progress-interval 1)
  (subword-mode 1)
  (font-lock-mode t))

(defun das-initialize-c-mode ()
  (add-hook 'c-mode-common-hook 'das-c-mode-common-hook))

(defun das-rust-mode-hook ()
  (show-paren-mode)
  (define-key rust-mode-map (kbd "RET") 'newline-and-indent)
  (setq c-backslash-max-column 79)
  (setq show-trailing-whitespace t)
  (subword-mode 1)
  (font-lock-mode t))

(defun das-initialize-rust-mode ()
  (add-hook 'rust-mode-hook 'das-rust-mode-hook))

(defun das-initialize-ido-mode ()
  (setq ido-use-filename-at-point nil
        ido-use-url-at-point nil
        ido-enable-flex-matching t
        ido-max-prospects 6
        ido-confirm-unique-completion t
        ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window
        make-backup-files nil)
  (ido-mode t))

(defun das-initialize-agda-mode ()
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "~/Library/Haskell/bin/agda-mode locate")))

  (custom-set-variables
   '(agda2-include-dirs (quote ("/Users/sanjoy/Code/agda-stdlib/src" ".")))
   '(agda2-program-name "/Users/sanjoy/Library/Haskell/bin/agda"))

  (custom-set-faces
   '(agda2-highlight-datatype-face ((t (:foreground "light blue"))))
   '(agda2-highlight-function-face ((t (:foreground "light blue"))))
   '(agda2-highlight-module-face ((t (:foreground "light green"))))
   '(agda2-highlight-number-face ((t (:foreground "light green"))))
   '(agda2-highlight-primitive-type-face ((t (:foreground "light blue"))))
   '(agda2-highlight-record-face ((t (:foreground "light blue"))))
   '(agda2-highlight-symbol-face ((t (:foreground "DarkOrange3"))))))

(defun das-initialize-magit-mode ()
  (add-hook 'magit-log-edit-mode-hook 'flyspell-mode))

(defun das-run-cmd-on-file (prefix suffix)
  (let ((whole-file-name (buffer-file-name)))
    (if (not (null whole-file-name))
        (progn
          (shell-command
           (concat prefix " " (file-name-nondirectory whole-file-name) " " suffix))
          t)
      (message "could not %s %s fiie" prefix suffix))))

(defun das-p4-edit ()
  (interactive)
  (if (das-run-cmd-on-file "p4 edit" "")
      (setq buffer-read-only nil)))

(defun das-p4-revert ()
  (interactive)
  (das-run-cmd-on-file "p4 revert" ""))

(defun das-initialize-p4-mode ()
  (global-set-key (kbd "C-c C-g C-e") 'das-p4-edit)
  (global-set-key (kbd "C-c C-g C-r") 'das-p4-revert))

(defun das-initialize-slime ()
  (add-to-list 'load-path "~/.emacs.d/third-party/slime")
  (require 'slime-autoloads)
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))