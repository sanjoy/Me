;;; modes-and-styles.el

(defun das-initialize-modes ()
  (require 'cc-mode)
  (require 'gitconfig-mode)
  (require 'gitignore-mode)
  (require 'haskell-mode)
  (require 'llvm-mode)
  (require 'rtags)
  ;(load "/usr/local/Cellar/proof-general/4.4/share/emacs/site-lisp/proof-general/generic/proof-site.el")
  (setq magit-last-seen-setup-instructions "1.4.0")
  (require 'magit)
  (require 'paredit)
  (require 'rcirc)
  (require 'tablegen-mode))

(defun das-initialize-styles ()
  (require 'google-c-style)
  (load "/usr/local/share/clang/clang-format.el")
  (global-set-key [C-M-tab] 'clang-format-region)
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

(defun das-text-mode-hook ()
  (setq fill-column 9999999)
  (flyspell-mode))

(defun das-initialize-text-mode ()
  (add-hook 'text-mode-hook (lambda () (das-text-mode-hook))))

(defun das-initialize-lisp-mode ()
  (add-hook 'lisp-mode-hook (lambda () (das-lisp-hook nil)))
  (add-hook 'emacs-lisp-mode-hook (lambda () (das-lisp-hook nil)))
  (add-hook 'lisp-interaction-mode-hook (lambda () (das-lisp-hook t))))

(defun das-initialize-haskell-mode ()
  (add-hook 'after-init-hook
            (lambda ()
              (add-hook 'haskell-mode-hook 'haskell-doc-mode)
              (add-hook 'haskell-mode-hook 'haskell-indent-mode)
              (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
              (add-hook 'haskell-mode-hook
                        (lambda ()
                          (interactive)
                          (setq show-trailing-whitespace t)
                          (local-set-key (kbd "C-c C-c") 'comment-region)
                          (local-set-key (kbd "C-c C-u") 'uncomment-region))))))


(setq +das-c-project-directories+
      (mapcar (lambda (x)
                (cons (concat das-source-directory (car x) "/") (cdr x)))
              '(("llvm" . (lambda ()
                            (column-marker-1 80)
                            (c-set-style "llvm.org")))
                ("clang" . (lambda ()
                            (column-marker-1 80)
                            (c-set-style "llvm.org")))
                ("sdmalloc" . (lambda ()
                                (column-marker-1 80)
                                (c-set-style "llvm.org")))
                ("ray" . (lambda ()
                                (column-marker-1 80)
                                (c-set-style "llvm.org")))
                ("llvm.git" . (lambda ()
                                (column-marker-1 80)
                                (c-set-style "llvm.org")))
                ("gdb" . (lambda ()
                           (c-set-style "gnu")))
                ("gcc" . (lambda ()
                           (c-set-style "gnu"))))))

(defun das-get-project-style (file-name)
  (assoc-default file-name +das-c-project-directories+ 'string-prefix-p))

(defun das-c-style ()
  (if buffer-file-name
      (let ((style (das-get-project-style (buffer-file-name))))
        (if style (funcall style)))))

(defun das-initialize-tramp-mode ()
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path "/home/sanjoy/prefix/bin/"))

(defun das-c-mode-common-hook ()
  (setq-default
   c-basic-offset 5
   c-macro-prompt-flag t
   c-offsets-alist '((innamespace . 0)))
  (rtags-enable-standard-keybindings)
  (c-toggle-auto-newline -1)
  (show-paren-mode)
  (c-set-offset 'inextern-lang 0)
  (define-key c-mode-map (kbd "RET") 'newline-and-indent)
  (define-key c++-mode-map (kbd "RET") 'newline-and-indent)
  (define-key c-mode-map (kbd "TAB") 'indent-for-tab-command)
  (define-key c++-mode-map (kbd "TAB") 'indent-for-tab-command)
  (setq c-backslash-max-column 79)
  (setq show-trailing-whitespace t)
  (setq c-progress-interval 1)
  (subword-mode 1)
  (font-lock-mode t))

(defun das-initialize-c-mode ()
  (add-hook 'c-mode-common-hook 'das-c-mode-common-hook))

(defun das-rust-mode-hook ()
  (show-paren-mode)
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

(defun das-initialize-agda-mode ()
  ;; (load-file (let ((coding-system-for-read 'utf-8))
  ;;              (shell-command-to-string "agda-mode locate")))
  )

(defun das-get-string-from-file (file-path)
  "Return file-path's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun rcirc-detach-buffer ()
  (interactive)
  (let ((buffer (current-buffer)))
    (when (and (rcirc-buffer-process)
           (eq (process-status (rcirc-buffer-process)) 'open))
      (with-rcirc-server-buffer
    (setq rcirc-buffer-alist
          (rassq-delete-all buffer rcirc-buffer-alist)))
      (rcirc-update-short-buffer-names)
      (if (rcirc-channel-p rcirc-target)
      (rcirc-send-string (rcirc-buffer-process)
                 (concat "DETACH " rcirc-target))))
    (setq rcirc-target nil)
    (kill-buffer buffer)))

(defun connect-oftc ()
  (interactive)
  (let ((rcirc-pwd (das-get-string-from-file "~/.rcirc-authinfo"))
        (rcirc-url (das-get-string-from-file "~/.rcirc-url")))
    (rcirc-connect
     (concat "alpha." rcirc-url) 6697
     rcirc-default-nick rcirc-default-user-name "Sanjoy Das"
     '("#llvm" "#llvm-build")
     (concat "sanjoy/oftc:" rcirc-pwd)
     'tls)))

(defun connect-freenode ()
  (interactive)
  (let ((rcirc-pwd (das-get-string-from-file "~/.rcirc-authinfo"))
        (rcirc-url (das-get-string-from-file "~/.rcirc-url")))
    (rcirc-connect
     (concat "bravo." rcirc-url) 6697
     rcirc-default-nick rcirc-default-user-name "Sanjoy Das"
     '("#fuchsia" "#phabricator" "#redprl")
     (concat "sanjoy/freenode:" rcirc-pwd)
     'tls)))

(defun connect-klug ()
  (interactive)
  (let ((rcirc-pwd (das-get-string-from-file "~/.rcirc-authinfo"))
        (rcirc-url (das-get-string-from-file "~/.rcirc-url")))
    (rcirc-connect
     (concat "charlie." rcirc-url) 6697
     rcirc-default-nick rcirc-default-user-name "Sanjoy Das"
     '("#general" "#random")
     (concat "sanjoy/klug:" rcirc-pwd)
     'tls)))

(defun das-initialize-rcirc-mode ()
  (define-key rcirc-mode-map [(control c) (control d)] 'rcirc-detach-buffer))

(defun das-format-reply-block ()
  (interactive)
  (if (use-region-p)
      (let ((begin (region-beginning))
            (end (region-end)))
        (progn
          (replace-regexp "^> " "" nil begin end)
          (goto-char begin) (insert "\n")
          (fill-region begin (- end 2))
          (replace-regexp "^" "> " nil (+ begin 1) (- end 2))))
    (error "Please select a region!")))

(global-set-key (kbd "C-c C-g C-i") 'das-format-reply-block)
