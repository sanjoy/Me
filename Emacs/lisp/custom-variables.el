(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-include-dirs (quote ("." "/Users/sanjoy/Code/agda-stdlib/src/")))
 '(git-commit-finish-query-functions nil)
 '(package-selected-packages
   (quote
    (writegood-mode slime ctags-update ctags zenburn-theme zenburn string-inflection seq scala-mode sass-mode rtags php-mode paredit magit llvm-mode levenshtein haskell-mode google-this google-c-style gitignore-mode gitconfig-mode framemove exec-path-from-shell column-marker color-theme)))
 '(safe-local-variable-values
   (quote
    ((coq-prog-args "-emacs-U")
     (eval flet
           ((pre
             (s)
             (concat
              (locate-dominating-file buffer-file-name ".dir-locals.el")
              s)))
           (setq coq-load-path
                 (\`
                  ((rec
                    (\,
                     (pre "lib/sflib"))
                    "sflib")
                   (rec
                    (\,
                     (pre "lib/paco/src"))
                    "Paco")
                   (rec
                    (\,
                     (pre "lib/hahn"))
                    "cmem")
                   (rec
                    (\,
                     (pre "src/axiomatic"))
                    "cmem")
                   (rec
                    (\,
                     (pre "src/lib"))
                    "cmem")
                   (rec
                    (\,
                     (pre "src/opt"))
                    "cmem")
                   (rec
                    (\,
                     (pre "src/prop"))
                    "cmem")
                   (rec
                    (\,
                     (pre "src/lang"))
                    "cmem")
                   (rec
                    (\,
                     (pre "src/drf"))
                    "cmem")
                   (rec
                    (\,
                     (pre "src/hahn"))
                    "cmem")
                   (rec
                    (\,
                     (pre "src/while"))
                    "cmem"))))))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587)
 '(tramp-inline-compress-start-size 83886080))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-datatype-face ((t (:foreground "chartreuse4"))))
 '(agda2-highlight-function-face ((t (:foreground "cyan"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "light blue"))))
 '(agda2-highlight-record-face ((t (:foreground "VioletRed2")))))
