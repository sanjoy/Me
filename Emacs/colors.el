;;; colors.el

(defun sanjoy-color-theme-ultra-mild (&optional preview)
  (interactive)
  (color-theme-install
   '(sanjoy-color-theme-ultra-mild
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

(defun sanjoy-install-color-theme ()
  (color-theme-initialize)
  (sanjoy-color-theme-ultra-mild))
