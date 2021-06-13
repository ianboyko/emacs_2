(deftheme Boyko Theme 20210521
  "Created 2021-05-21.")

(custom-theme-set-variables
 'Boyko Theme 20210521
 '(cua-mode t)
 '(inhibit-startup-screen t)
 '(package-selected-packages '(ox-hugo ham-mode hackernews emmet-mode markdown-mode yasnippet org-edna))
 '(display-time-mode t)
 '(global-display-line-numbers-mode t))

(custom-theme-set-faces
 'Boyko Theme 20210521
 '(default ((t (:family "Noto Sans" :foundry "GOOG" :slant normal :weight normal :height 143 :width normal))))
 '(line-number ((t (:inherit (shadow default) :foreground "dim gray")))))

(provide-theme 'Boyko Theme 20210521)
