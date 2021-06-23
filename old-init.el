(add-to-list 'default-frame-alist '(fullscreen . maximixed))

;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.6)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
;;    (set-face-attribute (car face) nil :font "Calibri" :weight 'regular :height (cdr face)))

(use-package org-bullets
	     :after org
	     :hook (org-mode . org-bullets-mode)
	     :custom
	     (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org
	     :config
	     (setq org-ellipsis " ▾"
		   org-hide-emphasis-markers t))

  (defun org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
	     :hook (org-mode . org-mode-visual-fill))

;; testing git ssh
