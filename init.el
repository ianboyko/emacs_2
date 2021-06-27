(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'window-setup-hook 'toggle-frame-maximized t) ; full screen on start

(setq initial-scratch-message nil)

;;Save backup files elsewhere ***NOT WORKING***
;;(setq auto-save-file-name-transforms
;;      `((".*" ,(concat user-emacs-directory "auto-save/") t))) 

(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
;;(menu-bar-mode -1)            ; Disable the menu bar
(scroll-bar-mode -1)      ; Disable visible scrollbar 
(transient-mark-mode 1)

;;DISABLE BELL FUNCTION
(setq ring-bell-function 'ignore)

;; Org-mode stop indentation
;; disable indentation
(setq org-adapt-indentation nil)

;;UNFILL DAMNIT!
(defun unfill-region (begin end)
  "Remove all linebreaks in a region but leave paragraphs, 
  indented text (quotes,code) and lines starting with an asterix (lists) intakt."
  (interactive "r")
  (replace-regexp "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" nil begin end))

;;MORE UNFILL
;Stefan Monnier <foo at acm.org>. It is the opposite of 
;fill-paragraph. Takes a multi-line paragraph and makes 
;it into a single line of text.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes '(deeper-blue))
 '(custom-safe-themes t)
 '(global-display-line-numbers-mode t)
 '(inhibit-startup-screen t)
 '(org-agenda-files '("~/Sync/org/personal/inbox.org"))
 '(org-archive-location "~/Sync/org/personal/archive.org::* From %s")
 '(org-bullets-bullet-list '("◉" "○" ">" "-"))
 '(org-export-backends '(ascii html icalendar latex md odt org))
 '(org-hide-emphasis-markers t)
 '(org-support-shift-select 'always)
 '(package-selected-packages
   '(pabbrev org-roam helpful ox-epub nov org-superstar org-bullets hippie-expand-slime mu4e-overview ox-hugo ham-mode hackernews emmet-mode markdown-mode yasnippet org-edna))
 '(split-window-horizontally t)
 '(word-wrap t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#181a26" :foreground "gray80" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 155 :width normal :foundry "GOOG" :family "Noto Sans"))))
 '(helm-selection ((t (:extend t :background "khaki1" :distant-foreground "black"))))
 '(line-number ((t (:inherit (shadow default) :foreground "dim gray"))))
 '(linum ((t (:inherit (shadow default) :foreground "DodgerBlue4" :foundry "GOOG" :family ""))))
 '(mode-line ((t (:background "gray12" :foreground "gray" :box (:line-width 1 :style released-button)))))
 '(mode-line-buffer-id ((t (:foreground "gray" :weight bold))))
 '(org-code ((t (:inherit shadow :family "Andale Mono"))))
 '(org-level-1 ((t (:inherit outline-1 :extend nil :weight bold :height 1.5))))
 '(org-level-2 ((t (:inherit outline-2 :extend nil :weight normal :height 1.35))))
 '(org-level-3 ((t (:inherit outline-3 :extend nil :weight normal :height 1.1)))))

(global-visual-line-mode t)

;;emacs post
(load "~/.emacs.d/post")
(require 'post)

;;new keybind for C-c C-t
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key [(meta shift up)]  'move-line-up)
(global-set-key [(meta shift down)]  'move-line-down)

;; new keybind for <f12>
(global-set-key [(f12)] 'flyspell-correct-word-before-point)

;;new keybind for C-;
(global-set-key (kbd "C-;") #'other-window)
(global-set-key (kbd "C-,") #'prev-window)

(defun prev-window ()
  (interactive)
  (other-window -1))

;; Replace list hyphen with dot
 (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Epub shite that doesn't work 
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;;pabbrev mode
(require 'pabbrev)
(defun my-org-mode-settings ()
   (pabbrev-mode))
(add-hook 'org-mode-hook 'my-org-mode-settings)
(global-pabbrev-mode)
;(global-smart-tab-mode 1) ;;I get an error

(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "CANCELLED" "DONE")))

;; Org mode new item doesn't split header
(setq org-M-RET-may-split-line '((item . nil)))

;; Org mode target for capture
(setq org-default-notes-file (concat org-directory "~/Sync/org/personal/inbox.org"))

;; Org mode sample capture template
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Sync/org/personal/inbox.org" "Inbox")
         "* TODO %?\n  %i\n  %a")))
