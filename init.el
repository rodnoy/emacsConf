;; repositories and dependencies configuration

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; hide welcome screen in emacs

(setq inhibit-startup-message t)

;; configure centered text for org mode

(defun kirill/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))


(use-package visual-fill-column
  :hook (org-mode . kirill/org-mode-visual-fill)
  :hook (gfm-mode . kirill/org-mode-visual-fill)
  :hook (markdown-mode . kirill/org-mode-visual-fill))


;; curl -l https://iterm2.com/misc/install_shell_integration_and_utilities.sh | bash

;; set spell correction

(setq ispell-program-name "aspell")
(add-to-list 'exec-path "/usr/local/bin/aspell")

;; forbid to emacs to create back-up files like #file or ~file

(setq make-backup-files nil)

;; display line numbers

(global-display-line-numbers-mode t)

;; set default directory to open when type C-x C-f
(setq default-directory "~/Documents/notes")

;; disable line numbers for some modes
(dolist (mode '(org-mode-hook term-mode-hook shell-mode-hook
                treemacs-mode-hook eshell-mode-hook gfm-mode-hook
                markdown-mode-hook))
                (add-hook mode (lambda () (display-line-numbers-mode
                0))))

;; disable tool bar

(tool-bar-mode -1)          ; disable the toolbar

(menu-bar-mode -1)          ; disable tabbar

;; turn on abbrev mode globally
(setq-default abbrev-mode t)
;; use org package

(use-package org)

;; add markdown mode

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :commands (markdown-mode gfm-mode)
  :config
  (setq markdown-command "pandoc -t html5"))

;; preview

(use-package simple-httpd
  :ensure t
  :config
  (setq httpd-port 7070)
  (setq httpd-host (system-name)))

(use-package impatient-mode
  :ensure t
  :commands impatient-mode)


(defun my-markdown-filter (buffer)
  (princ
   (with-temp-buffer
     (let ((tmp (buffer-name)))
       (set-buffer buffer)
       (set-buffer (markdown tmp))
       (format "<!doctype html><html><title>markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
   (current-buffer)))

(defun my-markdown-preview ()
  "preview markdown."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'my-markdown-filter)
  (imp-visit-buffer))

;(when (memq window-system '(mac ns x))
;  (exec-path-from-shell-initialize))
(defun set-exec-path-from-shell-path ()
  "sets the exec-path to the same value used by the user shell"
  (let ((path-from-shell
         (replace-regexp-in-string
          "[[:space:]\n]*$" ""
          (shell-command-to-string "$shell -l -c 'echo $path'"))))
    (setenv "path" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-path)


 ;; custom-set-variables was added by custom.
 ;; if you edit it by hand, you could mess it up, so be careful.
 ;; your init file should contain only one such instance.
 ;; if there is more than one, they won't work right.



 '(package-selected-packages
   (quote
    (arduino-mode auto-org-md exec-path-from-shell flymd swift3-mode writeroom-mode swift-mode pastelmac-theme org-beautify-theme obsidian-theme objc-font-lock hydandata-light-theme eink-theme bliss-theme aurora-theme ample-theme)))



;;i launch my macros file for each emacs document
(load "~/documents/notes/macros")
;; i launch auto-fill-mode by default to avoid to launch it all the time
;; (setq-default auto-fill-function 'do-auto-fill)
;; use fly spell mode by default
(define-globalized-minor-mode my-global-flyspell-mode flyspell-mode
  (lambda () (flyspell-mode 1)))

(my-global-flyspell-mode 1)
;; use right option button in emacs as usual macos button
;; https://apple.stackexchange.com/questions/12087/emacs-on-mac-os-x-to-alt-or-command

(use-package doom-themes
  :init (load-theme 'doom-palenight t))


(use-package all-the-icons)
;; works on mac
(add-hook 'after-init-hook #'doom-modeline-mode)
(setq doom-modeline-height 25)
;; works on linux
;; (use-package doom-modeline
  ;; :init (doom-modeline-mode 1)
  ;; :custom ((doom-modeline-height 15)))

;; fonts
;; you will most likely need to adjust this font size for your system!
(defvar kirill/default-font-size 160)
(defvar kirill/default-variable-font-size 160)



(set-face-attribute 'default nil :font "fira code" :height kirill/default-font-size)

;; set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "fira code" :height kirill/default-font-size)

;; set the variable pitch face
;; uncomment on mac os
(set-face-attribute 'variable-pitch nil :font "cantarell" :height kirill/default-variable-font-size :weight 'regular)

;; show recent files

(require 'recentf)
(recentf-mode 1)

(setq recentf-auto-cleanup 'never)
;; custom key settings
(global-set-key (kbd "C-C ;") 'comment-line)
;; set-mark-command,,
(global-set-key (kbd "C-c SPC") 'set-mark-command)
;; show recent files
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
;; change how mark-up looks in emacs without modyfing exporting format

(setq org-emphasis-alist '(("*" (bold :foreground "#00b295" )) ("/"
	   (italic :foreground "#bce7fd")) ("_" underline) ("=" (:background "#c492b1"
	   :foreground "black")) ("~" (:background "#dbbadd"
	   :foreground "black")) ("+" (:strike-through t))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(shrink-path doom-modeline all-the-icons doom-themes visual-fill-column use-package markdown-mode impatient-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
