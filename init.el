
;; my repos
;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;              ("marmalade" . "http://marmalade-repo.org/packages/")
;;              ("melpa" . "http://melpa.org/packages/")))


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

;; hide welcome screen in Emacs

(setq inhibit-startup-message t)

;; configure centered text for Org mode

(defun kirill/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))


(use-package visual-fill-column
  :hook (org-mode . kirill/org-mode-visual-fill)
  :hook (gfm-mode . kirill/org-mode-visual-fill))


;; curl -L https://iterm2.com/misc/install_shell_integration_and_utilities.sh | bash

;; set spell correction

(setq ispell-program-name "aspell")
(add-to-list 'exec-path "/usr/local/bin")

;; forbid to emacs to create back-up files like #file or ~file

(setq make-backup-files nil)

;; display line numbers

(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook term-mode-hook shell-mode-hook
                treemacs-mode-hook eshell-mode-hook gfm-mode-hook))
                (add-hook mode (lambda () (display-line-numbers-mode
                0))))

;; disable tool bar

(tool-bar-mode -1)          ; Disable the toolbar

(menu-bar-mode -1)          ; Disable tabbar

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
       (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
   (current-buffer)))

(defun my-markdown-preview ()
  "Preview markdown."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'my-markdown-filter)
  (imp-visit-buffer))

;(when (memq window-system '(mac ns x))
;  (exec-path-from-shell-initialize))
(defun set-exec-path-from-shell-PATH ()
  "Sets the exec-path to the same value used by the user shell"
  (let ((path-from-shell
         (replace-regexp-in-string
          "[[:space:]\n]*$" ""
          (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)


 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.



 '(package-selected-packages
   (quote
    (arduino-mode auto-org-md exec-path-from-shell flymd swift3-mode writeroom-mode swift-mode pastelmac-theme org-beautify-theme obsidian-theme objc-font-lock hydandata-light-theme eink-theme bliss-theme aurora-theme ample-theme)))



;;I launch my macros file for each emacs document
(load "~/Documents/notes/macros")
;; I launch auto-fill-mode by default to avoid to launch it all the time
;; (setq-default auto-fill-function 'do-auto-fill)
;; use fly spell mode by default
(define-globalized-minor-mode my-global-flyspell-mode flyspell-mode
  (lambda () (flyspell-mode 1)))

(my-global-flyspell-mode 1)
;; use right option button in emacs as usual macOS button
;; https://apple.stackexchange.com/questions/12087/emacs-on-mac-os-x-to-alt-or-command
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#1E1C31" "#FF8080" "#95FFA4" "#FFE9AA" "#91DDFF" "#C991E1" "#AAFFE4" "#CBE3E7"])
 '(awesome-tray-mode-line-active-color "#0031a9")
 '(awesome-tray-mode-line-inactive-color "#d7d7d7")
 '(custom-enabled-themes '(doom-challenger-deep))
 '(custom-safe-themes
   '("7a3dde6f39d2e02e8bf6059c7ac1de729600c53e9826ddf2b8a9db56eaaf3fd9" "cae81b048b8bccb7308cdcb4a91e085b3c959401e74a0f125e7c5b173b916bf9" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "5036346b7b232c57f76e8fb72a9c0558174f87760113546d3a9838130f1cdb74" "74ba9ed7161a26bfe04580279b8cad163c00b802f54c574bfa5d924b99daa4b9" "d6603a129c32b716b3d3541fc0b6bfe83d0e07f1954ee64517aa62c9405a3441" "4a8d4375d90a7051115db94ed40e9abb2c0766e80e228ecad60e06b3b397acab" "6c9cbcdfd0e373dc30197c5059f79c25c07035ff5d0cc42aa045614d3919dab4" "3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "188fed85e53a774ae62e09ec95d58bb8f54932b3fd77223101d036e3564f9206" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "a3b6a3708c6692674196266aad1cb19188a6da7b4f961e1369a68f06577afa16" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "e57eec7e0399272aaca7985a5cc94f3a2675db4cd2dbd79a99c72786e489e43c" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "6084dce7da6b7447dcb9f93a981284dc823bab54f801ebf8a8e362a5332d2753" "711efe8b1233f2cf52f338fd7f15ce11c836d0b6240a18fffffc2cbd5bfe61b0" "57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693" default))
 '(fci-rule-color "#858FA5")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-theme-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-theme-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-theme-fringe-yellow))
 '(highlight-tail-colors '(("#aecf90" . 0) ("#c0efff" . 20)))
 '(hl-sexp-background-color "#efebe9")
 '(hl-todo-keyword-faces
   '(("HOLD" . "#70480f")
     ("TODO" . "#721045")
     ("NEXT" . "#5317ac")
     ("THEM" . "#8f0075")
     ("PROG" . "#00538b")
     ("OKAY" . "#30517f")
     ("DONT" . "#315b00")
     ("FAIL" . "#a60000")
     ("BUG" . "#a60000")
     ("DONE" . "#005e00")
     ("NOTE" . "#863927")
     ("KLUDGE" . "#813e00")
     ("HACK" . "#813e00")
     ("TEMP" . "#5f0000")
     ("FIXME" . "#a0132f")
     ("XXX+" . "#972500")
     ("REVIEW" . "#005a5f")
     ("DEPRECATED" . "#201f55")))
 '(ibuffer-deletion-face 'modus-theme-mark-del)
 '(ibuffer-filter-group-name-face 'modus-theme-mark-symbol)
 '(ibuffer-marked-face 'modus-theme-mark-sel)
 '(ibuffer-title-face 'modus-theme-pseudo-header)
 '(jdee-db-active-breakpoint-face-colors (cons "#100E23" "#906CFF"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#100E23" "#95FFA4"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#100E23" "#565575"))
 '(objed-cursor-color "#FF8080")
 '(package-selected-packages
   '(org-tree-slide challenger-deep-theme writeroom-mode mood-one-theme doom-themes afternoon-theme))
 '(pdf-view-midnight-colors (cons "#CBE3E7" "#1E1C31"))
 '(rustic-ansi-faces
   ["#1E1C31" "#FF8080" "#95FFA4" "#FFE9AA" "#91DDFF" "#C991E1" "#AAFFE4" "#CBE3E7"])
 '(vc-annotate-background "#1E1C31")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (list
    (cons 20 "#95FFA4")
    (cons 40 "#b8f7a6")
    (cons 60 "#dbf0a8")
    (cons 80 "#FFE9AA")
    (cons 100 "#ffd799")
    (cons 120 "#ffc488")
    (cons 140 "#FFB378")
    (cons 160 "#eda79b")
    (cons 180 "#db9cbd")
    (cons 200 "#C991E1")
    (cons 220 "#db8bc0")
    (cons 240 "#ed85a0")
    (cons 260 "#FF8080")
    (cons 280 "#d4757d")
    (cons 300 "#aa6a7a")
    (cons 320 "#805f77")
    (cons 340 "#858FA5")
    (cons 360 "#858FA5")))
 '(vc-annotate-very-old-color nil)
 '(xterm-color-names
   ["#000000" "#a60000" "#005e00" "#813e00" "#0031a9" "#721045" "#00538b" "#f0f0f0"])
 '(xterm-color-names-bright
   ["#505050" "#972500" "#315b00" "#70480f" "#2544bb" "#8f0075" "#30517f" "#ffffff"]))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :extend nil :stipple nil :background "#121212" :foreground "#CCEEEE" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 180 :width normal :foundry "default" :family "MonoLisa")))))

;; Fonts
;; You will most likely need to adjust this font size for your system!
(defvar kirill/default-font-size 180)
(defvar kirill/default-variable-font-size 180)

(set-face-attribute 'default nil :font "Fira Code Retina" :height kirill/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height kirill/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height kirill/default-variable-font-size :weight 'regular)

;; (custom-set-faces
;;  '(default ((t (:inherit nil :extend nil :stipple nil :background "#121212" :foreground "#CCEEEE" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 180 :width normal :foundry "default" :family "MonaLisa Regular")))))

;; (custom-set-faces
;;  '(default ((t (:inherit nil :extend nil :stipple nil :background "#121212" :foreground "#CCEEEE" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 180 :width normal :foundry "default" :family "Menlo")))))


;; custom key settings
(global-set-key (kbd "C-c ;") 'comment-line)
;; set-mark-command,,
(global-set-key (kbd "C-c SPC") 'set-mark-command)
(global-set-key (kbd "S-s") 'kill-ring-save)
;; command+shift+;
;; change how mark-up looks in emacs without modyfing exporting format


;; (setq org-emphasis-alist '(("*" (bold :foreground "Orange" )) ("/"
;; 	   italic) ("_" underline) ("=" (:background "maroon"
;; 	   :foreground "white")) ("~" (:background "deep sky blue"
;; 	   :foreground "MidnightBlue")) ("+" (:strike-through t))))



(setq org-emphasis-alist '(("*" (bold :foreground "#00b295" )) ("/"
	   (italic :foreground "#bce7fd")) ("_" underline) ("=" (:background "#c492b1"
	   :foreground "black")) ("~" (:background "#dbbadd"
	   :foreground "black")) ("+" (:strike-through t))))
