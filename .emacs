(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)
(package-refresh-contents)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-treemacs flycheck-rust telephone-line yasnippet flycheck-pos-tip flycheck magit lsp-ui all-the-icons doom-themes lsp-mode use-package treemacs-evil rust-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (require 'use-package))

(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package treemacs
  :ensure t)
(use-package treemacs-evil
  :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
(use-package flycheck-pos-tip
  :ensure t)

(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backends))

;(use-package company-box
;  :hook (company-mode . company-box-mode))

(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t))

(use-package elcord
  :ensure t
  :config
  (elcord-mode))

(use-package magit
  :ensure t)

(use-package lsp-ui
  :ensure t)

(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-rust-server 'rust-analyzer)
  ;(setq lsp-rust-server 'rls)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (add-hook 'rust-mode-hook 'lsp))

(use-package lsp-treemacs
  :ensure t
  :config
  (lsp-treemacs-sync-mode 1))

(use-package all-the-icons
  :ensure t)

(use-package telephone-line
  :ensure t
  :config
  (telephone-line-mode 1))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;(doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(global-set-key (kbd "M-RET t") 'treemacs-select-window)
(global-set-key (kbd "M-RET s") 'treemacs-switch-workspace)
(global-set-key (kbd "M-RET w") 'ace-window)
(global-set-key (kbd "M-RET c") 'rust-compile)
(global-set-key (kbd "M-RET r") 'rust-run)
(global-set-key (kbd "M-RET e") 'lsp-execute-code-action)
(global-set-key (kbd "<f11>") 'fullscreen)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; show matching parenthesis
(show-paren-mode 1)

;; optimizations for emacs-lsp
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; enable ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; disable gtk stuff
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))  ;; no toolbar
(menu-bar-mode -1) ;;no menubar
(scroll-bar-mode -1) ;; no scroll bar

;; set font
(set-frame-font "Droid Sans Mono 12" nil t)

;; enable parenthesis and quotes pairing thing
(electric-pair-mode)

;; fullscreen
(defun fullscreen ()
 (interactive)
 (set-frame-parameter nil 'fullscreen
   (if (frame-parameter nil 'fullscreen) nil 'fullboth)))


;(add-to-list 'default-frame-alist '(fullscreen . maximized))
