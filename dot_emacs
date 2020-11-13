;;; package --- Summary

;;; Commentary:


;;; Code:

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
  (add-to-list 'package-archives (cons "ublt" (concat proto "://elpa.ubolonton.org/packages/")) t)
  (add-to-list 'package-archives (cons "celpa" (concat proto "://celpa.conao3.com/packages/")) t)
  )

(package-initialize)
(package-refresh-contents)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-enabled-themes '(doom-grubbox))
 '(custom-safe-themes
   '("2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" default))
 '(package-selected-packages
   '(chess scad-preview dockerfile-mode heaven-and-hell yaml-mode rustic scad-mode lsp-treemacs flycheck-rust telephone-line yasnippet flycheck-pos-tip flycheck magit lsp-ui all-the-icons doom-themes lsp-mode use-package treemacs-evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (require 'use-package))

(require 'org)

(setq org-file-dir "~/.emacs.d/org")
(mapc #'org-babel-load-file (directory-files org-file-dir t "\\.org$"))
