;;; package --- Summary

;;; Commentary:


;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

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
   '(evil-org org-roam chess scad-preview dockerfile-mode heaven-and-hell yaml-mode rustic scad-mode lsp-treemacs flycheck-rust telephone-line yasnippet flycheck-pos-tip flycheck magit lsp-ui all-the-icons doom-themes lsp-mode use-package treemacs-evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq vc-follow-symlinks t)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(straight-use-package 'use-package)

(use-package org
  :straight t)

(setq org-file-dir "~/.emacs.d/org")
(mapc #'delete-file (directory-files org-file-dir t "\\.el$"))
(mapc #'org-babel-load-file (directory-files org-file-dir t "\\.org$"))
