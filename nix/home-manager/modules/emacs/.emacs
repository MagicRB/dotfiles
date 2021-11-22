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
 '(custom-enabled-themes '())
 '(custom-safe-themes
   '("2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" default))
 '(package-selected-packages
   '()))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq vc-follow-symlinks t)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/org-task-dump")

(straight-use-package 'use-package)
(straight-thaw-versions)

;; load general early for now
(use-package general
  :straight t)

(use-package org
  :straight t)

(defun magic_rb/org-transclusion-babel-load (&optional narrowed)
  "Call `org-babel-load-file` on all transcludes in the current file."
  (interactive "P")
  (save-restriction
    (let ((marker (move-marker (make-marker) (point)))
	  (load-link (lambda ()
		       (let* ((keyword-plist (org-transclusion-keyword-string-to-plist))
			      (link (org-transclusion-wrap-path-to-link
				     (plist-get keyword-plist :link)))

			      (link-raw (org-element-property :raw-link link))
			      (link-type (org-element-property :type link)))
			 (when (string-equal link-type "id")
			   (message "RB Loading: %s" (org-id-find-id-file link-raw))
			   (org-babel-load-file (org-id-find-id-file link-raw)))))))
      (unless narrowed (widen))
      (goto-char (point-min))

      ;; Handle inactive transclusions
      (let ((regexp "^[ \t]*#\\+TRANSCLUDE:"))
	(while (re-search-forward regexp nil t)
	  ;; Don't transclude if within a transclusion to avoid infinite
	  ;; recursion
	  (unless (or (org-transclusion-within-transclusion-p)
		      (plist-get (org-transclusion-keyword-string-to-plist)
				 :disable-auto))
	    (funcall load-link))))

      ;; Handle active transclusions
      (while (setq match (text-property-search-forward 'org-transclusion-type))
	(goto-char (prop-match-beginning match))
	(org-transclusion-remove)
	(funcall load-link)
	(org-transclusion-add))

      (goto-char marker)
      (move-marker marker nil) ; point nowhere for GC
      t)))

(require 'cl-lib)

(use-package org-roam
  :straight t
  :demand t
  :init
  (setq org-roam-v2-ack t
	org-roam-directory "~/roam"))

(defvar magic_rb/org-init-files
      (cl-concatenate
       'list
       (directory-files "~/.emacs.d/org" t "\\.org$")
       (seq-map #'car
	(org-roam-db-query
	 [:select [nodes:file]
	  :from tags
	  :left-join nodes
	  :on (= tags:node-id nodes:id)
	  :where (like tag (quote "%\"emacs-load\""))])))
      "List of org files, which should be tangled and loaded.")

(defvar magic_rb/org-el-init-files
      (cl-map 'list
	      (lambda (file) (concat (file-name-sans-extension file) ".el"))
	      magic_rb/org-init-files)
      "List of generated elisp files from magic_rb/org-init-files.")

(defun magic_rb/delete-file-maybe (file)
  "If FILE exists, delete it."
  (when (file-exists-p file)
    (delete-file file)))

(mapc #'magic_rb/delete-file-maybe magic_rb/org-el-init-files)
(mapc #'org-babel-load-file magic_rb/org-init-files)


(provide '.emacs)
;;; .emacs ends here
