;; -*- lexical-binding: t -*-
;;; ytplay.el --- search for and play a YT video

;;; Commentary:

(require 'cl-lib)

(defvar ytplay-youtube-dl-commmad "/nix/store/g0656rl1hplb6w35xs6nc79r6rdm8gzp-python3.8-youtube-dl-2020.12.22/bin/youtube-dl")

(defvar ytplay-process nil)
(defvar ytplay-buffer (get-buffer-create " yt-play process buffer"))
(defvar ytplay-callback nil)
(defvar ytplay-fail-callback nil)

(defvar ytplay-command-queue (list))

(defun ytplay--run-yt-dl (args callback fail-callback)
  (if (or ytplay-callback ytplay-process ytplay-fail-callback)
      (push (list args callback fail-callback) ytplay-command-queue)
    (setq ytplay-callback callback)
    (setq ytplay-fail-callback fail-callback)
    (with-current-buffer ytplay-buffer (erase-buffer))
    (let ((command (append (list ytplay-youtube-dl-commmad) args)))
      (setq ytplay-process
	    (make-process
	     :name "youtube-dl"
	     :command command
	     :buffer ytplay-buffer
	     :sentinel 'ytplay--sentinel)))))

(defun ytplay--sentinel (process event)
  (let ((result (string-trim (with-current-buffer ytplay-buffer (buffer-string))))
	(callback ytplay-callback)
	(fail-callback ytplay-fail-callback)
	(event (string-trim event)))
    (with-current-buffer ytplay-buffer (erase-buffer))   
    (setq ytplay-process nil)
    (setq ytplay-callback nil)
    (setq ytplay-fail-callback nil)
    (if (string-equal event "finished")
	(funcall callback result)
      (funcall fail-callback)))
  (when-let ((command (pop ytplay-command-queue)))
    (ytplay--run-yt-dl (nth 0 command) (nth 1 command) (nth 2 command))))

(defun ytplay--run-yt-dl-seq (args-list callback)
  (let ((acc (list))
	(target-count (length args-list))
	(completed 0))
    (cl-loop for args in args-list
	     do (ytplay--run-yt-dl
		 args
		 (lambda (results)
		   (setq completed (+ completed 1))
		   (push results acc)
		   (when (eq target-count completed) (funcall callback acc)))
		 (lambda ()
		   (setq completed (+ completed 1))
		   (when (eq target-count completed) (funcall callback acc)))))))

(defun ytplay--reset ()
  (interactive)
  (setq ytplay-process nil)
  (setq ytplay-buffer (get-buffer-create " yt-play process buffer"))
  (setq ytplay-callback nil)
  (setq ytplay-fail-callback nil)
  (setq ytplay-command-queue (list)))

;; (defun ytplay--ivy-builder (acc rest)
;;   (if-let ((vid-id (pop rest)))
;;       (ytplay--run-yt-dl
;;        (list "--get-title" "--" vid-id)
;;        (lambda (results vid-id acc rest)
;; 	 (push `(,(string-trim results) . ,vid-id) acc)
;; 	 (ytplay--ivy-builder acc rest)))
;;     (let ((vid-id  (completing-read "Select a result: " acc)))
;;       )))

;; (results (split-string results))

(defun ytplay--ivy-callback (vid-ids)
  (let* ((vid-ids (split-string vid-ids))
	 (sequence (cl-loop for vid-id in vid-ids
			    collect (list "--get-title" "--" vid-id))))
    (ytplay--run-yt-dl-seq
     sequence
     (lambda (titles)
       (let* ((videos (cl-pairlis titles vid-ids))
	     (vid-id (completing-read "Select a result: " videos)))
	 (make-process
          :name "xdg-open"
          :command (list
		    "xdg-open"
		    (format
		     "https://www.youtube.com/watch?v=%s"
		     (cdr (assoc vid-id videos))))))))))

(defun ytplay-search (search-term)
  (interactive "MSearch term: ")
  (ytplay--run-yt-dl
   (list (format "ytsearch10:%s" search-term) "--get-id")
   'ytplay--ivy-callback
   (lambda () (message "ytplay.el failure!"))))
