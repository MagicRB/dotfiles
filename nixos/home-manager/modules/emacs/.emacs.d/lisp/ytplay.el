;;; ytplay.el --- search for and play a YT video

;;; Commentary:

(setq ytplay-youtube-dl-commmad "/nix/store/g0656rl1hplb6w35xs6nc79r6rdm8gzp-python3.8-youtube-dl-2020.12.22/bin/youtube-dl")

(setq ytplay-process nil)
(setq ytplay-buffer (get-buffer-create " yt-play process buffer"))
(setq ytplay-callback nil)
(setq ytplay-callback-args nil)

(setq ytplay-command-queue (list))

(defun ytplay--run-yt-dl (args callback &rest callback-args)
  (if (or ytplay-callback ytplay-process ytplay-callback-args)
      (push (list args callback) ytplay-command-queue)
    (progn
      (setq ytplay-callback callback)
      (setq ytplay-callback-args callback-args)
      (with-current-buffer ytplay-buffer (erase-buffer))
      (let ((command (append (list ytplay-youtube-dl-commmad) args)))
	(progn
	  ;; (message "%s - %s" command callback)
	  (setq ytplay-process
		(make-process
		 :name "youtube-dl"
		 :command command
		 :buffer ytplay-buffer
		 :sentinel 'ytplay--sentinel)))))))

(defun ytplay--sentinel (process event)
  (let ((result (string-trim (with-current-buffer ytplay-buffer (buffer-string))))
	(callback ytplay-callback)
	(callback-args ytplay-callback-args)
	(event (string-trim event)))
    (progn
      (with-current-buffer ytplay-buffer (erase-buffer))
      (setq ytplay-process nil)
      (setq ytplay-callback nil)
      (setq ytplay-callback-args nil)
      (cond
       ((string-equal event "finished") (apply callback result callback-args))
       (t (message "ytplay error")))))
  (when-let ((command (pop ytplay-command-queue)))
    (ytplay--run-yt-dl (nth 0 command) (nth 1 command))))

(defun ytplay--search-sentinel (process event)
  (cond
   ((string-equal event "finished")) ((ytplay--search-finished))
   ((t)) ((setq ytplay-process nil) (setq ytplay-callback nil))))

(defun ytplay--ivy-builder (acc rest)
  (if-let ((vid-id (pop rest)))
      (ytplay--run-yt-dl
       (list "--get-title" "--" vid-id)
       (lambda (results vid-id acc rest)
	 (push `(,(string-trim results) ,vid-id) acc)
	 (ytplay--ivy-builder acc rest))
       vid-id
       acc
       rest)
    (ivy-read
     "Select a result: "
     acc
     :action (lambda (x)
	       (message "%s" (format "https://www.youtube.com/watch?v=%s" (nth 1 x)))
	       (make-process
		:name "xdg-open"
		:command (list "xdg-open" (format "https://www.youtube.com/watch?v=%s" (nth 1 x))))))))

(defun ytplay--ivy-callback (results)
  (let ((results (split-string results)))
    (ytplay--ivy-builder (list "") results)))

(defun ytplay-search (search-term)
  (interactive "MSearch term: ")
  (ytplay--run-yt-dl (list (format "ytsearch10:%s" search-term) "--get-id") 'ytplay--ivy-callback))
