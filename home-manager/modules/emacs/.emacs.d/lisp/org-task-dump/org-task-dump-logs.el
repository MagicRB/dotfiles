;;
;; $Id: org-task-dump-logs.el,v 1a9aa8013f28 2016/04/18 10:20:04 pkgs $
;;


(defun hmw/org-task-retrieve-logs(&optional date-formatter pom)
  "Retrieve all state change notes for a given task and return
them as a list of 3-tuples. Each tuple consists of the old state,
the new state and the timestamp of the state change. The
timestamp is formatted by the optional function DATE-FORMATTER.

The very first element of the returned list is not a 3-tuple, but a
string holding the heading of the processed task.

Optionally the point can be set to POM programmatically."
  (save-excursion
    (unless date-formatter (setq date-formatter 'identity))
    (if pom (goto-char pom))
    (org-back-to-heading t)
    (let* ((end (org-entry-end-position))
           (reversed org-log-states-order-reversed)
           (search (if reversed 're-search-forward 're-search-backward))
           (limit (if reversed end (point)))
           (re (format
                "^[ \t]*-[ \t]+\\(?:State \"%s\"\s+from\s+\"%s\".*%s%s\\)"
                org-todo-regexp
                org-todo-regexp
                org-ts-regexp-inactive
                (let ((value (cdr (assq 'done org-log-note-headings))))
                  (if (not value) ""
                    (concat "\\|"
                            (org-replace-escapes
                             (regexp-quote value)
                             `(("%d" . ,org-ts-regexp-inactive)
                               ("%D" . ,org-ts-regexp)
                               ("%s" . "\"\\S-+\"")
                               ("%S" . "\"\\S-+\"")
                               ("%t" . ,org-ts-regexp-inactive)
                               ("%T" . ,org-ts-regexp)
                               ("%u" . ".*?")
                               ("%U" . ".*?"))))))))
           log-entries)
      (unless reversed (goto-char end))
      (while  (funcall search re limit t)
        (push (list (org-match-string-no-properties 2)
                    (org-match-string-no-properties 1)
                    (funcall date-formatter (org-match-string-no-properties 3)))
	      log-entries))
      (push (org-no-properties (org-get-heading)) log-entries)
      log-entries)))

(provide 'org-task-dump-logs)
