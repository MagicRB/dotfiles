;;
;; $Id: org-task-generate-calendar-view.el,v 08890e87177e 2016/04/18 10:24:19 pkgs $
;;


(load "org-task-dump-logs.el")
(load "svg.el")

(defun hmw/generate-calendar-view(file-name year heading statechanges)
  "Generate a calendar view for the given YEAR and colours the dates
according to the given STATECHANGES. The view is annotated with a
HEADING and a legend. It is saved in FILE-NAME in SVG format.

If the list contains several state changes for a given date, CANCELLED has precedence over DONE over OPEN."
  (with-temp-buffer
    (hmw/svg-header 650 170)
    (hmw/svg-text 10 20 (format "Statistics for %s for %s" heading year))

    ;; Month names
    (let ((xpos 40)
          (ypos 40))
      (dotimes (i 12)
        (hmw/svg-text xpos ypos (elt calendar-month-abbrev-array i))
        (setq xpos (+ 40 (* (ceiling (/ (calendar-day-number (list (+ 2 i) 1 year)) 7.0)) 11)))))

    ;; Day names
    (let ((xpos 10)
          (ypos 60))
      (dotimes (i 7)
        (hmw/svg-text xpos ypos (elt calendar-day-abbrev-array
                                     (% (+ i calendar-week-start-day) 7)))
        (setq ypos (+ ypos 11))))

    ;; Boxes
    (let ((xpos 40)
          (ypos 50)
	  (colour)
	  (el)
          (free (% (+ (calendar-day-of-week (list 1 1 year)) 6) 7)))

      (setq ypos (+ ypos (* free 11)))
      (dotimes (i (calendar-day-number (list 12 31 year)))
        (setq els (cl-remove-if-not (lambda (e) (equal (elt e 2) (1+ i))) statechanges))

        (setq el
              (car
               (or 
                (cl-remove-if-not (lambda (e) (equal (elt e 1) "CANCELLED")) els)
                (cl-remove-if-not (lambda (e) (equal (elt e 1) "DONE")) els)
                (cl-remove-if-not (lambda (e) (equal (elt e 1) "OPEN")) els))))

        (cond
	 ((equal el nil)
	  (hmw/svg-rect xpos ypos 10 10 "#646464"))

	 ((equal (elt el 1) "OPEN")
	  (hmw/svg-rect xpos ypos 10 10 "#0000ff"))
	 
	 ((equal (elt el 1) "DONE")
	  (hmw/svg-rect xpos ypos 10 10 "#00ff00"))

	 ((equal (elt el 1) "CANCELLED")
	  (hmw/svg-rect xpos ypos 10 10 "#ff0000")))
      
        ;; Jump to the next column
        (if (= (% (+ free 1 i) 7) 0)
            (progn
              (setq ypos 50)
              (setq xpos (+ xpos 11)))
          (setq ypos (+ ypos 11)))))

    ;; Legend
    (hmw/svg-text 10 150 "OPEN")
    (hmw/svg-rect 45 141 10 10 "#0000ff")
    (hmw/svg-text 70 150 "DONE")
    (hmw/svg-rect 105 141 10 10 "#00ff00")
    (hmw/svg-text 130 150 "CANCELLED")
    (hmw/svg-rect 195 141 10 10 "#ff0000")
    
    (hmw/svg-footer)
    (write-file file-name)))


(defun hmw/date-formatter(d year)
  "Converts a date D in ORG date format into the day number. As a side
  effect all dates that are not in YEAR are filtered."
  (let ((date (org-date-to-gregorian d)))
    (if (equal (elt date 2) year)
        (calendar-day-number date)
      nil)))


(defun hmw/org-task-generate-calendar-view(svg-file-name year)
  "Generate a calendar view of the state changes of a given task. The
  calendar view is generated for YEAR and saved in SVG-FILE-NAME in
  SVG format."
  (interactive "FSave as SVG file: \nnGenerate view for year (YYYY): ")
  (let ((log-entries (hmw/org-task-retrieve-logs
		      (lambda (d) (hmw/date-formatter d year)))))
    (hmw/generate-calendar-view svg-file-name year
				(car log-entries) (cdr log-entries))))

(provide 'org-task-generate-calendar-view)
