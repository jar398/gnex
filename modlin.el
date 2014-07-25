; -*- Version: 14; -*-
; KMP-ish mode line
; Last modified 2004-01-10 on rosebud.mumble.net

(defvar display-time-string nil)

; Emacs's mode-line-string is the following:
;  - Version 18.44
;    "--%1*%1*-Emacs: %17b   %M   %[(%m)%]----%3p-%-"
;  - Version 18.51
;     (""
;      mode-line-modified
;      mode-line-buffer-identification
;      "   "
;      global-mode-string
;      "   %[("
;      mode-name
;      minor-mode-alist
;      "%n"
;      mode-line-process
;      ")%]----"
;      (-3 . "%p")
;      "-%-")

(defvar not-nex-mode-line-format default-mode-line-format)

(defvar nex-mode-line-format
  (if (equal (substring emacs-version 0 5) "18.44")
      "%[GNEX  %M [%m] %b%] --%p--  %1*%1*%-"
      '("%[GNEX  "
	display-time-string
	" ["
	mode-name
	minor-mode-alist
	"%n"				;"Narrow", if appropriate
	mode-line-process
	"] %b%] --%p--  %1*%1*%-")))


(setq default-mode-line-format nex-mode-line-format)

(save-excursion
  (set-buffer "*scratch*")
  (setq mode-line-format default-mode-line-format))

(defvar display-time-process nil)

(defvar display-time-interval 60
  "*Seconds between updates of time in the mode line.")

(defun nex-display-time ()
  "Display current time in mode line of each buffer.
Updates automatically every minute."
  (interactive)
  (if (not (and display-time-process
		(eq (process-status display-time-process) 'run)))
      (save-excursion
	  (if display-time-process
	      (delete-process display-time-process))
;          (or (listp global-mode-string)
;              (setq global-mode-string '("")))
;          (or (memq 'display-time-string global-mode-string)
;              (setq global-mode-string
;                    (append global-mode-string '(display-time-string))))
	  (setq display-time-string "time and load")
	  (condition-case nil
	      (progn (setq display-time-process
			   (start-process "display-time" nil
					; "loadst" "-n"	;was "wakeup"
					  "wakeup"
					  (int-to-string
					      display-time-interval)))
		     (process-kill-without-query display-time-process)
		     (set-process-sentinel display-time-process
					   'display-time-sentinel)
		     (set-process-filter display-time-process
					 'nex-display-time-filter))
	    (error 'error)))))

(defun display-time-sentinel (proc reason)
  (or (eq (process-status proc) 'run)
      (setq display-time-string ""))
  ;; Force mode-line updates
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))

(defun nex-display-time-filter (proc string)
  ;; We ignore the output of the process entirely, for now.  That means
  ;; no load average or mail notification.  Fix later.

  (update-time)

  ;; Force redisplay of all buffers' mode lines to be considered.
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  ;; Do redisplay right now, if no input pending.
  (sit-for 0))

(defun really-update-time (&optional string)
  ;; Have  "Mon Apr 13 13:23:30 1987"
  ;;        0123456789012345678901234
  ;; Want  " 2:03pm 13 Apr 87"
  (let* ((time (or string (current-time-string)))
	 (hour   (read (substring time 11 13)))
	 (minute (substring time 14 16))
	 (hour-mod-12 (mod hour 12)))
    (if (= hour-mod-12 0) (setq hour-mod-12 12))
    (let ((string
	   (concat
	     (cond ((and (= hour 0) (equal minute "00"))
		    "Midnight")
		   ((and (= hour 12) (equal minute "00"))
		    "Noon")
		   (t
		    (concat (prin1-to-string hour-mod-12)
			    ":"
			    minute
			    (if (< hour 12) "am" "pm"))))
	     (substring time 7 10)
	     (substring time 3 7)
	     " "
	     (substring time 22 24))))
      (if (stringp default-mode-line-format)
	  (setq global-mode-string string)
	  (setq display-time-string string)))))

(if (emacs-version>= 20 2)
    (setq modlin-timer (run-with-timer 60 60 'update-time))
  (really-update-time))
