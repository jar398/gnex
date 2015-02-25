; -*- Version: 84; -*-
; Last modified: 2008-06-18 on biota.neurocommons.org

; NEX emulation for GNU emacs
; Thanks to Steve Gildea (gildea@bbn.com) for switch-to-previous-buffer.
; Copied from sid.stanford.edu 8/21/89
; Versions from void and rak merged 1/2/89
; Emacs 19 stuff incorporated 6/14/95
; More mods incorporated 5/28/98

; To do:
;  Search features: from beginning/end, etc.
;  Recenter definition (C-M-r)
;  Comment-smart M-q for Lisp mode
;  Better prefix character stuff, e.g. help for prefixes (C-h c, C-h k)
;  Numeric arg to just-one-space

(defvar nex-global-map (copy-keymap global-map))


(defun nex ()
  "Turn this emacs into a GNEX."
  (interactive nil)
  (if (boundp 'nex-mode-line-format)
      (setq default-mode-line-format nex-mode-line-format
	    mode-line-format nex-mode-line-format))
  (use-global-map nex-global-map))

(defun not-nex ()
  "Turn this GNEX into an emacs."
  (interactive nil)
  (if (boundp 'not-nex-mode-line-format)
      (setq default-mode-line-format not-nex-mode-line-format
	    mode-line-format not-nex-mode-line-format))
  (use-global-map global-map))

(defun gnex () (interactive nil) (nex))
(defun not-gnex () (interactive nil) (not-nex))

(defun eql (x y) (eq x y)) ;what's this for?  tab in shell ??

;----- copied from ~cph/elisp/cphlib.el

(defvar emacs-version-major)
(defvar emacs-version-minor)

(let* ((first-dot (string-match "\\." emacs-version))
       (esv (substring emacs-version (1+ first-dot)))
       (second-dot (string-match "\\." esv)))
  (setq emacs-version-major
	(string-to-number (substring emacs-version 0 first-dot)))
  (setq emacs-version-minor
	(string-to-number (if second-dot
			   (substring esv 0 second-dot)
			   esv))))

(defun emacs-version>= (major minor)
  (or (> emacs-version-major major)
      (and (= emacs-version-major major)
	   (>= emacs-version-minor minor))))

(cond ((emacs-version>= 19 18)
       ;; (menu-bar-mode -1)    ;why not?
       (condition-case nil
	   (scroll-bar-mode -1)
	 (error nil))
       )
      ((emacs-version>= 19 0)
       (menu-bar-mode -1)
       (scroll-bar-mode -1)
       ;; What's this for?
       (load-library "filladapt")))


; Miscellaneous fixes

(defun fix-scheme-mode ()
  (interactive nil)
  (modify-syntax-entry ?.
		       "_"
		       scheme-mode-syntax-table))

(put 'eval-expression  'disabled 'nil)
(put 'narrow-to-region 'disabled 'nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


(setq text-mode-hook 'turn-on-auto-fill)

(setq auto-mode-alist
      (append '(("\\.t$" . scheme-mode)
		("\\.sch$" . scheme-mode)
		("\\.txt$" . text-mode)
		("\\.lsp$" . lisp-mode)
		("^/usr/tmp/nn\\." . text-mode))
	      auto-mode-alist))

; Not defined in emacs 24.4
(autoload 'scheme-indent-sexp "scheme" "" t)

; Handy utilities

(defun uppercasep (c)
  (and (<= ?A c) (<= c ?Z)))

(defun lowercasep (c)
  (and (<= ?a c) (<= c ?z)))

(defun controlify (c)
  (if (= c ? )
      0
      (logxor (upcase c) 64)))

(defun controlp (char)
  (and (<= 0 char) (< char 32) (not (= char ?\e))))

(defun controllable (char)
  "Returns T iff CHAR is a meaningful argument to controlify."
  (or (= char ? )
      (and (<= 63 char) (< char 96))
      (and (<= ?a char) (<= char ?z))))

; Use concat instead
;(defun string-append (&rest strings)
;  (let ((s (make-string (apply '+ (mapcar 'length strings)) 0))
;        (i 0) (j 0))
;    (while strings
;      (setq j 0)
;      (while (< j (length (car strings)))
;        (aset s i (aref (car strings) j))
;        (setq j (+ j 1))
;        (setq i (+ i 1)))
;      (setq strings (cdr strings)))
;    s))

(defun read-char-from-minibuffer (prompt)
  (if (and (not executing-kbd-macro)    ;was executing-macro
	   (sit-for 1))
      (message prompt))
  (read-char))

(defun to-string (thing)
  (cond ((stringp thing) thing)
	((symbolp thing) (symbol-name thing))
	((numberp thing) (make-string 1 thing))
	(t (signal 'wrong-type-argument (list thing)))))


; Simple commands

; Space scroll-up iff buffer is read-only.

(defun self-insert-or-scroll-up (arg)
  "Same as self-insert, unless buffer is read-only, in which case scroll-up."
  (interactive "P")
  (if buffer-read-only
      (call-interactively 'scroll-up arg)
      (call-interactively 'self-insert-command arg)))

; Counter

(defvar *counter* 0)

(defun insert-counter (arg)
  (interactive "P")
  (if arg
      (if (numberp arg)
	  (setq *counter* arg)
	  (setq *counter* 0)))
  (insert (number-to-string *counter*))
  (setq *counter* (+ *counter* 1)))


; Quit = save-buffers-kill-emacs

(defun quit ()
  (interactive nil)
  (call-interactively 'save-buffers-kill-emacs))

; Control-meta-L

(defun previous-buffer (n)
  "Return the Nth previously selected buffer.  Default is normally 2.
Actually, it is the number of windows plus 1.  That is, no argument
switches to the most recently selected buffer that is not visible.  If
N is 1, repeated calls will cycle through all buffers; -1 cycles the
other way.  If N is greater than 1, the first N buffers on the buffer
list are rotated.  gildea 13 Feb 89"
;  (interactive "P")
  (if (not n)
      (other-buffer)
    (let ((buffer-list (buffer-list)))
      (setq n (prefix-numeric-value n))
      (cond ((= n 1)
	     (bury-buffer (current-buffer))
	     (setq n 2))
	    ((< n 0)
	     (setq buffer-list (nreverse buffer-list))
	     (setq n (- n))))
      (while (and (> n 1) buffer-list)
	(setq n (1- n))
	(setq buffer-list (cdr buffer-list))
	(while (eq (elt (buffer-name (car buffer-list)) 0) ? )
	  (setq buffer-list (cdr buffer-list))))
      (if buffer-list
	  (car buffer-list)
	(error "There aren't that many buffers")))))

(defun switch-to-previous-buffer (n)
  "Switch to the Nth previously selected buffer.
See documentation for previous-buffer for explanation of values N can take."
  (interactive "P")
  (switch-to-buffer (previous-buffer n)))

; Force redisplay without repositioning

(defun force-redisplay (arg)
  "With no argument, forces redisplay without repositioning.
With prefix argument N, put point on line ARG."
  (interactive "P")
  (update-time)
  (cond ((null arg)
	 (let ((start (window-start)))  ;Kludge
	   (recenter nil)
	   (set-window-start (selected-window) start)))
	(t
	 (recenter arg))))

; Meta-vertical-bar

(defun draw-vertical-line (where)
  (interactive "P")
  (if (null where) (setq where (current-column)))
  (if (consp where) (setq where (car where)))
  (if (< (window-width (selected-window)) (+ where 4))
      (delete-other-windows))
  (split-window-horizontally (+ where 1))
  (other-window 1)
  (scroll-left (+ where 2))
  (other-window -1)
  ;; (with-output-to-temp-buffer "")
  )

; Mildly enhanced rmail

(defun read-mail (arg)
  "With no argument, runs rmail on ~/RMAIL.  With prefix argument,
prompts for the name of an rmail file."
  (interactive "P")
  (if arg
      (call-interactively 'rmail-input)
      (rmail)))

; Comment out region

(defun comment-out-region (arg)
  "Insert comment string at beginning of each line in the region."
  (interactive "P")
  (let (start end)
    (if (< (point) (mark))
        (setq start (point) end (mark-marker))
        (setq start (mark) end (point-marker)))
    (save-excursion
      (untabify start (marker-position end))
      (goto-char start)
      (if (not (bolp))
	  (progn (end-of-line) (forward-char)))
      (while (< (point) (marker-position end))
	(if (eq arg '-)
	    (if (looking-at comment-start)
		(delete-char (length comment-start)))
	    (insert comment-start))
	(end-of-line)
	(forward-char)))))

;(defun uncomment-out-region (arg)
;  (interactive nil)
;  (comment-out-region '-))


; Allow use of ISO 8859 (European) character set, both input and ouput

;(standard-display-european t)

(defun insert-ISO-8859 (c)
  "Insert an ISO 8859 character by adding 128 to argument.
The mapping is as follows:

 ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏ
 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNO

ÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ
PQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~

For example, to insert a bullet, type control-X , 7"
  (interactive "c")
  (insert (+ c 128)))


(define-key nex-global-map "\C-X," 'insert-ISO-8859)


; Word count?

(defun wc () (interactive) (count-matches "[a-zA-Z]*"))


; Character search

(defvar last-search-string nil)

(defun character-search-forward (char)
  "Search forward for a character.
The following characters are treated specially:
  ^A       String search
  ^H       Help
  ^I       Incremental search
  ^Q	   Quote
  ^S, ESC  Repeat previous search"
  (interactive "cC-s-")
  (update-time)   ;Randomness
  (cond ((= char 1)			;^a
	 (setq command-history (cdr command-history))
	 (call-interactively 'string-search-forward))
	((or (= char 19)		;^s -- last thing
	     (= char 27)		;esc
	     (= char 28))		;^\
	 (setq command-history (cdr command-history))
	 (string-search-forward last-search-string))
	((= char 9)			;^i -- incremental
	 (setq command-history (cdr command-history))
	 (call-interactively 'isearch-forward))
	((= char 8)			;^h -- help
	 (describe-function this-command))
	((= char 17)			;^q -- quote
	 (string-search-forward (char-to-string (read-char))))
	((= char 7) (keyboard-quit))
	((= char 22)			;^v -- delimited
	 (setq command-history (cdr command-history))
	 (call-interactively '%delimited-search-forward))
	(t
	 (string-search-forward (char-to-string char)))))

(defun character-search-backward (char)
  "Search backward for a character.
The following characters are treated specially:
  ^A       String search
  ^H       Help
  ^I       Incremental search
  ^Q	   Quote
  ^R, ESC  Repeat previous search"
  (interactive "cC-r-")
  (cond ((= char 1)			;^a
	 (setq command-history (cdr command-history))
	 (call-interactively 'string-search-backward))
	((or (= char 18) (= char 27))	;^r -- last thing
	 (setq command-history (cdr command-history))
	 (string-search-backward last-search-string))
	((= char 9)			;^i -- incremental
	 (setq command-history (cdr command-history))
	 (call-interactively 'isearch-backward))
	((= char 8)			;^h -- help
	 (describe-function this-command))
	((= char 17)			;^q -- quote
	 (string-search-backward (char-to-string (read-char))))
	((= char 7) (keyboard-quit))
	(t
	 (string-search-backward (char-to-string char)))))

(defun string-search-forward (string)
  (interactive "sSearch: ")
  (string-search string 'search-forward nil))

(defun string-search-backward (string)
  (interactive "sSearch backward: ")
  (string-search string 'search-backward nil))

(defun %delimited-search-forward (string)
  (interactive "sDelimited search: ")
  (string-search string 're-search-forward t))

(defun string-search (string search-function delimitp)
  (if (equal string "")
      (setq string last-search-string)
      (setq last-search-string string))
  (if delimitp
      (setq string (concat "\\b"
			   (regexp-quote string)
			   "\\b")))
  (let ((pt (save-excursion (if (funcall search-function string nil t)
				(point)
				nil))))
    (if pt
	(progn (set-mark-command nil) (goto-char pt))
	(signal 'search-failed (list string)))))

; Mini-Find Tag

(defvar last-mini-tag "" "Last tag sought by mini-find-tag.")

(defun mini-find-tag (tagname &optional next)
  "Search for a definition of TAGNAME in current buffer.
 If TAGNAME is a null string, the expression in the buffer
around or before point is used as the tag name.
 If second arg NEXT is non-nil (interactively, with prefix arg),
searches for the next definition in the buffer
that matches the tag name used in the previous mini-find-tag."

  (interactive (if current-prefix-arg
		   '(nil t)
		 (list (read-string "Mini-find tag: "))))
  (if (equal tagname "")             ;See definition of find-tag.
      (setq tagname (save-excursion
		      (buffer-substring
		       (progn (backward-sexp 1) (point))
		       (progn (forward-sexp 1) (point))))))
  (let ((pt (save-excursion
	      (if (not next)
		  (goto-char (point-min))
		(setq tagname last-mini-tag))
	      (setq last-mini-tag tagname)
	      (if (re-search-forward
		     (concat "^(def"
			     constituent-char-regexp
			     "*[ \t(']*"
			     (regexp-quote tagname))
		     nil t)
		  (point)
		nil))))
    (if pt
	(progn (set-mark-command nil)
	       (goto-char pt))
      (signal 'search-failed '()))))

; Regexp that matches any Lisp/Scheme constituent character (including :)

(defvar constituent-char-regexp "[!-'*-/0-9:<-@A-Z^_a-z~]")

; indent-differently

(defun indent-differently ()
  (interactive nil)
  (let ((here (point)))
    (save-excursion
      (back-to-indentation)
      (backward-up-list 1)
      (forward-char 1)
      (let ((i -1)
	    (function nil)
	    (p (point)))
	(while (<= (point) here)
	  (setq i (+ i 1))
	  (forward-sexp 1)
	  (if (= i 0)
	      (setq function (buffer-substring p (point)))))
	(setq i (- i 1))
	(let ((name (intern (downcase function))))
	  (cond ((equal (get name scheme-indent-property) i)
		 (message "Indent %s nil" name)
		 (put name scheme-indent-property nil))
		(t
		 (message "Indent %s %d" name i)
		 (put name scheme-indent-property i))))))
    ;; was: (scheme-indent-line)
    (indent-for-tab-command)))

(defvar scheme-indent-property
  (if (emacs-version>= 19 0)
      'scheme-indent-function
      'scheme-indent-hook))

; Version numbers
; Pirated from GNU's "set-auto-mode"

(defun get-attribute-position (name)
  "Look for an attribute in the -*- attribute list of current buffer.
Returns a pair (beg . end) of positions if attribute NAME:FOO occurs
between -*-'s in the first lineof the file, otherwise returns nil."
  (let (beg end val)
    (save-excursion
      (goto-char (point-min))
      (skip-chars-forward " \t\n")
      (if (and (search-forward "-*-"
			       (save-excursion (end-of-line) (point))
			       t)
	       (progn
		 (skip-chars-forward " \t")
		 (setq beg (point))
		 (search-forward "-*-"
				 (save-excursion (end-of-line) (point))
				 t))
	       (progn
		 (forward-char -3)
		 (skip-chars-backward " \t")
		 (setq end (point))
		 (goto-char beg)
		 (if (search-forward ":" end t)
		     (progn
		       (goto-char beg)
		       (if (let ((case-fold-search t))
			     (search-forward (concat name ":") end t))
			   (progn
			     (skip-chars-forward " \t")
			     (setq beg (point))
			     (if (search-forward ";" end t)
				 (forward-char -1)
			       (goto-char end))
			     (skip-chars-backward " \t")
			     (setq end (point))
			     t)))
		   (equal name "mode"))))
	  (cons beg end)
	nil))))

(defun maybe-increment-version ()
  (update-time)
  (let ((v (get-attribute-position "version")))
    (if v
	(let ((z (string-to-number (buffer-substring (car v) (cdr v)))))
	  (if z
	      (save-excursion
		(goto-char (car v))
		(delete-region (point) (cdr v))
		(insert (number-to-string (1+ z))))
	    (error "weird version number"))))))

; was: (setq write-file-hook 'maybe-increment-version)
(add-hook 'maybe-increment-version 'write-file-functions)

(defun update-time ()
  (when (fboundp 'really-update-time)
    (really-update-time)))


; Indentations

; Scheme stuff
(put 'case      scheme-indent-property 1)
(put 'do        scheme-indent-property 2)
(put 'if        scheme-indent-property nil)
(put 'letrec    scheme-indent-property 1)
(put 'call-with-current-continuation scheme-indent-property 0)
(put 'call-with-input-file  scheme-indent-property 1)
(put 'call-with-output-file scheme-indent-property 1)
(put 'call-with-values scheme-indent-property 1)
(put 'with-input-from-file  scheme-indent-property 1)
(put 'with-output-to-file   scheme-indent-property 1)
(put 'syntax-rules scheme-indent-property 1)

; Unusual stuff
(put 'fluid-let scheme-indent-property 1)

; T stuff
;(put 'bind      scheme-indent-property 1)
(put 'destructure scheme-indent-property 1)
(put 'iterate   scheme-indent-property 2)
(put 'labels    scheme-indent-property 1)
(put 'object    scheme-indent-property 1)
(put 'operation scheme-indent-property 1)
(put 'receive   scheme-indent-property 2)
(put 'select    scheme-indent-property 1)
(put 'with-open-ports   scheme-indent-property 1)

; Common Lisp stuff
(put 'block		   'lisp-indent-hook 1)
(put 'case		   'lisp-indent-hook 1)
(put 'do                   'lisp-indent-hook 2)
(put 'dolist		   'lisp-indent-hook 1)
(put 'do-external-symbols  'lisp-indent-hook 1)
(put 'do-symbols	   'lisp-indent-hook 1)
(put 'ecase		   'lisp-indent-hook 1)
(put 'error-with-restarts  'lisp-indent-hook 1)
(put 'etypecase		   'lisp-indent-hook 1)
(put 'eval-when		   'lisp-indent-hook 1)
(put 'flet                 'lisp-indent-hook 1)
(put 'if                   'lisp-indent-hook nil)
(put 'labels               'lisp-indent-hook 1)
(put 'locally		   'lisp-indent-hook 1)
(put 'macrolet		   'lisp-indent-hook 1)
(put 'multiple-value-prog1 'lisp-indent-hook 1)
(put 'multiple-value-bind  'lisp-indent-hook 2)
(put 'progv		   'lisp-indent-hook 2)
(put 'signal-with-restarts 'lisp-indent-hook 1)
(put 'tagbody		   'lisp-indent-hook 0)
(put 'typecase		   'lisp-indent-hook 1)
(put 'unless		   'lisp-indent-hook 1)
(put 'when		   'lisp-indent-hook 1)
(put 'with-open-file       'lisp-indent-hook 1)
(put 'with-input-from-string 'lisp-indent-hook 1)
(put 'with-output-to-string 'lisp-indent-hook 1)

(put 'DEFUN		   'lisp-indent-hook 2)
(put 'DOLIST		   'lisp-indent-hook 1)
(put 'LET		   'lisp-indent-hook 1)
(put 'LET*		   'lisp-indent-hook 1)
(put 'MULTIPLE-VALUE-BIND  'lisp-indent-hook 2)

; Conditions stuff
(put 'define-condition	   'lisp-indent-hook 3)
(put 'handler-case	   'lisp-indent-hook 1)
(put 'handler-bind	   'lisp-indent-hook 1)
(put 'restart-case	   'lisp-indent-hook 1)
(put 'restart-bind	   'lisp-indent-hook 1)
(put 'with-simple-restart  'lisp-indent-hook 1)


; Returns a pair (command . string)

; Can't use read-key-sequence because it doesn't take keymaps as
; arguments.

(defun get-key-sequence (nextc next-global-map next-local-map prefix)
  (let (c local global keybuf local-command global-command)

    (setq keybuf "")

    (while (or next-local-map next-global-map)
      (if (>= nextc 0)
	  (progn (setq c nextc)
		 (setq nextc -1))
	  (setq c (read-char-from-minibuffer
		   (if (= (length keybuf) 0)
		       prefix
		       (concat prefix
			       (key-description keybuf)
			       "-")))))

      (if (>= c 0200)			;Meta bit -> escape
	  (progn (setq nextc (logand c 0177))
		 (setq c meta-prefix-char)))
      (setq global 
	    (if next-global-map
		(get-keyelt (access-keymap next-global-map c))
		nil))
      (setq local
	    (if next-local-map
		(get-keyelt (access-keymap next-local-map c))
		nil))

      ;; If C is not defined in either keymap
      ;; and it is an uppercase letter, try corresponding lowercase.

      (if (and (null global) (null local) (uppercasep c))
	  (progn
	    (setq global
		  (if next-global-map
		      (get-keyelt (access-keymap next-global-map (downcase c)))
		      nil))
	    (setq local
		  (if next-local-map
		      (get-keyelt (access-keymap next-local-map (downcase c)))
		      nil))

	    ;; If that has worked better that the original char,
	    ;; downcase it permanently.
	    (if (or global local)
		(setq c (downcase c)))))

      (setq keybuf (concat keybuf (make-string 1 c)))

      (setq next-local-map nil)
      (setq next-global-map nil)

      (setq local-command local)
      (setq global-command global)

      ;; Trace symbols to their function definitions

      (while (and (symbolp global) (fboundp global))
	(setq global (symbol-function global)))
      (while (and (symbolp local) (fboundp local))
	(setq local (symbol-function local)))

      ;; Are the definitions prefix characters?

      (if (or (keymapp local)
	      ;; If nextc is set, we are processing a prefix char
	      ;; that represents a meta-bit.
	      ;; Let a global prefix definition override a local non-prefix.
	      ;; This is for minibuffers that redefine Escape for completion.
	      ;; A real Escape gets completion, but Meta bits get ESC-prefix.
	      (and (or (null local) (>= nextc 0))
		   (keymapp global)))

	  (progn (setq next-local-map
		       (if (keymapp local) local nil))
		 (setq next-global-map
		       (if (keymapp global) global nil)))))

    ;; (message (concat prefix (key-description keybuf)))
    (list global-command local-command keybuf)))

; Translated from C code in keyboard.c

(defun get-keymap (object)
  (let ((tem object))
    (while (and (symbolp tem) (fboundp tem))
      (setq tem (symbol-function tem)))
    (if (keymapp tem)
	tem
	nil)))

; I can't figure this one out at all.

(defun get-keyelt (object)
  (let (map)
    (while (progn (setq map (get-keymap (car-safe object)))
		  (keymapp map))
      (setq object (cdr object))
      (setq object (if (consp map)
		       (cdr (assq object (cdr map)))
		       (aref map object))))
    object))

; The representation of keymaps changes with each major version of emacs.

(defun access-keymap (map char)
  "Get from MAP the binding of character CHAR."
  (cond ((symbolp map)
	 (access-keymap (symbol-function map) char))
	((vectorp map)
	 (aref map char))  ;obs.
	((not (consp map))
	 nil)				;error
	((vectorp (car (cdr map)))
	 (aref (car (cdr map)) char))
	((char-table-p (car (cdr map)))
	 (aref (car (cdr map)) char))
	(t
	 (cdr (assq char (cdr map))))))


(define-key nex-global-map "\C-c" nil)
(define-key nex-global-map "\C-w" nil)
(define-key nex-global-map "\C-x" nil)
(define-key nex-global-map "\e" nil)

; Command prefix commands 

; Control
(defvar nex-control-map (make-sparse-keymap))

(defun nex-control-prefix ()
  (interactive nil)
  (let ((stuff (get-control-command)))
    ;; stuff is (global local string)
    (call-interactively (or (car (cdr stuff)) (car stuff)))))

(defun get-control-command ()
  (let ((c (read-char-from-minibuffer "C-")))
    (if (controllable c)
	(get-key-sequence (if (controlp c) c (controlify c))
			  nex-global-map
			  nil
			  "")
	(get-key-sequence c
			  nex-control-map
			  nil
			  "C-"))))

(defun define-control (string command)
  (if (controllable (aref string 0))
      (define-key nex-global-map
	(concat (to-string (controlify (aref string 0)))
		(substring string 1))
	command)
      (define-key nex-control-map string command)))

; Meta

(defvar nex-meta-map (copy-keymap 'ESC-prefix))

(defun nex-meta-prefix ()
  (interactive nil)
  (let* ((keys (this-command-keys))
	 (c (aref keys (- (length keys) 1)))
	 (stuff (get-meta-command (controlp c)))
	 (cmd (or (car (cdr stuff)) (car stuff))))
    ;; stuff is (global local string)
    (if cmd
	(call-interactively cmd)
      (error "Undefined: M-%s" (nth 2 stuff)))))

; Returns (global local string)

(defun get-meta-command (flush-controls)
  (let ((c (read-char-from-minibuffer "M-")))
    (get-key-sequence (if (and flush-controls (controlp c))
			  (logxor c 64)
			  c)
		      nex-meta-map
		      (local-meta-map)
		      "M-")))

(defun local-meta-map ()
  (if (current-local-map)
      (get-keyelt (access-keymap (current-local-map) ?\e))
      nil))

(defun define-meta (string command)
  (define-key nex-meta-map string command))

; Control-meta

(defvar nex-control-meta-map (make-sparse-keymap))

(defun nex-control-meta-prefix ()
  "Emulate control-meta command prefix."
  (interactive nil)
  (let ((stuff (get-control-meta-command)))
    (call-interactively (or (car (cdr stuff)) (car stuff)))))

;+++ Problem: there's no way to type local mode commands like C-c x
(defun get-control-meta-command ()
  (let ((c (read-char-from-minibuffer "C-M-")))
    (if (or (controlp c) (controllable c))
	(get-key-sequence (if (controlp c) c (controlify c))
			  nex-meta-map
			  (local-meta-map)
			  "M-")   ;this will look funny sometimes
	(get-key-sequence c
			  nex-control-meta-map
			  nil     ;+++ Fix later
			  "C-M-"))))

(defun define-control-meta (string command)
  (let ((c (aref string 0)))
    (if (controllable c)
	;; Turn it into meta- (control- X)
	(let ((new-string (concat (to-string (controlify c))
				  (substring string 1))))
	  (define-key nex-meta-map new-string command))
	(define-key nex-control-meta-map string command))))

; Control-X

(defvar nex-control-x-map (copy-keymap 'Control-X-prefix))
(fset 'nex-control-x-prefix nex-control-x-map)

; Key bindings

(define-key nex-global-map " " 'self-insert-or-scroll-up)
(define-key nex-global-map "\C-?" 'backward-delete-char-untabify) ;rubout

(define-key minibuffer-local-completion-map "\C-m"
  'minibuffer-complete-and-exit)

; Control
(define-control "." 'tags-loop-continue)
(define-control "z" 'nex-control-meta-prefix)
(define-control "h" 'help-command)	;?
(define-control "l" 'force-redisplay)
(define-control "r" 'character-search-backward)
(define-control "s" 'character-search-forward)
(define-control "w" 'nex-meta-prefix)
(define-control "x" 'nex-control-x-prefix)
(define-control "\\" 'character-search-forward)
(define-control "["  nex-meta-map)	;escape
(define-control "^" 'nex-control-prefix)
(define-control "\C-i" 'indent-differently)  ;control-tab
(define-control "%" 'tags-search)                ; 5/3/1999

; Meta
(define-meta "i" 'indent-relative)      ;++
(define-meta "s" 'center-line)          ;++
(define-meta " "    'just-one-space)
(define-meta "/"    'dabbrev-expand) ;++    was describe-key-briefly
(define-meta "|"    'draw-vertical-line)
(define-meta ","    'mini-find-tag)     ;++
(define-meta "]"    'forward-paragraph)
(define-meta "["    'backward-paragraph)

; Control-meta
(define-control-meta "l" 'switch-to-previous-buffer) ;++
(define-control-meta "q" 'scheme-indent-sexp)

(define-control-meta "\C-?" 'backward-kill-sexp)
(define-control-meta ";" 'kill-comment)
(define-control-meta "%" 'tags-query-replace)

; Control-X
(define-key nex-global-map "\C-x\C-b" 'electric-buffer-list)
;(define-key nex-global-map "\C-x\C-c" 'suspend-emacs)
(define-key nex-global-map "\C-x\C-k" 'kill-region)
(define-key nex-global-map "\C-x\C-y" 'insert-file)
(define-key nex-global-map "\C-x\C-\\" 'save-buffer) ;instead of ^X ^S
(define-key nex-global-map "\C-xi"     'info)
(define-key nex-global-map "\C-xr"     'read-mail)
(define-key nex-global-map "\C-x#"     'insert-counter)

; eval-expression is now  ESC ESC :

; Candidate for inclusion?
;
;; Mark the next URL in the buffer.
;; See http://www.w3.org/Addressing/URL/5_URI_BNF.html
;
;(defun mark-url ()
;  (interactive nil)
;  (re-search-forward "http:[a-z0-9A-Z/:$_@.&=#?%~-]*")  ;point is at end
;  (push-mark (match-beginning 0)))
;
;(define-meta "\"" 'mark-url)


; For Java, Common Lisp

(setq completion-ignored-extensions
      (append '(".class" ".fasl" ".fsl")
	      completion-ignored-extensions))

;--------------------
; Things from JAR's .gnex file

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
; renamed to buffer-file-coding-system in 23.2
(setq default-buffer-file-coding-system 'utf-8)
;; From Emacs wiki
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;; MS Windows clipboard is UTF-16LE 
(set-clipboard-coding-system 'utf-16le-dos)

(setq resize-mini-windows nil)

; Deal with emacs 21 horrors?  Maybe not.
(add-hook 'comint-mode-hook
	  '(lambda ()
	     (remove-hook 'comint-output-filter-functions
 	     		  'comint-postoutput-scroll-to-bottom)
	     (remove-hook 'pre-command-hook 'comint-preinput-scroll-to-bottom)
	     (remove-hook 'pre-command-hook 'comint-preinput-scroll-to-bottom t)))

; Apparently these aren't respected.  Ugh!
; comint-scroll-to-bottom-on-output
; comint-scroll-to-bottom-on-input

(global-font-lock-mode nil)

(setq inhibit-startup-screen t)  ;Emacs 22

; Recover vertical space
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

; FIXES FOR EMACS VERSION 24

(setq line-move-visual nil)

(add-hook 'comint-mode-hook
	  '(lambda ()
	     (define-meta "p" 'comint-previous-input)
	     (define-meta "n" 'comint-next-input)))

(add-hook 'shell-mode-hook
	  '(lambda ()
	     (define-meta "p" 'comint-previous-input)
	     (define-meta "n" 'comint-next-input)))


; Fix control-meta-Q
(define-control-meta "q" 'indent-sexp)

; Fix control-M and control-J
(when (fboundp 'electric-indent-mode)
  (electric-indent-mode 0)
  (add-hook 'java-mode-hook '(lambda ()
                               (electric-indent-mode 0))))

(setq delete-active-region nil)

; From lisp IRC
(setq-default indent-tabs-mode nil)


;--------------------
; Coda

(use-global-map nex-global-map)
(put 'nex 'loaded t)

; Sub-load some stuff

(condition-case nil
    (if (and (getenv "GNEX")
	     (load "$GNEX/modlin" nil t)
	     (fboundp 'start-process))
	;; This loses in newer emacs because the program "wakeup" isn't
	;; found.
	(nex-display-time))
  (error (message "lost loading modlin")))

; Load init file...  (why not?)

;(if (not (get 'nex 'init-file-loaded))
;    (progn (message "loading .gnex")
;	   (load-file "~/.gnex")
;	   (put 'nex 'init-file-loaded t)))


;; Not surprisingly, some code in this file is based on code from Gnu
;; Emacs, which is:

;; Copyright (C) 1985, 1986, 1987, ... Free Software Foundation, Inc.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.
