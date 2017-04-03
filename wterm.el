;;; wterm.el --- a Windows console terminal mode package                     -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Debugfan Chin

;; Author: Debugfan Chin <debugfanchin@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Put a description of the package here

;;; Code:

;; code goes here

(require 'term)

;;;###autoload
(defgroup wterm nil
    "A Windows console terminal mode"
    :group 'processes)

;;;###autoload
(defcustom wterm-disable-flag nil
  "Non-nil means disable wterm..."
  :type 'boolean
  :require 'wterm
  :group 'wterm)

;;;###autoload  
(defcustom wterm-standard-filter-flag nil
  "Non-nil means use standard output filter..."
  :type 'boolean
  :require 'wterm
  :group 'wterm)
  
(defun wterm-toggle-filter()
    (interactive)
    (setq wterm-standard-filter-flag (not wterm-standard-filter-flag))
    (message "wterm-standard-filter-flag is %s" wterm-standard-filter-flag))

(defun wterm-emulate-terminal (proc str)
  (with-current-buffer (process-buffer proc)
    (let* ((i 0) char funny
	   count       ; number of decoded chars in substring
	   count-bytes ; number of bytes
	   decoded-substring
	   save-point save-marker old-point temp win
	   (buffer-undo-list t)
	   (selected (selected-window))
	   last-win
           handled-ansi-message
	   (str-length (length str)))
      (save-selected-window

        (let ((newstr (term-handle-ansi-terminal-messages str)))
          (unless (eq str newstr)
	    (setq handled-ansi-message t
		  str newstr)))
        (setq str-length (length str))

	(when (marker-buffer term-pending-delete-marker)
	  ;; Delete text following term-pending-delete-marker.
	  (delete-region term-pending-delete-marker (process-mark proc))
	  (set-marker term-pending-delete-marker nil))

	(when (/= (point) (process-mark proc))
	  (setq save-point (point-marker)))

        (setf term-vertical-motion
              (if (eq (window-buffer) (current-buffer))
                  'vertical-motion
                'term-buffer-vertical-motion))
        (setq save-marker (copy-marker (process-mark proc)))
	(goto-char (process-mark proc))

	(save-restriction
	  ;; If the buffer is in line mode, and there is a partial
	  ;; input line, save the line (by narrowing to leave it
	  ;; outside the restriction ) until we're done with output.
	  (when (and (> (point-max) (process-mark proc))
		     (term-in-line-mode))
	    (narrow-to-region (point-min) (process-mark proc)))

	  (when term-log-buffer
	    (princ str term-log-buffer))
	  (cond ((eq term-terminal-state 4) ;; Have saved pending output.
		 (setq str (concat term-terminal-parameter str))
		 (setq term-terminal-parameter nil)
		 (setq str-length (length str))
		 (setq term-terminal-state 0)))

	  (while (< i str-length)
	    (setq char (aref str i))
	    (cond ((< term-terminal-state 2)
		   ;; Look for prefix of regular chars
		   (setq funny
			 (string-match "[\r\n\000\007\033\t\b\032\016\017]"
				       str i))
		   (when (not funny) (setq funny str-length))
		   (cond ((> funny i)
			  ;; Decode the string before counting
			  ;; characters, to avoid garbling of certain
			  ;; multibyte characters (bug#1006).
			  (setq decoded-substring
				(decode-coding-string
				 (substring str i funny)
				 locale-coding-system))
			  (cond ((eq term-terminal-state 1)
				 ;; We are in state 1, we need to wrap
				 ;; around.  Go to the beginning of
				 ;; the next line and switch to state
				 ;; 0.
				 (term-down 1 t)
				 (term-move-columns (- (term-current-column)))
				 (setq term-terminal-state 0)))
			  (setq count (length decoded-substring))
			  (setq temp (- (+ (term-horizontal-column) count)
					term-width))
			  (cond ((or term-suppress-hard-newline (<= temp 0)))
				;; All count chars fit in line.
				((> count temp) ;; Some chars fit.
				 ;; This iteration, handle only what fits.
				 (setq count (- count temp))
				 (setq count-bytes
				       (length
					(encode-coding-string
					 (substring decoded-substring 0 count)
					 'binary)))
				 (setq temp 0)
				 (setq funny (+ count-bytes i)))
				((or (not (or term-pager-count
					      term-scroll-with-delete))
				     (>  (term-handle-scroll 1) 0))
				 (term-adjust-current-row-cache 1)
				 (setq count (min count term-width))
				 (setq count-bytes
				       (length
					(encode-coding-string
					 (substring decoded-substring 0 count)
					 'binary)))
				 (setq funny (+ count-bytes i))
				 (setq term-start-line-column
				       term-current-column))
				(t ;; Doing PAGER processing.
				 (setq count 0 funny i)
				 (setq term-current-column nil)
				 (setq term-start-line-column nil)))
			  (setq old-point (point))

			  ;; Insert a string, check how many columns
			  ;; we moved, then delete that many columns
			  ;; following point if not eob nor insert-mode.
			  (let ((old-column (current-column))
				columns pos)
			    (insert (decode-coding-string (substring str i funny) locale-coding-system))
			    (setq term-current-column (current-column)
				  columns (- term-current-column old-column))
			    (when (not (or (eobp) term-insert-mode))
			      (setq pos (point))
			      (term-move-columns columns)
			      (delete-region pos (point)))
			    ;; In insert mode if the current line
			    ;; has become too long it needs to be
			    ;; chopped off.
			    (when term-insert-mode
			      (setq pos (point))
			      (end-of-line)
			      (when (> (current-column) term-width)
				(delete-region (- (point) (- (current-column) term-width))
					       (point)))
			      (goto-char pos)))
			  (setq term-current-column nil)

			  (put-text-property old-point (point)
					     'font-lock-face term-current-face)
			  ;; If the last char was written in last column,
			  ;; back up one column, but remember we did so.
			  ;; Thus we emulate xterm/vt100-style line-wrapping.
			  (cond ((eq temp 0)
				 (term-move-columns -1)
				 (setq term-terminal-state 1)))
			  (setq i (1- funny)))
			 ((and (setq term-terminal-state 0)
			       (eq char ?\^I)) ; TAB (terminfo: ht)
			  (setq count (term-current-column))
			  ;; The line cannot exceed term-width. TAB at
			  ;; the end of a line should not cause wrapping.
			  (setq count (min term-width
					   (+ count 8 (- (mod count 8)))))
			  (if (> term-width count)
			      (progn
				(term-move-columns
				 (- count (term-current-column)))
				(setq term-current-column count))
			    (when (> term-width (term-current-column))
			      (term-move-columns
			       (1- (- term-width (term-current-column)))))
			    (when (= term-width (term-current-column))
			      (term-move-columns -1))))
			 ((eq char ?\r)  ;; (terminfo: cr)
			  (term-vertical-motion 0)
			  (setq term-current-column term-start-line-column))
			 ((eq char ?\n)  ;; (terminfo: cud1, ind)
			  (unless (and term-kill-echo-list
				       (term-check-kill-echo-list))
			    (term-down 1 t)))
			 ((eq char ?\b)  ;; (terminfo: cub1)
			  (if (or wterm-disable-flag wterm-standard-filter-flag)
                (term-move-columns -1) (backward-delete-char 1)))
			 ((eq char ?\033) ; Escape
			  (setq term-terminal-state 2))
			 ((eq char 0))	       ; NUL: Do nothing
			 ((eq char ?\016))     ; Shift Out - ignored
			 ((eq char ?\017))     ; Shift In - ignored
			 ((eq char ?\^G) ;; (terminfo: bel)
			  (beep t))
			 ((and (eq char ?\032)
                               (not handled-ansi-message))
			  (let ((end (string-match "\r?$" str i)))
			    (if end
				(funcall term-command-hook
					 (decode-coding-string
					  (prog1 (substring str (1+ i) end)
					    (setq i (match-end 0)))
					  locale-coding-system))
			      (setq term-terminal-parameter (substring str i))
			      (setq term-terminal-state 4)
			      (setq i str-length))))
			 (t   ; insert char FIXME: Should never happen
			  (term-move-columns 1)
			  (backward-delete-char 1)
			  (insert char))))
		  ((eq term-terminal-state 2)	  ; Seen Esc
		   (cond ((eq char ?\133)	  ;; ?\133 = ?[

                          ;; Some modifications to cope with multiple
                          ;; settings like ^[[01;32;43m -mm
                          ;; Note that now the init value of
                          ;; term-terminal-previous-parameter has been
                          ;; changed to -1

			  (setq term-terminal-parameter 0)
			  (setq term-terminal-previous-parameter -1)
			  (setq term-terminal-previous-parameter-2 -1)
			  (setq term-terminal-previous-parameter-3 -1)
			  (setq term-terminal-previous-parameter-4 -1)
			  (setq term-terminal-more-parameters 0)
			  (setq term-terminal-state 3))
			 ((eq char ?D) ;; scroll forward
			  (term-handle-deferred-scroll)
			  (term-down 1 t)
			  (setq term-terminal-state 0))
			 ;; ((eq char ?E) ;; (terminfo: nw), not used for
			 ;; 	       ;; now, but this is a working
			 ;; 	       ;; implementation
			 ;;  (term-down 1)
			 ;;  (term-goto term-current-row 0)
			 ;;  (setq term-terminal-state 0))
			 ((eq char ?M) ;; scroll reversed (terminfo: ri)
			  (if (or (< (term-current-row) term-scroll-start)
				  (>= (1- (term-current-row))
				      term-scroll-start))
			      ;; Scrolling up will not move outside
			      ;; the scroll region.
			      (term-down -1)
			    ;; Scrolling the scroll region is needed.
			    (term-down -1 t))
			  (setq term-terminal-state 0))
			 ((eq char ?7) ;; Save cursor (terminfo: sc)
			  (term-handle-deferred-scroll)
			  (setq term-saved-cursor
				(list (term-current-row)
				      (term-horizontal-column)
				      term-ansi-current-bg-color
				      term-ansi-current-bold
				      term-ansi-current-color
				      term-ansi-current-invisible
				      term-ansi-current-reverse
				      term-ansi-current-underline
				      term-current-face)
				)
			  (setq term-terminal-state 0))
			 ((eq char ?8) ;; Restore cursor (terminfo: rc)
			  (when term-saved-cursor
			    (term-goto (nth 0 term-saved-cursor)
				       (nth 1 term-saved-cursor))
			    (setq term-ansi-current-bg-color
				  (nth 2 term-saved-cursor)
				  term-ansi-current-bold
				  (nth 3 term-saved-cursor)
				  term-ansi-current-color
				  (nth 4 term-saved-cursor)
				  term-ansi-current-invisible
				  (nth 5 term-saved-cursor)
				  term-ansi-current-reverse
				  (nth 6 term-saved-cursor)
				  term-ansi-current-underline
				  (nth 7 term-saved-cursor)
				  term-current-face
				  (nth 8 term-saved-cursor)))
			  (setq term-terminal-state 0))
			 ((eq char ?c) ;; \Ec - Reset (terminfo: rs1)
			  ;; This is used by the "clear" program.
			  (setq term-terminal-state 0)
			  (term-reset-terminal))
			 ;; The \E#8 reset sequence for xterm. We
			 ;; probably don't need to handle it, but this
			 ;; is the code to parse it.
			 ;; ((eq char ?#)
			 ;;  (when (eq (aref str (1+ i)) ?8)
			 ;;    (setq i (1+ i))
			 ;;    (setq term-scroll-start 0)
			 ;;    (setq term-scroll-end term-height)
			 ;;    (setq term-terminal-state 0)))
			 ((setq term-terminal-state 0))))
		  ((eq term-terminal-state 3) ; Seen Esc [
		   (cond ((and (>= char ?0) (<= char ?9))
			  (setq term-terminal-parameter
				(+ (* 10 term-terminal-parameter) (- char ?0))))
			 ((eq char ?\;)
                          ;; Some modifications to cope with multiple
                          ;; settings like ^[[01;32;43m -mm
			  (setq term-terminal-more-parameters 1)
			  (setq term-terminal-previous-parameter-4
				term-terminal-previous-parameter-3)
			  (setq term-terminal-previous-parameter-3
				term-terminal-previous-parameter-2)
			  (setq term-terminal-previous-parameter-2
				term-terminal-previous-parameter)
			  (setq term-terminal-previous-parameter
				term-terminal-parameter)
			  (setq term-terminal-parameter 0))
			 ((eq char ??)) ; Ignore ?
			 (t
			  (term-handle-ansi-escape proc char)
			  (setq term-terminal-more-parameters 0)
			  (setq term-terminal-previous-parameter-4 -1)
			  (setq term-terminal-previous-parameter-3 -1)
			  (setq term-terminal-previous-parameter-2 -1)
			  (setq term-terminal-previous-parameter -1)
			  (setq term-terminal-state 0)))))
	    (when (term-handling-pager)
	      ;; Finish stuff to get ready to handle PAGER.
	      (if (> (% (current-column) term-width) 0)
		  (setq term-terminal-parameter
			(substring str i))
		;; We're at column 0.  Goto end of buffer; to compensate,
		;; prepend a ?\r for later.  This looks more consistent.
		(if (zerop i)
		    (setq term-terminal-parameter
			  (concat "\r" (substring str i)))
		  (setq term-terminal-parameter (substring str (1- i)))
		  (aset term-terminal-parameter 0 ?\r))
		(goto-char (point-max)))
	      (setq term-terminal-state 4)
	      (make-local-variable 'term-pager-old-filter)
	      (setq term-pager-old-filter (process-filter proc))
	      (set-process-filter proc term-pager-filter)
	      (setq i str-length))
	    (setq i (1+ i))))

	(when (>= (term-current-row) term-height)
	  (term-handle-deferred-scroll))

	(set-marker (process-mark proc) (point))
	(when save-point
	  (goto-char save-point)
	  (set-marker save-point nil))

	;; Check for a pending filename-and-line number to display.
	;; We do this before scrolling, because we might create a new window.
	(when (and term-pending-frame
		   (eq (window-buffer selected) (current-buffer)))
	  (term-display-line (car term-pending-frame)
			     (cdr term-pending-frame))
          (setq term-pending-frame nil))

	;; Scroll each window displaying the buffer but (by default)
	;; only if the point matches the process-mark we started with.
	(setq win selected)
	;; Avoid infinite loop in strange case where minibuffer window
	;; is selected but not active.
	(while (window-minibuffer-p win)
	  (setq win (next-window win nil t)))
	(setq last-win win)
	(while (progn
		 (setq win (next-window win nil t))
		 (when (eq (window-buffer win) (process-buffer proc))
		   (let ((scroll term-scroll-to-bottom-on-output))
		     (select-window win)
		     (when (or (= (point) save-marker)
			       (eq scroll t) (eq scroll 'all)
			       ;; Maybe user wants point to jump to the end.
			       (and (eq selected win)
				    (or (eq scroll 'this) (not save-point)))
			       (and (eq scroll 'others)
				    (not (eq selected win))))
		       (goto-char term-home-marker)
		       (recenter 0)
		       (goto-char (process-mark proc))
		       (if (not (pos-visible-in-window-p (point) win))
			   (recenter -1)))
		     ;; Optionally scroll so that the text
		     ;; ends at the bottom of the window.
		     (when (and term-scroll-show-maximum-output
				(>= (point) (process-mark proc)))
		       (save-excursion
			 (goto-char (point-max))
			 (recenter -1)))))
		 (not (eq win last-win))))

        ;; Stolen from comint.el and adapted -mm
	(when (> term-buffer-maximum-size 0)
	  (save-excursion
	    (goto-char (process-mark (get-buffer-process (current-buffer))))
	    (forward-line (- term-buffer-maximum-size))
	    (beginning-of-line)
	    (delete-region (point-min) (point))))
	(set-marker save-marker nil)))
    ;; This might be expensive, but we need it to handle something
    ;; like `sleep 5 | less -c' in more-or-less real time.
    (when (get-buffer-window (current-buffer))
      (redisplay))))  

(defun term-emulate-terminal (proc str)
    (wterm-emulate-terminal proc str))

(defun wterm-send-return ()
  (interactive)
  (term-send-raw-string "\r\n"))

(defun wterm-send-up    () (interactive) (term-send-raw-string "\eOA"))
(defun wterm-send-down  () (interactive) (term-send-raw-string "\eOB"))
(defun wterm-send-right () (interactive) (term-send-raw-string "\eOC"))
(defun wterm-send-left  () (interactive) (term-send-raw-string "\eOD"))
(defun wterm-send-home  () (interactive) (term-send-raw-string "\e[1~"))
(defun wterm-send-insert() (interactive) (term-send-raw-string "\e[2~"))
(defun wterm-send-end   () (interactive) (term-send-raw-string "\e[4~"))
(defun wterm-send-prior () (interactive) (term-send-raw-string "\e[5~"))
(defun wterm-send-next  () (interactive) (term-send-raw-string "\e[6~"))
(defun wterm-send-del   () (interactive) (term-send-raw-string "?\x7f"))
(defun wterm-send-backspace  () (interactive) (term-send-raw-string "\b"))

(defun wterm-adjust-raw-map (map)
    (if wterm-disable-flag
        (progn
        (define-key map (kbd "<return>") 'wterm-send-raw)
        (define-key map [up] 'term-send-up)
        (define-key map [down] 'term-send-down)
        (define-key map [right] 'term-send-right)
        (define-key map [left] 'term-send-left)
        (define-key map [delete] 'term-send-del)
        (define-key map [deletechar] 'term-send-del)
        (define-key map [backspace] 'term-send-backspace)
        (define-key map [home] 'term-send-home)
        (define-key map [end] 'term-send-end)
        (define-key map [insert] 'term-send-insert)
        (define-key map [S-prior] 'scroll-down)
        (define-key map [S-next] 'scroll-up)
        (define-key map [prior] 'term-send-prior)
        (define-key map [next] 'term-send-next))

        (progn
        (define-key map (kbd "<return>") 'wterm-send-return)
        (define-key map [up] 'wterm-send-up)
        (define-key map [down] 'wterm-send-down)
        (define-key map [right] 'wterm-send-right)
        (define-key map [left] 'wterm-send-left)
        (define-key map [delete] 'wterm-send-del)
        (define-key map [deletechar] 'wterm-send-del)
        (define-key map [backspace] 'wterm-send-backspace)
        (define-key map [home] 'wterm-send-home)
        (define-key map [end] 'wterm-send-end)
        (define-key map [insert] 'wterm-send-insert)
        (define-key map [S-prior] 'wscroll-down)
        (define-key map [S-next] 'wscroll-up)
        (define-key map [prior] 'wterm-send-prior)
        (define-key map [next] 'wterm-send-next))))
  
(defun term-char-mode ()
  "Switch to char (\"raw\") sub-mode of term mode.
Each character you type is sent directly to the inferior without
intervention from Emacs, except for the escape character (usually C-c)."
  (interactive)
  ;; FIXME: Emit message? Cfr ilisp-raw-message
  (when (term-in-line-mode)
    (setq term-old-mode-map (current-local-map))
    (wterm-adjust-raw-map term-raw-map)
    (use-local-map term-raw-map)
    (easy-menu-add term-terminal-menu)
    (easy-menu-add term-signals-menu)

    ;; Send existing partial line to inferior (without newline).
    (let ((pmark (process-mark (get-buffer-process (current-buffer))))
	  (save-input-sender term-input-sender))
      (when (> (point) pmark)
	(unwind-protect
	    (progn
	      (setq term-input-sender
		    (symbol-function 'term-send-string))
	      (end-of-line)
	      (term-send-input))
	  (setq term-input-sender save-input-sender))))
    (term-update-mode-line)))  
  
(defun term-simple-send (proc string)
  "Default function for sending to PROC input STRING.
This just sends STRING plus a newline.  To override this,
set the hook `term-input-sender'."
  (term-send-string proc string)
  (term-send-string proc (if wterm-disable-flag "\n" "\r\n")))

;; This auxiliary function cranks up the process for term-exec in
;; the appropriate environment.

(defun term-exec-1 (name buffer command switches)
  ;; We need to do an extra (fork-less) exec to run stty.
  ;; (This would not be needed if we had suitable Emacs primitives.)
  ;; The 'if ...; then shift; fi' hack is because Bourne shell
  ;; loses one arg when called with -c, and newer shells (bash,  ksh) don't.
  ;; Thus we add an extra dummy argument "..", and then remove it.
  (let ((process-environment
	 (nconc
	  (list
	   (format "TERM=%s" term-term-name)
	   (format "TERMINFO=%s" data-directory)
	   (format term-termcap-format "TERMCAP="
		   term-term-name term-height term-width)

	   ;; This is for backwards compatibility with Bash 4.3 and earlier.
	   ;; Remove this hack once Bash 4.4-or-later is common, because
	   ;; it breaks './configure' of some packages that expect it to
	   ;; say where to find EMACS.
	   (format "EMACS=%s (term:%s)" emacs-version term-protocol-version)

	   (format "INSIDE_EMACS=%s,term:%s" emacs-version term-protocol-version)
	   (format "LINES=%d" term-height)
	   (format "COLUMNS=%d" term-width))
	  process-environment))
	(process-connection-type t)
	;; We should suppress conversion of end-of-line format.
	(inhibit-eol-conversion t)
	;; The process's output contains not just chars but also binary
	;; escape codes, so we need to see the raw output.  We will have to
	;; do the decoding by hand on the parts that are made of chars.
	(coding-system-for-read 'binary))
    (if wterm-disable-flag
        (apply 'start-process name buffer
           "/bin/sh" "-c"
           (format "stty -nl echo rows %d columns %d sane 2>/dev/null;\
    if [ $1 = .. ]; then shift; fi; exec \"$@\""
               term-height term-width)
           ".."
           command switches)
         (apply 'start-process name buffer
           "cmd" "/c"
           (format "mode %d,%d && %s"
               term-width term-height command)
            switches))))

;;;###autoload
(defun term (program)
  "Start a terminal-emulator in a new buffer.
The buffer is in Term mode; see `term-mode' for the
commands to use in that buffer.

\\<term-raw-map>Type \\[switch-to-buffer] to switch to another buffer."
  (interactive (list (read-from-minibuffer "Run program: "
                           (or explicit-shell-file-name
                           (unless wterm-disable-flag "powershell")
					       (getenv "ESHELL")
					       (getenv "SHELL")
					       "/bin/sh"))))
  (set-buffer (make-term "terminal" program))
  (term-mode)
  (term-char-mode)
  (switch-to-buffer "*terminal*"))

(provide 'wterm)
;;; wterm.el ends here
