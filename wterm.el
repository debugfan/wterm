;;; wterm.el --- a windows console terminal mode package                     -*- lexical-binding: t; -*-

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
        ((apply 'start-process name buffer
           "/bin/sh" "-c"
           (format "stty -nl echo rows %d columns %d sane 2>/dev/null;\
    if [ $1 = .. ]; then shift; fi; exec \"$@\""
               term-height term-width)
           ".."
           command switches))
         (apply 'start-process name buffer
           "cmd" "/c"
           (format "mode %d,%d && "
               term-width term-height)
           command switches))))

;;;###autoload
(defun term (program)
  "Start a terminal-emulator in a new buffer.
The buffer is in Term mode; see `term-mode' for the
commands to use in that buffer.

\\<term-raw-map>Type \\[switch-to-buffer] to switch to another buffer."
  (interactive (list (read-from-minibuffer "Run program: "
                           (or explicit-shell-file-name
                           (unless wterm-disable-flag "cmd")
					       (getenv "ESHELL")
					       (getenv "SHELL")
					       "/bin/sh"))))
  (set-buffer (make-term "terminal" program))
  (term-mode)
  (term-char-mode)
  (switch-to-buffer "*terminal*"))

(provide 'wterm)
;;; wterm.el ends here
