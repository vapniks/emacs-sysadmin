;;; sysadmin.el --- Linux sysadmin helpers

;; Filename: sysadmin.el
;; Description: Linux sysadmin helpers
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2017, Joe Bloggs, all rites reversed.
;; Created: 2017-01-21 02:59:08
;; Version: 0.1
;; Last-Updated: 2017-01-21 02:59:08
;;           By: Joe Bloggs
;;     Update #: 1
;; URL: https://github.com/vapniks/sysadmin
;; Keywords: hardware
;; Compatibility: GNU Emacs 24.5.1
;; Package-Requires:  
;;
;; Features that might be required by this library:
;;
;; 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 1ArFina3Mi8UDghjarGqATeBgXRDWrsmzo
;;
;; Linux sysadmin helpers
;; 
;;;;;;;;

;;; Commands:
;;

;;
;; All of the above can be customized by:
;;      M-x customize-group RET sysadmin RET
;;

;;; Installation:
;;
;; Put sysadmin.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'sysadmin)

;;; History:

;;; Require


;;; Code:

;; REMEMBER TODO ;;;###autoload's
(defgroup sysadmin nil
  "User options for sysadmin")

(defcustom sysadmin-commands nil
  "List of commands and associated elisp code and notes.
Each entry consists of:
 1) A name
 2) A command
 3) Elisp code to be run in the buffer containing the command output
 4) Notes explaining the output"
  :group 'sysadmin
  :type '(alist :key-type (string :tag "Name")
		:value-type (list (string :tag "Short description")
				  (choice :tag "Contents"
					  (cons :tag "Shell command"
						(const cmd)
						(string :tag "Shell command"))
					  (cons :tag "File"
						(const file)
						(file :must-match t :tag "File")))
				  (repeat :tag "Code to eval when buffer is created"
					  (sexp :tag "Sexp"))
				  (choice :tag "Notes"
					  (cons :tag "Text"
						(const text)
						(string :tag "Text"))
					  (cons :tag "File"
						(const file)
						(file :must-match t :tag "File"))
					  (repeat :tag "Conditional notes"
						  (cons (sexp :tag "Condition")
							(choice :tag "Note"
								(cons :tag "Text"
								      (const text)
								      (string :tag "Text"))
								(cons :tag "File"
								      (const file)
								      (file :must-match t :tag "File")))))))))

(defcustom sysadmin-cmd-groups nil
  "Lists of groups of names from `sysadmin-commands'.
The first element is a name for the group, and subsequent elements are names from `sysadmin-commands',
or other group names."
  :group 'sysadmin
  :type '(alist :key-type (string :tag "Group name")
		:value-type (repeat (string :tag "Name"))))

(cl-defun sysadmin-handle-shell-command (command output-buffer
						 &optional directory
						 (sentinel 'shell-command-sentinel))
  "Like `shell-command' for Tramp files."
  (let* ((directory (or directory default-directory))
	 (args (if (and directory
			(string-match (nth 0 tramp-file-name-structure) directory))
		   (append
		    (cons
		     (tramp-get-method-parameter
		      (tramp-file-name-method
		       (tramp-dissect-file-name directory))
		      'tramp-remote-shell)
		     (tramp-get-method-parameter
		      (tramp-file-name-method
		       (tramp-dissect-file-name directory))
		      'tramp-remote-shell-args))
		    (list command))
		 (list shell-file-name shell-command-switch command)))
	 (output-buffer
	  (cond
	   ((bufferp output-buffer) output-buffer)
	   ((stringp output-buffer) (get-buffer-create output-buffer))
	   (t (error "Invalid value for `output-buffer' arg"))))
	 (p (get-buffer-process output-buffer)))
    ;; Check whether there is another process running. 
    (when p
      (if (yes-or-no-p (concat "A command is already running in buffer " output-buffer ".  Kill it? "))
	  (ignore-errors (kill-process p))
	(tramp-user-error p "Shell command in progress")))
    (with-current-buffer output-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq default-directory directory)
      (prog1
	  ;; Run the process.
	  (setq p (apply 'start-file-process "*Async Shell*" output-buffer args))
	;; Display output.
	(setq mode-line-process '(":%s"))
	(set-process-sentinel p sentinel)))))

;; TODO: find out why it doesn't always return all output for remote hosts
(cl-defun sysadmin-cmd (name &optional switchbufp (directory default-directory))
  "Apply the command associated with NAME in `sysadmin-commands' and collect the output in a buffer.
Return that buffer."
  (interactive (list (ido-completing-read "Name: " (mapcar 'car sysadmin-commands))
		     t
		     (if current-prefix-arg
			 (ido-read-directory-name "Directory: ")
		       default-directory)))
  (cl-destructuring-bind (name description contents code notes)
      (assoc name sysadmin-commands)
    (let* ((host (if (and directory
			  (string-match (nth 0 tramp-file-name-structure) directory))
		     (elt (tramp-dissect-file-name directory) 2)
		   "localhost"))
	   (buffer (get-buffer-create (concat name "@" host)))
	   (fileorcmd (cdr contents))
	   (sentinel `(lambda (p s) (if (memq (process-status p) '(exit s))
					,@code))))
      (with-current-buffer buffer
	(setq buffer-read-only nil)
	(erase-buffer)
	(setq header-line-format (concat "\"" fileorcmd "\"" " @ " host " : " description)))
      (cl-case (car contents)
	(file (let* ((filename fileorcmd))
		(if (file-readable-p filename)
		    (with-current-buffer buffer
		      (insert-file-contents filename)))))
	(cmd (let ((command fileorcmd)
		   (process-connection-type t))
	       (with-current-buffer buffer
		 (sysadmin-handle-shell-command command buffer directory sentinel)))))
      (if switchbufp (switch-to-buffer buffer))
      buffer)))


(provide 'sysadmin)

;; (org-readme-sync)
;; (magit-push)

;;; sysadmin.el ends here
