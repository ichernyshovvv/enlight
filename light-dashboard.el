;;; light-dashboard.el --- Simple dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ilya Chernyshov

;; Author: Ilya Chernyshov <ichernyshovvv@gmail.com>
;; Version: 0.1-pre
;; Package-Requires: ((emacs "29.1"))
;; Keywords: startup, screen, tools, dashboard
;; URL: https://github.com/ichernyshovvv/light-dashboard

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple dashboard that could be used instead of splash screen.

;;; Code:

(require 'text-property-search)

;;;; Defcustoms

(defgroup light-dashboard nil
  "Simple dashboard."
  :group 'applications
  :prefix "light-dashboard-")

(defcustom light-dashboard-list
  '("Org Mode"
    ("Org-Agenda (current day)" (org-agenda nil "a") "a")
    "Other"
    ("Projects" project-switch-project "p"))
  "List of items and sections in the dashboard.

The value of this variable is a list.  Acceptable elements:

  STRING
  (\"Item text\" SYMBOL-OR-FORM [KEY])

STRING is a name of a section
SYMBOL-OR-FORM is form or a function symbol to execute
KEY is a string acceptable for `keymap-set'."
  :type '(repeat
	  (choice
	   (string :tag "Section name")
	   (list string (choice function list) string))))

;;;; Faces

(defface light-dashboard-selected-face
  '((t (:underline t)))
  "Face used for currently selected item.")

(defface light-dashboard-section
  '((t (:inherit font-lock-keyword-face)))
  "Face used for sections.")

(defface light-dashboard-key
  '((t (:inherit font-lock-constant-face)))
  "Face used for key strings.")

;;; Vars and functions

(defvar light-dashboard-buffer-name "*light-dashboard*")

(define-derived-mode light-dashboard-mode
  special-mode "Light Dashboard"
  (setq cursor-type nil
	goal-column 0)
  (cursor-face-highlight-mode 1))

;;;###autoload
(defun light-dashboard-open ()
  "Open `light-dashboard'."
  (interactive)
  (switch-to-buffer light-dashboard-buffer-name)
  (let ((inhibit-read-only t)
	(longest
	 (+ (apply #'max
		   (mapcar (lambda (x) (length (car-safe x)))
			   light-dashboard-list))
	    5))
	(buffer-map (make-sparse-keymap)))
    (light-dashboard-mode)
    (erase-buffer)
    (insert
     (propertize "\n"
		 'line-height (* (line-pixel-height)
				 (1- (/ (window-height) 2))))
     (mapconcat
      (lambda (item)
	(cond
	 ((listp item)
	  (let ((map (make-sparse-keymap))
		(command
		 (pcase (cadr item)
		   ((pred listp)
		    (lambda ()
		      (interactive)
		      (eval (cadr item))))
		   ((pred symbolp) (cadr item)))))
	    (keymap-set map "<mouse-1>" command)
	    (keymap-set map "RET" command)
	    (concat
	     (propertize
	      (car item)
	      'item t
	      'keymap map
	      'line-prefix
	      `(space . (:align-to (- center ,(/ longest 2))))
	      'cursor-face 'light-dashboard-selected-face
	      'mouse-face 'light-dashboard-selected-face)
	     (when (caddr item)
	       (keymap-set buffer-map (caddr item) command)
	       (concat
		(make-string (- longest (length (car item))) ? )
		(propertize
		 (caddr item)
		 'face 'light-dashboard-key))))))
	 ((stringp item)
	  (propertize
	   item
	   'line-prefix
	   `(space . (:align-to (- center ,(/ (length item) 2))))
	   'intangible t
	   'face 'light-dashboard-section))))
      light-dashboard-list
      (propertize "\n" 'intangible t)))
    (keymap-set buffer-map "g" #'light-dashboard-open)
    (use-local-map buffer-map)
    (goto-char (point-min))
    (text-property-search-forward 'item t)))

(provide 'light-dashboard)

;;; light-dashboard.el ends here
