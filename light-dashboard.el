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

(defcustom light-dashboard-alist
  '(("Org Mode"
     ("Org-Agenda (current day)" (org-agenda nil "a") "a"))
    ("Other"
     ("Projects" project-switch-project "p")))
  "List of items and sections in the dashboard.

The value of this variable is an alist of the form:

  ((\"Section-1\" ITEM ITEM ...)
   (\"Section-2\" ITEM ITEM ...)
   ...)

Where ITEM is of the form:

  (\"Item text\" SYMBOL-OR-FORM [KEY])

SYMBOL-OR-FORM is a form or a function symbol.  If it's a
function symbol, the function will be called when a user presses
RET or clicks on that item.

KEY is a string acceptable for `keymap-set'.  If it is specified,
the SYMBOL-OR-FORM is bound to this key in the dashboard buffer
keymap."
  :type '(alist :key-type string
		:value-type (list string (choice function list) string)))

(defcustom light-dashboard-right-margin 5
  "Right margin applied after the items column, in number of characters."
  :type 'integer)

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

(defun light-dashboard-column-width ()
  "Calculate column width for the dashboard in number of characters."
  (+ (apply #'max
	    (mapcar (lambda (x) (length (car x)))
		    (apply #'append (mapcar #'cdr light-dashboard-alist))))
     light-dashboard-right-margin))

(defun light-dashboard-form-section (column-width buffer-map section)
  (pcase-let ((`(,section-name . ,items) section))
    (concat (propertize section-name
                        'line-prefix
                        `(space . (:align-to (- center ,(/ (length section-name) 2))))
                        'intangible t
                        'face 'light-dashboard-section)
            (propertize "\n" 'intangible t)
            (mapconcat
             (apply-partially #'light-dashboard-form-item column-width buffer-map)
             items
             (propertize "\n" 'intangible t)))))

(defun light-dashboard--normalize-command (command)
  (if (commandp command)
      command
    (lambda ()
      (interactive)
      (eval command))))

(defun light-dashboard--bind-map (command)
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<mouse-1>" command)
    (keymap-set map "RET" command)
    map))

(defun light-dashboard-form-item (column-width buffer-map item)
  (pcase-let ((`(,desc ,command ,shortcut) item))
    (let ((command (light-dashboard--normalize-command command)))
      (concat (propertize desc
	                  'item t
	                  'keymap (light-dashboard--bind-map command)
	                  'line-prefix
	                  `(space . (:align-to (- center ,(/ column-width 2))))
	                  'cursor-face 'light-dashboard-selected-face
	                  'mouse-face 'light-dashboard-selected-face)
	      (when shortcut
	        (keymap-set buffer-map shortcut command)
	        (concat (make-string (- column-width (length desc)) ? )
	                (propertize shortcut 'face 'light-dashboard-key)))))))

;;;###autoload
(defun light-dashboard-open ()
  "Open `light-dashboard'."
  (interactive)
  (switch-to-buffer light-dashboard-buffer-name)
  (let ((inhibit-read-only t)
	(longest (light-dashboard-column-width))
	(buffer-map (make-sparse-keymap)))
    (unless (derived-mode-p 'light-dashboard-mode)
      (light-dashboard-mode))
    (erase-buffer)
    (insert
     (propertize "\n"
		 'line-height (* (line-pixel-height)
				 (1- (/ (window-height) 2))))
     (mapconcat
      (apply-partially #'light-dashboard-form-section
		       longest buffer-map)
      light-dashboard-alist
      (propertize "\n" 'intangible t)))
    (keymap-set buffer-map "g" #'light-dashboard-open)
    (use-local-map buffer-map)
    (goto-char (point-min))
    (text-property-search-forward 'item t)))

(provide 'light-dashboard)

;;; light-dashboard.el ends here
