;;; light-dashboard.el --- Simple dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ilya Chernyshov

;; Author: Ilya Chernyshov <ichernyshovvv@gmail.com>
;; Version: 0.1
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

(require 'subr-x)
(require 'seq)

(defgroup light-dashboard nil
  "Simple dashboard."
  :group 'applications
  :prefix "light-dashboard-")

(defun light-dashboard--normalize-command (command)
  "Normalize COMMAND."
  `(lambda (&optional button)
     (interactive)
     ,(if (commandp command)
	  `(call-interactively ',command)
	command)))

(defvar-keymap light-dashboard-mode-map)
(defvar light-dashboard-dashboard-string nil)
(defvar light-dashboard-height nil)
(defvar light-dashboard-width nil)
(defvar light-dashboard-buffer-name "*light-dashboard*")

(defun light-dashboard--max-item-length (alist)
  "Calculate max length of item-names in ALIST."
  (thread-last alist
	       (seq-mapcat #'cdr)
	       (seq-map (lambda (x) (length (car x))))
	       (seq-max)))

(defcustom light-dashboard-right-margin 5
  "Right margin applied after the items column, in number of characters."
  :type 'integer)

(defun light-dashboard--insert-section (section)
  "Insert SECTION in the current buffer."
  (pcase-let ((`(,section-name . ,items) section))
    (insert
     (propertize section-name
                 'line-prefix
                 `(space . (:align-to (- center ,(/ (length section-name) 2))))
                 'face 'light-dashboard-section)
     "\n")
    (mapc #'light-dashboard--insert-item items)))

(defun light-dashboard--insert-item (item)
  "Insert ITEM in the current buffer."
  (pcase-let ((`(,desc ,command ,shortcut) item))
    (insert-text-button
     desc
     'face 'default
     'action (light-dashboard--normalize-command command)
     'help-echo nil
     'cursor-face 'light-dashboard-selected-face
     'mouse-face 'light-dashboard-selected-face)
    (when shortcut
      (insert
       (concat (make-string (- light-dashboard-width (length desc)) ? )
	       (propertize shortcut 'face 'light-dashboard-key))))
    (insert "\n")))

(defun light-dashboard--generate-dashboard (alist)
  "Generate the dashboard using ALIST and return as a string."
  (with-temp-buffer
    (mapc #'light-dashboard--insert-section alist)
    (buffer-string)))

(defun light-dashboard--dashboard-height (alist)
  "Calculate max length of item-names in ALIST."
  (apply #'+ (mapcar #'length alist)))

(defun light-dashboard--generate-map (alist)
  "Generate the keymap for `light-dashboard-mode' from ALIST."
  (let ((map (make-sparse-keymap)))
    (mapc
     (lambda (section)
       (mapc
	(pcase-lambda (`(,_ ,command ,shortcut))
	  (let ((command (light-dashboard--normalize-command command)))
	    (when shortcut
	      (keymap-set map shortcut command))))
	(cdr section)))
     alist)
    (keymap-set map "g" #'light-dashboard-open)
    (keymap-set map "q" #'quit-window)
    (keymap-set map "<remap> <next-line>" 'forward-button)
    (keymap-set map "<remap> <right-char>" 'forward-button)
    (keymap-set map "<remap> <previous-line>" 'backward-button)
    (keymap-set map "<remap> <left-char>" 'backward-button)
    map))

(defun light-dashboard--update (symbol value)
  "Set SYMBOL's value to VALUE.
Also update `light-dashboard-mode-map', `light-dashboard-dashboard-string',
`light-dashboard-height', `light-dashboard-width'."
  (setq light-dashboard-mode-map (light-dashboard--generate-map value)
	light-dashboard-width (+ (light-dashboard--max-item-length value)
				 light-dashboard-right-margin)
	light-dashboard-height (light-dashboard--dashboard-height value)
	light-dashboard-dashboard-string (light-dashboard--generate-dashboard value))
  (set symbol value))

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
                :value-type (list string (choice function list) string))
  :set #'light-dashboard--update)

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

(define-derived-mode light-dashboard-mode
  special-mode "Light Dashboard"
  (when (fboundp #'cursor-face-highlight-mode)
    (setq cursor-type nil)
    (cursor-face-highlight-mode 1)))

(defun light-dashboard--top-margin (dashboard-height)
  "Calculate top margin height to center dashboard based on DASHBOARD-HEIGHT."
  (max (/ (- (window-height) dashboard-height) 2) 0))

;;;###autoload
(defun light-dashboard-open ()
  "Open `light-dashboard'."
  (interactive)
  (switch-to-buffer light-dashboard-buffer-name)
  (let ((inhibit-read-only t))
    (light-dashboard-mode)
    (erase-buffer)
    (insert-char ?\n (light-dashboard--top-margin light-dashboard-height))
    (setq line-prefix
	  `(space . (:align-to (- center ,(/ light-dashboard-width 2)))))
    (insert light-dashboard-dashboard-string)
    (goto-char (point-min))
    (forward-button 1)))

(provide 'light-dashboard)

;;; light-dashboard.el ends here
