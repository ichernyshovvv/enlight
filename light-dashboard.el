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
(require 'subr-x)
(require 'seq)

;;;; Defcustoms

(defgroup light-dashboard nil
  "Simple dashboard."
  :group 'applications
  :prefix "light-dashboard-")

;; TODO: add setter to update dashboard/keymap on change
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
  (setq goal-column 0)
  (when (fboundp #'cursor-face-highlight-mode)
    (setq cursor-type nil)
    (cursor-face-highlight-mode 1)))

;; TODO: use a macro to avoid `eval' and extra `lambda' wrapping
(defun light-dashboard--normalize-command (command)
  "Normalize COMMAND."
  (if (commandp command)
      command
    (lambda ()
      (interactive)
      (eval command))))

;; TODO: use button.el to avoid manual binding?
(defun light-dashboard--bind-map (command)
  "Return a new keymap with COMMAND bound to mouse-1 and RET."
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<mouse-1>" command)
    (keymap-set map "RET" command)
    map))

(defun light-dashboard--form-item (column-width buffer-map item)
  "Format ITEM using COLUMN-WIDTH, bind commands in BUFFER-MAP."
  (pcase-let ((`(,desc ,command ,shortcut) item))
    (let ((command (light-dashboard--normalize-command command)))
      (concat
       (propertize desc
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

(defun light-dashboard--form-section (column-width buffer-map section)
  "Format SECTION using COLUMN-WIDTH, bind commands in BUFFER-MAP."
  (pcase-let ((`(,section-name . ,items) section))
    (concat
     (propertize section-name
                 'line-prefix
                 `(space . (:align-to (- center ,(/ (length section-name) 2))))
                 'intangible t
                 'face 'light-dashboard-section)
     (propertize "\n" 'intangible t)
     (mapconcat (apply-partially #'light-dashboard--form-item column-width buffer-map)
                items
                (propertize "\n" 'intangible t)))))

(defun light-dashboard--max-item-length (alist)
  "Calculate max length of item-names in ALIST."
  (thread-last alist
	       (seq-mapcat #'cdr)
	       (seq-map (lambda (x) (length (car x))))
	       (seq-max)))

(defun light-dashboard--top-margin-height ()
  "Calculate top margin height to center dashboard properly."
  (* (line-pixel-height)
     (1- (/ (window-height) 2))))

(defun light-dashboard--form-dashboard ()
  "Format `light-dashboard-alist'."
  (let ((column-width (+ (light-dashboard--max-item-length light-dashboard-alist)
                         light-dashboard-right-margin))
        ;; TODO: use the mode keymap
        (buffer-map (make-sparse-keymap)))
    (keymap-set buffer-map "g" #'light-dashboard-open)
    (keymap-set buffer-map "q" #'quit-window)
    (use-local-map buffer-map)
    (concat (propertize "\n"
                        'line-height (light-dashboard--top-margin-height))
            (mapconcat
             (apply-partially #'light-dashboard--form-section column-width buffer-map)
             light-dashboard-alist
             (propertize "\n" 'intangible t)))))

;;;###autoload
(defun light-dashboard-open ()
  "Open `light-dashboard'."
  (interactive)
  (switch-to-buffer light-dashboard-buffer-name)
  (let ((inhibit-read-only t))
    (unless (derived-mode-p 'light-dashboard-mode)
      (light-dashboard-mode))
    (erase-buffer)
    (insert (light-dashboard--form-dashboard))
    (goto-char (point-min))
    (text-property-search-forward 'item t)))

(provide 'light-dashboard)

;;; light-dashboard.el ends here
