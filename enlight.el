;;; enlight.el --- Highly customizable startup screen -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ilya Chernyshov

;; Author: Ilya Chernyshov <ichernyshovvv@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: startup, screen, tools, dashboard
;; URL: https://github.com/ichernyshovvv/enlight

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

;; Highly customizable startup screen.

;;; Code:

(defgroup enlight nil
  "Highly customizable startup screen."
  :group 'applications
  :prefix "enlight-")

(defcustom enlight-center-vertically t
  "Non-nil means center the buffer content vertically."
  :type 'boolean)

(defcustom enlight-center-horizontally t
  "Non-nil means center the buffer content horizontally."
  :type 'boolean)

(defcustom enlight-after-insert-hook nil
  "Hook run right after inserting of `enlight-content'."
  :type 'hook)

(defvar-keymap enlight-mode-map
  "g" #'enlight-open
  "q" #'quit-window)

(defvar enlight-height nil)
(defvar enlight-width nil)
(defvar enlight-buffer-name "*enlight*")

(defun enlight--count-lines (string)
  "Return number of lines in STRING."
  (with-temp-buffer
    (insert string)
    (count-lines (point-min) (point-max))))

(defun enlight--longest-line-length (string)
  "Return length of the longest line in STRING."
  (apply #'max (mapcar #'length (split-string string "\n"))))

(defun enlight--update (symbol value)
  "Set SYMBOL's value to VALUE.
Also update `enlight-height', `enlight-width'."
  (when enlight-center-horizontally
    (setq enlight-width (enlight--longest-line-length value)))
  (when enlight-center-vertically
    (setq enlight-height (enlight--count-lines value)))
  (set symbol value))

(defcustom enlight-content
  "You've been enlightened\nby enlight"
  "String to be inserted to `enlight' buffer."
  :type 'string
  :set #'enlight--update)

(define-derived-mode enlight-mode
  special-mode "Enlight"
  (when (fboundp #'cursor-face-highlight-mode)
    (setq cursor-type nil)
    (cursor-face-highlight-mode 1)))

(defun enlight--top-margin (height)
  "Calculate top margin to center `elight' buffer content based on HEIGHT."
  (max (/ (- (window-height
	      (get-buffer-window
	       (get-buffer-create enlight-buffer-name)))
	     height)
	  2)
       0))

;;;###autoload
(defun enlight ()
  "Return `enlight' buffer ready for display."
  (with-current-buffer (get-buffer-create enlight-buffer-name)
    (let ((inhibit-read-only t))
      (unless (derived-mode-p 'enlight-mode)
	(enlight-mode))
      (erase-buffer)
      (when enlight-center-vertically
	(insert-char ?\n (enlight--top-margin enlight-height)))
      (when enlight-center-horizontally
	(setq line-prefix
	      `(space . (:align-to (- center ,(/ enlight-width 2))))))
      (insert enlight-content)
      (goto-char (point-min))
      (run-hooks 'enlight-after-insert-hook))
    (current-buffer)))

;;;###autoload
(defun enlight-open ()
  "Open `enlight'."
  (interactive)
  (switch-to-buffer (enlight)))

(provide 'enlight)

;;; enlight.el ends here
