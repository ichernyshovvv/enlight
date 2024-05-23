;;; enlight.el --- Vertical menus for `enlight'. -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ilya Chernyshov

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

;; Vertical menus for `enlight'.

;; (enlight-menu
;;  '(("Org Mode"
;;     ("Org-Agenda (current day)" (org-agenda nil "a") "a"))
;;    ("Other"
;;     ("Projects" project-switch-project "p"))))

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'text-property-search)

(defvar enlight-menu-count 0)

(defgroup enlight-menu nil
  "Vertical menus for `enlight'."
  :group 'applications
  :prefix "enlight-menu-")

(defcustom enlight-menu-right-margin 5
  "Right margin applied after the items column, in number of characters."
  :type 'integer)

(defface enlight-menu-selected-face
  '((t (:underline t)))
  "Face used for currently selected item.")

(defface enlight-menu-section
  '((t (:inherit font-lock-keyword-face)))
  "Face used for sections.")

(defface enlight-menu-key
  '((t (:inherit font-lock-constant-face)))
  "Face used for key strings.")

(defun enlight--normalize-command (command)
  "Normalize COMMAND."
  `(lambda (&optional button)
     (interactive)
     ,(if (commandp command)
	  `(call-interactively ',command)
	command)))

(defvar enlight-mode-map)
(defun enlight-menu--apply-keys (alist)
  "Set keys in `enlight-mode-map' from ALIST."
  (mapc
   (lambda (section)
     (mapc
      (pcase-lambda (`(,_ ,command ,shortcut))
	(when shortcut
	  (let ((command (enlight--normalize-command command)))
	    (keymap-set enlight-mode-map shortcut command))))
      (cdr section)))
   alist)
  (keymap-set enlight-mode-map "<remap> <next-line>" 'enlight-menu-forward-item)
  (keymap-set enlight-mode-map "<remap> <right-char>" 'enlight-forward-menu)
  (keymap-set enlight-mode-map "<remap> <previous-line>" 'enlight-menu-backward-item)
  (keymap-set enlight-mode-map "<remap> <left-char>" 'enlight-backward-menu))

(defun enlight-menu--max-item-length (alist)
  "Calculate max length of item-names in ALIST."
  (thread-last alist
	       (seq-mapcat #'cdr)
	       (seq-map (lambda (x) (length (car x))))
	       (seq-max)))

;; Copied from `s-center' from s.el
(defun enlight-center-string (len s)
  "If S is shorter than LEN, pad it with spaces so it is centered."
  (let ((extra (max 0 (- len (length s)))))
    (concat
     (make-string (ceiling extra 2) ?\s)
     s
     (make-string (floor extra 2) ?\s))))

(defun enlight-menu--insert-section (menu-id width section)
  "Insert SECTION using WIDTH in the current buffer to the menu with MENU-ID."
  (pcase-let ((`(,section-name . ,items) section))
    (insert
     (propertize (enlight-center-string width section-name)
		 'face 'enlight-menu-section)
     "\n")
    (mapc (apply-partially #'enlight-menu--insert-item menu-id width) items)))

(defun enlight-menu--insert-item (menu-id width item)
  "Insert ITEM using WIDTH in the current buffer to the menu with MENU-ID."
  (pcase-let ((`(,desc ,command ,shortcut) item))
    (insert-text-button
     desc
     'menu-id menu-id
     'face 'default
     'action (enlight--normalize-command command)
     'help-echo nil
     'cursor-face 'enlight-menu-selected-face
     'mouse-face 'enlight-menu-selected-face)
    (when shortcut
      (insert
       (make-string (- width (length desc)) ? )
       (propertize shortcut 'face 'enlight-menu-key)))
    (insert "\n")))

(defun enlight-menu-first-button ()
  "Jumpt to first button in the current buffer."
  (goto-char (point-min))
  (forward-button 1 nil nil t))

;;;###autoload
(defun enlight-menu (alist)
  "Generate a vertical menu using ALIST and return as a string.

The form of ALIST:

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
  (let ((width (+ (enlight-menu--max-item-length alist)
		  enlight-menu-right-margin)))
    (add-hook 'enlight-after-insert-hook #'enlight-menu-first-button)
    (enlight-menu--apply-keys alist)
    (with-temp-buffer
      (mapc (apply-partially #'enlight-menu--insert-section
			     (cl-incf enlight-menu-count) width)
	    alist)
      (buffer-string))))

(defun enlight-forward-menu ()
  "Go to next menu in the buffer."
  (interactive)
  (if (text-property-search-forward
       'menu-id (get-text-property (point) 'menu-id)
       (lambda (val prop) (and prop (not (eq val prop)))))
      (backward-button 1)
    (goto-char (point-min))
    (enlight-forward-menu)))

(defun enlight-backward-menu ()
  "Go to previous menu in the buffer."
  (interactive)
  (unless (text-property-search-backward
	   'menu-id (get-text-property (point) 'menu-id)
	   (lambda (val prop) (and prop (not (eq val prop)))))
    (goto-char (point-max))
    (enlight-backward-menu)))

(defun enlight-menu-forward-item ()
  "Go to next item in the current menu."
  (interactive)
  (let ((cur-menu-id (get-text-property (point) 'menu-id)))
    (while
	(progn
	  (forward-button 1 t)
	  (/= (get-text-property (point) 'menu-id)
	      cur-menu-id)))))

(defun enlight-menu-backward-item ()
  "Go to previous item in the current menu."
  (interactive)
  (let ((cur-menu-id (get-text-property (point) 'menu-id)))
    (while
	(progn
	  (backward-button 1 t)
	  (/= (get-text-property (point) 'menu-id)
	      cur-menu-id)))))

(provide 'enlight-menu)

;;; enlight-menu.el ends here
