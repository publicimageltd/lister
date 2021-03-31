;;; lister-highlight.el --- add highlighting to a lister list  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021

;; Author:  <joerg@joergvolbers.de>
;; Keywords: hypermedia

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


;;; Code:

(require 'lister)

;; * Highlight face

(defcustom lister-highlight-face-or-property 'hl-line
  "Face or property list used for highlighting.
The value can be either the name of a face (a symbol) or a
property list with face attributes."
  :group 'lister-highlight
  :type '(choice (face :tag "Name of a face")
		 (plist :tag "Face attributes")))

;; * Callbacks which do the highlighting

(defun lister-highlight-item ()
  "Highlight the item at point.
If the item is marked, add the highlighting face to the
background, letting the mark stand out. Else, add it to the
front, letting the highlighting stand out."
  (let* ((inhibit-read-only t)
	 (pos    (point))
	 (end    (lister-end-of-lines (current-buffer) pos t)))
    (when (/= pos end)
      (lister-add-face-property  pos end
				 lister-highlight-face-or-property
				 (get-text-property pos 'mark)))))

(defun lister-unhighlight-item ()
  "Remove the highlighting of the item at point."
  (let* ((inhibit-read-only t)
	 (pos    (point))
	 (end    (lister-end-of-lines (current-buffer) pos t)))
    (when (and end
	       (/= pos end))
      (lister-remove-face-property pos end
				   lister-highlight-face-or-property))))

;; * Define the mode

(define-minor-mode lister-highlight-mode
  "Toggle automatic highlighting of the lister item at point."
  :lighter ""
  :group 'lister-highlight
  (unless (derived-mode-p 'lister-mode)
    (user-error "This minor mode is to be used in a buffer with a lister major mode"))
  (if lister-highlight-mode
      ;; enable:
      (let ((buf (current-buffer)))
	(lister-add-enter-callback buf #'lister-highlight-item)
	(lister-add-leave-callback buf #'lister-unhighlight-item)
	(lister-sensor-enter buf))
    ;; disable:
    (progn
      (lister-sensor-leave buf)
      (let ((buf (current-buffer)))
	(lister-remove-enter-callback #'lister-highlight-item)
	(lister-remove-leave-callback #'lister-unhighlight-item)))))

(provide 'lister-highlight)
;;; lister-highlight.el ends here
