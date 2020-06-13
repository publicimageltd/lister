;;; lister-highlight.el --- add highlighting to a lister list  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  

;; Author:  <joerg@joergvolbers.de>
;; Keywords: 

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

;; 

;;; Code:

(require 'lister "lister.el")

;; * Highlight face

(defvar lister-highlight-face-or-property
  '(:foreground "yellow")
  "Text property or name of face to add when highlighting an
  item.")

;; * Callbacks which do the highlighting

(defun lister-highlight-item ()
  (let* ((pos    (point))
	 (end    (lister-end-of-lines (current-buffer) pos)))
    (lister-add-face-property pos end
			      lister-highlight-face-or-property)))

(defun lister-unhighlight-item ()
  (let* ((pos    (point))
	 (end    (lister-end-of-lines (current-buffer) pos)))
    (lister-remove-face-property pos end
				 lister-highlight-face-or-property)))

;; * Define the mode

(define-minor-mode lister-highlight-mode
  "Toggle highlighting of the selected lister item."
  :lighter ""
  :group 'lister
  (unless (lister-buffer-p (current-buffer))
    (user-error "This minor mode can only be used in a properly set up lister buffer"))
  (if lister-highlight-mode
      ;; enable:
      (progn
	(add-hook 'lister-enter-item-hook #'lister-highlight-item nil t)
	(add-hook 'lister-leave-item-hook #'lister-unhighlight-item nil t)
	(when lister-local-marker-list
	  (let* ((previous-point (point)))
	    (lister-sensor-function (selected-window) previous-point 'entered))))
    ;; disable:
    (progn
      (when lister-local-marker-list
	(let* ((previous-point (point)))
	  (lister-sensor-function (selected-window) previous-point 'left)))
      (remove-hook 'lister-enter-item-hook #'lister-highlight-item t)
      (remove-hook 'lister-leave-item-hook #'lister-unhighlight-item t))))



(provide 'lister-highlight)
;;; lister-highlight.el ends here
