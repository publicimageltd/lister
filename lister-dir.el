;;; lister-dir.el --- directory browser test implementation of lister  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  

;; Author:  <joerg@joergvolbers.de>
;; Keywords: files

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

(require 'cl-lib)
(cl-eval-when (eval)
  (if (not (member default-directory load-path))
      (add-to-list 'load-path default-directory)))

(require 'lister)
(require 'lister-highlight)

;;

(defun lister-dir-mapper (file)
  (list (format 
	 (if (file-directory-p file) "%s/" "%s")
	 (file-name-nondirectory file))))

(defun lister-item-message ()
  "Display the data of the item in the echo area."
  (message "Point: %d Stored data: %s"
	   (point)
	   (lister-get-data (current-buffer) :point)))

(defun lister-dir-insert-dir (buf path pos)
  (let* ((dir (directory-files path)))
    (lister-insert-sublist-below buf pos dir)))

(defun lister-dir-action (pos)
  (if (lister-get-prop (current-buffer) pos 'expanded)
      (progn
	(lister-remove-sublist-below (current-buffer) pos)
	(lister-set-prop (current-buffer) pos 'expanded nil))
    (let* ((file (lister-get-data (current-buffer) pos)))
      (when (file-directory-p file)
	(lister-insert-sublist-below (current-buffer)
				     pos
				     (directory-files file t))
	(lister-set-prop (current-buffer) pos 'expanded t)))))

(defun lister-dir ()
  (interactive)
  (let* ((buf (lister-setup (get-buffer-create "*LISTER-DIR*")
			    #'lister-dir-mapper))
	 (path user-emacs-directory))
    (lister-add-sequence buf (directory-files path t))
    (lister-add-enter-callback buf #'lister-item-message)
    (lister-set-header buf (format "%s:" path))
    (with-current-buffer buf
      (setq lister-local-action #'lister-dir-action))
    (lister-goto buf :first)
    (switch-to-buffer buf)))

(provide 'lister-dir)
;;; lister-dir.el ends here
