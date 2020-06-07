;;; lister-examples.el --- examples of using the lister interface  -*- lexical-binding: t; -*-

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
(require 'lister)

;; -----------------------------------------------------------

(defun lister-test-buffer ()
  "Return test buffer."
  (get-buffer-create "*LISTER*"))

(defun lister-mapper (data)
  (list
   "First Row"
   (format "Data: '%s'" data)
   "Third Row"))

(defun lister-item-message ()
  (message "Stored data: %s."
	   (lister-get-data (current-buffer) :point)))

(defun lister-interactive-test ()
  (interactive)
  (message "Neuer Start von `lister-interactive-test'")
  (let* ((lister-buf (lister-setup
		      (lister-test-buffer)
		      #'lister-mapper
		      '("A" "B" "C")
		      "HEADER"
		      "FOOTER")))
    (lister-add-enter-callback lister-buf #'lister-item-message)
    (switch-to-buffer lister-buf)
    (lister-highlight-mode)))

(provide 'lister-examples)
;;; lister-examples.el ends here
