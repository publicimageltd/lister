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

(require 'cl-lib)
(cl-eval-when (eval)
  (if (not (member default-directory load-path))
      (add-to-list 'load-path default-directory)))

(require 'lister)
(require 'lister-highlight)

;; * Use the callback

(defun lister-item-message ()
  "Display the data of the item in the echo area."
  (message "Stored data: %s."
	   (lister-get-data (current-buffer) :point)))

;; * Use the filter

(defun lister-filter-by-a (data)
  "Only allow data beginning with 'A"
  (ignore data) ;; silence byte compiler
  (string-match "\\`A" data))

(defun lister-filter-by-b (data)
  "Only allow data beginning with 'B"
  (ignore data) ;; silence byte compiler
  (string-match "\\`B" data))

(defun lister-key-negate-filter ()
  "Negate the present filter."
  (interactive)
  (lister-negate-filter (current-buffer))
  (message "Filter term is '%s'." lister-local-filter-term))

(defun lister-key-filter-by-a ()
  "Apply A-filter."
  (interactive)
  (lister-set-filter (current-buffer) #'lister-filter-by-a)
  (message "Filter 'A' set."))

(defun lister-key-filter-by-b ()
  "Apply B-filter."
  (interactive)
  (lister-set-filter (current-buffer) #'lister-filter-by-b)
  (message "Filter 'B' set."))

(defun lister-key-toggle-filter ()
  "Activate or deactivate the filter."
  (interactive)
  (if lister-local-filter-active
      (lister-deactivate-filter (current-buffer))  
    (lister-activate-filter (current-buffer)))
  (if lister-local-filter-active
      (message "Filter is active. Filter term is '%s'." lister-local-filter-term)
    (message "Filter is not active.")))

;; * Play around with the items

(defun lister-key-insert-item (data &optional level)
  "Insert DATA at POINT, indenting it at LEVEL."
  (interactive (list (read-string "Data: ")
		     (when current-prefix-arg
		       (read-string
			(format "Level at point is %s, enter new level for insertion (nil, integer or :previous, :current)\n : "
				(get-text-property (point) 'level))))))
  (let* ((level-arg (if (stringp level)
			(if (string-empty-p level)
			    nil
			  (if (string-prefix-p ":" level)
			      (intern level)
			    (string-to-number level))))))
    (lister-insert-sequence (current-buffer)
			    (point)
			    (split-string data nil t)
			    level-arg)))

(defun lister-key-delete-item ()
  "Delete item at point."
  (interactive)
  (lister-remove (current-buffer) :point))

(defun lister-key-get-data-tree ()
  "Store the data tree in the global variable 'test-data-tree'."
  (interactive)
  (setq test-data-tree (lister-get-all-data-tree (current-buffer)))
  (message "Variable `test-data-tree' set to %s." test-data-tree))

(defun lister-key-get-info ()
  "Display some information about the item at point."
  (interactive)
  (let* ((props (lister-get-props-at (current-buffer) (point)
				     'nlines 'level 'item)))
    (apply #'message "nlines: %s level: %s item: %s" props)))

;; * Use highlighting

(defun lister-key-toggle-highlight-mode ()
  "Toggle highlight mode."
  (interactive)
  (call-interactively 'lister-highlight-mode))

;; * The test "interface"



(defun lister-interactive-test ()
  (interactive)
  ;; (message "Neuer Start von `lister-interactive-test'")
  (let* ((buf (lister-setup
		      (get-buffer-create "*LISTER-TEST*")
		      #'list
		      '("AA"
			"AB"
			("EINS-UNTERITEM"
			 "ZWEI-UNTERITEM")
			"BA"
			"BC"
			("HUHU_SELTSAM_UNTER1"
			 "HUHU_SELTSAM_UNTER2"))
		      "HEADER"
		      "FOOTER")))
    (lister-add-enter-callback buf #'lister-item-message)
    (define-key lister-mode-map "a" #'lister-key-filter-by-a)
    (define-key lister-mode-map "b" #'lister-key-filter-by-b)
    (define-key lister-mode-map "n" #'lister-key-negate-filter)
    (define-key lister-mode-map "h" #'lister-key-toggle-highlight-mode)
    (define-key lister-mode-map "f" #'lister-key-toggle-filter)
    (define-key lister-mode-map "+" #'lister-key-insert-item)
    (define-key lister-mode-map "-" #'lister-key-delete-item)
    (define-key lister-mode-map "t" #'lister-key-get-data-tree)
    (define-key lister-mode-map "i" #'lister-key-get-info)
    (switch-to-buffer buf)))

(provide 'lister-examples)
;;; lister-examples.el ends here
