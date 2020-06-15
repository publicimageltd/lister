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
  (message "Stored data: %s."
	   (lister-get-data (current-buffer) :point)))

;; * Use the filter

(defun lister-filter-by-a (data)
  (ignore data) ;; silence byte compiler
  (string-match "\\`A" data))

(defun lister-filter-by-b (data)
  (ignore data) ;; silence byte compiler
  (string-match "\\`B" data))

(defun lister-key-filter-by-a ()
  (interactive)
  (lister-set-filter (current-buffer) #'lister-filter-by-a)
  (message "Filter 'A' set."))

(defun lister-key-filter-by-b ()
  (interactive)
  (lister-set-filter (current-buffer) #'lister-filter-by-b)
  (message "Filter 'B' set."))

(defun lister-key-toggle-filter ()
  (interactive)
  (if lister-local-filter-active
      (lister-deactivate-filter (current-buffer))  
    (lister-activate-filter (current-buffer)))
  (if lister-local-filter-active
      (message "Filter is active. Filter term is '%s'." lister-local-filter-term)
  (message "Filter is not active.")))


;; * Use highlighting

(defun lister-key-toggle-highlight-mode ()
  (interactive)
  (call-interactively 'lister-highlight-mode))

;; * The test "interface"

(defun lister-interactive-test ()
  (interactive)
  ;; (message "Neuer Start von `lister-interactive-test'")
  (let* ((buf (lister-setup
		      (get-buffer-create "*LISTER-TEST*")
		      #'list
		      '("AA" "AB" "BA"
			;;nil
			"BC")
		      "HEADER"
		      "FOOTER")))
    (lister-add-enter-callback buf #'lister-item-message)
    (define-key lister-mode-map "a" #'lister-key-filter-by-a)
    (define-key lister-mode-map "b" #'lister-key-filter-by-b)
    (define-key lister-mode-map "h" #'lister-key-toggle-highlight-mode)
    (define-key lister-mode-map "f" #'lister-key-toggle-filter)
    (switch-to-buffer buf)))

(provide 'lister-examples)
;;; lister-examples.el ends here
