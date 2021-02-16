;;; lister-interactive-playground.el --- test lister functions interactively  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021

;; Author:  <joerg@joergvolbers.de>
;; Keywords: org-roam, lister, convenience

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

;; Build up a buffer to interactively test lister.

;;; Code:

(require 'lister)
(require 'lister-highlight)

(defvar lister-interactive-buf nil
  "Buffer for interactive testing of the lister package.")

(defun lister-interactive-mapper (data)
  "Mapper for a lister list."
  (cl-typecase data
    (string (list data))
    (integer (list (format "Integer: %d" data)))
    (t (format "%s: %s" (type-of data) data))))

(defvar lister-interactive-list
  '("One" "Two" "Three" "Four" "Five" "Six" "Seven" "Eight" "Nine"
    10 11 12 13 14 15 16 17 18 19 20))

(defun lister-interactive-buffer ()
  "Return a buffer object set up as a alist."
  (if (and lister-interactive-buf
	   (buffer-live-p lister-interactive-buf))
      lister-interactive-buf
    (let* ((buf (generate-new-buffer "Lister Interactive Test")))
      (setq lister-interactive-buf
	    (lister-setup buf #'lister-interactive-mapper
			  lister-interactive-list
			  (format-time-string "Test buffer created at %H:%R")
			  (format "End, Finito, This is no Item anymore!")))
      (with-current-buffer lister-interactive-buf
	(lister-highlight-mode)
	(setq lister-local-action #'lister-interactive-action))
      buf)))

(defun lister-interactive-action (data)
  "Do something with the item at point."
  (interactive)
  (ignore data)
  (message "Nothing implemented to do."))


(defun lister-interactive-change-level (lister-buf position-or-symbol delta)
  "Change level of item at POS-OR-MARKER by adding DELTA."
  (let* ((new-level (+ delta (lister-level-at lister-buf position-or-symbol))))
    (lister-replace lister-buf position-or-symbol
		    (lister-get-data lister-buf position-or-symbol)
		    new-level)))

(defun lister-interactive-inc-level (lister-buf position-or-symbol)
  "Increase level of item at point."
  (interactive (list (current-buffer) :point))
  (lister-interactive-change-level lister-buf position-or-symbol +1))

(defun lister-interactive-dec-level (lister-buf position-or-symbol)
  "Decrease level of item at point."
  (interactive (list (current-buffer) :point))
  (lister-interactive-change-level lister-buf position-or-symbol -1))

;;;###autoload
(defun lister-interactive-test (&optional new)
  "Open interactive test buffer for the lister major mode."
  (interactive "P")
  (when (and new lister-interactive-buf)
    (kill-buffer lister-interactive-buf))
  (switch-to-buffer (lister-interactive-buffer)))

(provide 'lister-interactive-playground)
;;; lister-interactive-playground.el ends here
