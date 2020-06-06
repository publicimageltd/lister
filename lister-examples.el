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
;; Test


(defun lister--marker-list ()
  (interactive)
  (message "Marker list: %s"
	   (string-join
	    (mapcar (lambda (m)
		      (format "%d" (marker-position m)))
		    (lister-viewport-marker-list vp))
	    " ")))

(defun lister-highlight-marker (viewport)
  (with-current-buffer (lister-viewport-buffer viewport)
    (seq-doseq (m (lister-viewport-marker-list viewport))
      (overlay-put (make-overlay m (1+ m))
		   'face
		   '(:background "yellow")))))

(defun lister-remove-overlays (viewport)
  (with-current-buffer (lister-viewport-buffer viewport)
    (remove-overlays)))

(defun lister-blink-overlays (viewport)
  (switch-to-buffer (lister-viewport-buffer viewport))
  (lister-highlight-marker viewport)
  (redisplay)
  (sleep-for 0.5)
  (lister-remove-overlays viewport))

(defun lister-test-buffer ()
  "Return test buffer."
  (get-buffer-create "*LISTER*"))

(defun lister-mapper (data)
  (list
   "Zeile1"
   (format "%s" data)
   "Zeile 2"))

(defun lister-interactive-test ()
  (interactive)
  (let* ((lister-buf (lister-test-buffer))
	 (viewport (lister-setup lister-buf
				#'lister-mapper
				'("A" "B" "C"))))
    (setq vp viewport)
    (with-current-buffer lister-buf
      (setq lister-local-mapper '(lambda (d) "TEST")))
    (lister-set-footer lister-buf "FOOTER")
    (lister-set-header lister-buf "HEADER")
    (switch-to-buffer lister-buf)
    (lister-blink-overlays viewport)))

(provide 'lister-examples)
;;; lister-examples.el ends here
