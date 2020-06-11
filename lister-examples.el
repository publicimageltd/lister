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
(load-file "lister.el")

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

(defun lister-filter-by-a (data)
  (apply-partially #'string-match "\\`A"))

(defun lister-filter-by-b (data)
  (apply-partially #'string-match "\\`B"))

(defun lister-key-filter-by-a ()
  (interactive)
  (lister-set-filter (current-buffer) #'lister-filter-by-a))

(defun lister-key-filter-by-b ()
  (interactive)
  (lister-set-filter (current-buffer) #'lister-filter-by-b))

(defun lister-key-toggle-highlight-mode ()
  (interactive)
  (call-interactively 'lister-highlight-mode))

(defun lister-key-highlight-intangible ()
  (interactive)
  (save-excursion
    (let* ((inhibit-read-only t)
	   (cursor-sensor-inhibit t))
      (goto-char (point-min))
      (while (not (eobp))
	(let ((plist (text-properties-at (point)))
	      (next-change (or (next-property-change (point) (current-buffer))
			       (point-max))))
	  (when (plist-get plist 'cursor-intangible)
	    (put-text-property (1+ (point)) next-change
			       'face 'highlight))
	  (goto-char next-change))))))
    
(defun lister-interactive-test ()
  (interactive)
  (message "Neuer Start von `lister-interactive-test'")
  (let* ((lister-buf (lister-setup
		      (lister-test-buffer)
		      #'lister-mapper
		      '("AA" "AB" "BA" nil "BC")
		      "HEADER"
		      "FOOTER")))
    (lister-add-enter-callback lister-buf #'lister-item-message)
    (define-key lister-mode-map "a" #'lister-key-filter-by-a)
    (define-key lister-mode-map "b" #'lister-key-filter-by-b)
    (define-key lister-mode-map "h" #'lister-key-toggle-highlight-mode)
    (define-key lister-mode-map "i" #'lister-key-highlight-intangible)
    (switch-to-buffer lister-buf)
;;    (lister-highlight-mode)
    ))

;; -----------------------------------------------------------
;; Narrowing

(defun lister-narrow-test ()
  (interactive)
  (setq lister-narrow-input nil) ;; muss in lister-setup rein
  (let* ((files     (seq-filter
		     (apply-partially #'string-match "\\`[^.#%]")
		     (directory-files user-emacs-directory)))
	 (lister-buf (lister-setup
		      (lister-test-buffer)
		      #'lister-narrow-mapper
		      files
		      (format "%s:" user-emacs-directory))))
    (switch-to-buffer lister-buf)
    (lister-highlight-mode)
    (lister-narrow-with-minibuffer files)
    ))

;; TODO Es sollte eine globale Variable "lister-narrow-minibuffer-state" geben;
;; eine PLIST.

;; TODO Für async einen Timer vor dem update einbauen (siehe ivy--quehe-exhibit)

;; see ivy--minibuffer-setup
(defun lister-narrow-interactive-minibuffer-setup ()
  (add-hook 'post-command-hook #'lister-narrow-update nil t))

(defun lister-narrow-string-empty-p (s)
  "Return t if S is neither nil nor \"\"."
  (or (null s)
      (string-empty-p s)))

(defun lister-narrow-highlight-regexp (s regexp face-or-plist)
  "Highlight first match of REGEXP in S with FACE-OR-PLIST."
  (with-temp-buffer
    (insert  s)
    (goto-char (point-min))
    (ignore-errors
      (when (re-search-forward regexp (point-max) t)
	(lister-add-face-property (match-beginning 0) (match-end 0) face-or-plist)))
    (buffer-string)))

(defun lister-narrow-mapper (data)
  "Print DATA as a string and highlight match."
  (if (lister-narrow-string-empty-p lister-narrow-input)
      data
    (lister-narrow-highlight-regexp data
				    lister-narrow-input
				    'org-todo)))

(defun lister-narrow-propertize-minibuffer-prompt (face-or-plist &optional de-propertize)
  (with-current-buffer (window-buffer (minibuffer-window))
    (let* ((inhibit-field-text-motion t)
	   (inhibit-read-only t)
	   (fn (if de-propertize 'lister-remove-face-property 'lister-add-face-property)))
      (funcall fn (line-beginning-position) (minibuffer-prompt-end) face-or-plist))))

(defun lister-narrow-match-p (regexp data) 
  "Check if DATA is matched by REGEXP."
  (condition-case err
      (and (string-match-p regexp data)
	   (lister-narrow-propertize-minibuffer-prompt 'org-todo t)
	   t)
    (error
     (lister-narrow-propertize-minibuffer-prompt 'org-todo)
     data)))

(defun lister-narrow-update ()
  (setq lister-narrow-input (minibuffer-contents-no-properties))
  (lister-set-list lister-narrow-buffer
		   (seq-filter (apply-partially
				#'lister-narrow-match-p
				lister-narrow-input)
			       lister-narrow-list)))

(defun lister-narrow-with-minibuffer (datalist)
  (interactive)
  (let* ((calling-buffer (window-buffer (minibuffer-selected-window))))
    (unless (lister-buffer-p calling-buffer)
      (user-error "This function has to be called from within a lister buffer."))
    (setq lister-narrow-buffer calling-buffer)
    (setq lister-narrow-list   datalist)
    (setq lister-narrow-input  nil)
    (minibuffer-with-setup-hook
	#'lister-narrow-interactive-minibuffer-setup
      (read-from-minibuffer "Test: "))))

(provide 'lister-examples)
;;; lister-examples.el ends here
