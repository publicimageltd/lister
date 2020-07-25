;;; delve.el --- Delve into the depths of your org roam zettelkasten       -*- lexical-binding: t; -*-

;; Copyright (C) 2020  

;; Author:  <joerg@joergvolbers.de>
;; Keywords: hypermedia, org-roam

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

;; Delve into the depths of your zettelkasten.

;;; Code:

;; * Dependencies

;; add current buffer's directory to the load path:
(require 'cl-lib)
(cl-eval-when (eval)
  (if (not (member default-directory load-path))
      (add-to-list 'load-path default-directory)))

(setq load-prefer-newer t)
(require 'lister)
(require 'lister-highlight)
(require 'org-roam)

;; * Item data types

(defstruct (delve-generic-item (:constructor delve-make-generic-item)))

(defstruct (delve-tag (:constructor delve-make-tag)
		      (:include delve-generic-item))
  tag
  count)

(defstruct (delve-zettel (:constructor delve-make-zettel)
			 (:include delve-generic-item))
  title
  file
  tags
  mtime
  atime
  backlinks
  tolinks)

;; * Item mapper functions

(defun delve-represent-zettel (zettel)
  (list
   ;;
   (concat
    (when (delve-zettel-tags zettel)
      (concat "(" (propertize (string-join (delve-zettel-tags zettel) ", ") 'face 'org-level-1) ") "))
    (propertize (delve-zettel-title zettel) 'face 'org-document-title))
   ;;
   (concat
    (format "%s backlinks; %s links to this zettel." 
	    (delve-zettel-backlinks zettel)
	    (delve-zettel-tolinks zettel))
    (format-time-string "  Last modified: %D %T" (delve-zettel-mtime zettel)))))

(defun delve-represent-tag (tag)
  (list (concat "Tag: " (propertize (delve-tag-tag tag) 'face 'org-level-1)
		(when (delve-tag-count tag)
		  (format " (%d)" (delve-tag-count tag))))))

(defun delve-mapper (data)
  "Transform DATA into a printable list."
  (cl-case (type-of data)
      (delve-zettel (delve-represent-zettel data))
      (delve-tag    (delve-represent-tag data))
      (t      (list (format "UNKNOWN TYPE: %s"  (type-of data))))))

;; * Global variables

(defvar delve-buffer-name "*DELVE*"
  "Name of delve buffers.")

(defvar delve-version-string "0.1"
  "Current version of delve.")

;; * Helper

(defun delve--flatten (l)
  "Flatten the list L and remove any nil's in it.
This is a simple copy of dash's `-flatten' using `seq'. "
  (if (and (listp l) (listp (cdr l)))
      (seq-mapcat #'delve--flatten l)
    (list l)))

;; * Buffer basics

(defun delve-new-buffer ()
  "Return a new DELVE buffer."
   (generate-new-buffer delve-buffer-name))

;; * Org Roam DB
;; TODO Move this into a separate file delve-db

(defvar delve-db-error-buffer "*DELVE - Database Error*"
  "Name for a buffer displaying SQL errors.")

(defvar delve-db-there-were-errors nil
  "Whether an error has occured since last query.")

(defun delve-db-log-error (err &rest strings)
  "Insert ERR and additional STRINGS in the error buffer."
  (declare (indent 1))
  (with-current-buffer (get-buffer-create delve-db-error-buffer)
    (special-mode)
    (let* ((inhibit-read-only t)
	   (date-string (format-time-string "%D %T  "))
	   ;; prevent logging if this is not the first error:
	   (message-log-max (if delve-db-there-were-errors nil  message-log-max)))
      (insert date-string
	      (format "Error message: %s\n" (error-message-string err)))
      (seq-doseq (s strings)
	(when (stringp s)
	  (insert date-string s "\n")))
      (insert "\n"))
    (unless delve-db-there-were-errors
      (message "There are errors. See buffer '%s' for more information."
	       delve-db-error-buffer)
      (setq delve-db-there-were-errors t))))

(defun delve-db-safe-query (sql &rest args)
  "Call org roam SQL query (optionally using ARGS) in a safe way.
Catch all errors and redirect the error messages to an error
buffer.  If an error occurs, inform the user with a message and
return nil."
  (condition-case-unless-debug err
      (apply #'org-roam-db-query sql args)
    (error (delve-db-log-error err
			       " Error occured when executing the query:"
			       (format " %s" sql)
			       (when args
				 (format " Arguments: %s" args)))
	   nil)))

(defun delve-db-rearrange (pattern l)
  "For each item in L, return a new item rearranged by PATTERN. 

Each element of PATTERN can be either a symbol, an integer, a
list with an integer and a function name, or a list with an
integer and a sexp.

If the element in PATTERN is a symbol, just pass it through.

If the element in PATTERN is an integer, use it as an index to
return the correspondingly indexed element of the original item.

If the element in PATTERN is a list, use the first element of
this list as the index and the second as a mapping function. In
this case, insert the corresponding item after passing it to the
function. 

A third option is to use a list with an index and a sexp. Like
the function in the second variant above, the sexp is used as a
mapping function. The sexp will be eval'd with the variable `it'
bound to the original item's element.

Examples:

 (delve-db-rearrange [1 0] '((a b) (a b)))   -> ((b a) (b a))
 (delve-db-rearrange [0] '((a b c) (a b c))) ->  ((a) (a))

 (delve-db-rearrange [1 (0 1+)] '((1 0) (1 0)))      -> ((0 2) (0 2))
 (delve-db-rearrange [1 (0 (1+ it))] '((1 0) (1 0))) -> ((0 2) (0 2))

 (delve-db-rearrange [:count 1] '((0 20) (1 87))) -> ((:count 20) (:count 87))"
  (seq-map (lambda (item)
	     (seq-mapcat (lambda (index-or-list)
			   (list
			    (if (symbolp index-or-list)
				index-or-list
			      (if (listp index-or-list)
				  (progn
				    (defvar it) ;; force dynamic binding for calling the sexp 
				    (let* ((fn-or-sexp (cadr index-or-list))
					   (it         (seq-elt item (car index-or-list))))
				      (if (listp fn-or-sexp)
					  (eval fn-or-sexp)
					(funcall fn-or-sexp it))))
				(seq-elt item index-or-list)))))
			 pattern))
	   l))

(defun delve-db-rearrange-into (make-fn keyed-pattern l)
  "Rearrange each item in L and pass the result to MAKE-FN.
KEYED-PATTERN is an extension of the pattern used by
`delve-db-rearrange'. For each element, this extended pattern
also requires a keyword. The object is created by using the
keywords and the associated result value as key-value-pairs
passed to MAKE-FN."
  (seq-map (lambda (item)
	     (apply make-fn item))
	   (delve-db-rearrange keyed-pattern l)))
  
;; * Org Roam Queries

;; Queries returning plain lists:

(defun delve-db-roam-tags ()
  "Return a list of all #+ROAM_TAGS."
  (thread-last (delve-db-safe-query [:select :distinct tags:tags :from tags])
    (delve--flatten)
    (seq-uniq)
    (seq-sort #'string-lessp)))

(defun delve-db-count-tag (tag)
  "Count the occurences of TAG in the org roam db."
  (pcase-let* ((`((( _ ) ,n))
		(delve-db-safe-query [:select [ tags:tags
					  (as (funcall count tags:tags) n) ]
					:from tags
					:where (like tags:tags $r1)]
				     (format "%%%s%%" tag))))
    n))

(defun delve-query-roam-tags ()
  "Return a list of tags of all #+ROAM_TAGS."
  (let* ((tags (delve-db-roam-tags)))
    (seq-map (lambda (tag)
	       (delve-make-tag :tag tag
			       :count (delve-db-count-tag tag)))
	     tags)))


(defun delve-db-count-backlinks (file)
  "Return the number of files linking to FILE."
  (caar (delve-db-safe-query [:select
			[ (as (funcall count links:from) n) ]
			:from links 
			:where (= links:to $s1)]
		       file)))

(defun delve-db-count-tolinks (file)
  "Return the number of files linked from FILE."
  (caar (delve-db-safe-query [:select
			[ (as (funcall count links:to) n) ]
			:from links 
			:where (= links:from $s1)]
		       file)))

;; Queries resulting in delve types:

;; We could just use cl-sort with :key, but we want this function to
;; be used with thread-last, so we use our own. A possible drawback
;; may be that cl-sort works destructively and thus should be more
;; efficient on bigger lists, which are copied by seq-sort.
(defun delve-sort-query-results (sort-fn accessor-fn l)
  "Sort L using SORT-FN, accessing the items of L with ACCESSOR-FN."
  (seq-sort (lambda (i1 i2)
	      (funcall sort-fn
		       (funcall accessor-fn i1)
		       (funcall accessor-fn i2)))
	    l))


;; TODO This is way too much work for longer lists; find a better
;; solution.
(defun delve-query-update-link-counter (l)
  "For each zettel object in L, update the links counter."
  (with-temp-message "Counting links...."
  (seq-map (lambda (z)
	     (let ((file (delve-zettel-file z)))
	       (setf (delve-zettel-backlinks z) (delve-db-count-backlinks file))
	       (setf (delve-zettel-tolinks   z) (delve-db-count-tolinks file))
	       z))
	   l)))


(defun delve-query-zettel-with-tag (tag)
  "Return a list of all zettel tagged TAG."
  (thread-last 
      (delve-db-safe-query [:select [ tags:file titles:title tags:tags files:meta ]
			    :from tags
			    :left-join titles   :on (= tags:file titles:file )
			    :left-join files    :on (= tags:file files:file  )
			    :where (like tags:tags $r1) ]
			   (format "%%%s%%" tag))
    (delve-db-rearrange-into 'delve-make-zettel
			     [:file  0
			      :title 1
			      :tags  2
			      :mtime (3 (plist-get it :mtime))
			      :atime (3 (plist-get it :atime))])
    ;; (delve-sort-query-results #'string-lessp #'delve-zettel-title)))
    ;;    (delve-sort-query-results #'time-less-p #'delve-zettel-mtime)))
    (delve-sort-query-results (lambda (e1 e2) (time-less-p (car e2) (car e1))) #'delve-zettel-mtime)
    (delve-query-update-link-counter)))

(defun delve-query-zettel-matching-title (term)
  "Return a list of all zettel where the title contains TERM."
  (thread-last
      (delve-db-safe-query [:select [ titles:title titles:file files:meta tags:tags]
				    :from titles
				    :left-join files
				    :on (= titles:file files:file)
				    :left-join tags
				    :on (= titles:file tags:file)
				    :where (like titles:title $r1)]
			   (format "%%%s%%" term))
    (delve-db-rearrange-into 'delve-make-zettel
			     [:title 0
			      :file  1
			      :mtime (2 (plist-get it :mtime))
			      :atime (2 (plist-get it :atime))
			      :tags  3])
    (delve-query-update-link-counter)))

(defun delve-query-backlinks (zettel)
  "Return all zettel linking to ZETTEL."
  (thread-last
      (delve-db-safe-query [:select [ titles:title titles:file files:meta tags:tags
				      links:to links:from ]
				    :from links
				    :left-join titles    :on (= links:from titles:file)
				    :left-join files     :on (= links:from files:file)
				    :left-join tags      :on (= links:from tags:file)
				    :where (= links:to $s1)
				    :order-by (asc links:from)]
			   (delve-zettel-file zettel))
    (delve-db-rearrange-into 'delve-make-zettel
			     [:title  0
			      :file   1
			      :mtime (2 (plist-get it :mtime))
			      :atime (2 (plist-get it :atime))
			      :tags 3 ])
    (delve-query-update-link-counter)))

;; * Delve actions and keys

;; Set or reset the global list of the buffer

(defun delve-start-with-list (buf seq)
  "Delete all items in BUF and start afresh with SEQ."
  (lister-set-list buf seq)
  (when seq (lister-goto buf :first)))

(defun delve-start-with-list-at-point (buf pos)
  "Place the list at POS and its sublists as the new main list."
  (pcase-let ((`(,beg ,end _ ) (lister-sublist-boundaries buf pos)))
    (delve-start-with-list buf (lister-get-all-data-tree buf beg end))))

(defun delve-initial-list ()
  "Populate the current delve buffer with a list of tags."
  (interactive)
  (delve-start-with-list (current-buffer) (delve-query-roam-tags)))

;; Key "C-l"
(defun delve-sublist-to-top ()
  "Replace all items with the current sublist at point."
  (interactive)
  (unless lister-local-marker-list
    (user-error "There are not items in this buffer."))
  (delve-start-with-list-at-point (current-buffer) (point)))

;; Item action

(defun delve-insert-zettel-with-tag (buf pos tag)
  "Insert all zettel tagged TAG below the item at POS."
  (let* ((zettel (delve-query-zettel-with-tag tag)))
    (if zettel
	(lister-insert-sublist-below buf pos zettel)
      (user-error "No zettel found matching tag %s" tag))))

(defun delve-insert-backlinks (buf pos zettel)
  "Insert all backlinks to ZETTEL below the item at POS."
  (let* ((backlinks (delve-query-backlinks zettel)))
    (if backlinks
	(lister-insert-sublist-below buf pos backlinks)
      (user-error "No backlinks found."))))

;; Key "Enter"
(defun delve-action (data)
  "Action on pressing the enter key."
  (if (lister-sublist-below-p (current-buffer) (point))
      (lister-remove-sublist-below (current-buffer) (point))
    (cl-case (type-of data)
      (delve-tag     (delve-insert-zettel-with-tag (current-buffer) (point) (delve-tag-tag data)))
      (delve-zettel  (delve-insert-backlinks (current-buffer) (point) data)))))

;; Other key actions

(defun delve-visit-zettel (buf pos visit-function)
  "Open the zettel at POS using VISIT-FUNCTION."
  (let* ((data (lister-get-data (current-buffer) pos)))
    (unless (eq (type-of data) 'delve-zettel)
      (user-error "Item at point is no zettel"))
    (funcall visit-function (delve-zettel-file data))))

;; Key "o"
(defun delve-open ()
  "Open the item on point, leaving delve."
  (interactive)
  (delve-visit-zettel (current-buffer) :point #'find-file)
  (org-roam-buffer-toggle-display))

;; Key "v"
(defun delve-view ()
  "View the item on point without leaving delve."
  (interactive)
  (save-selected-window
    (delve-visit-zettel (current-buffer) :point #'find-file-other-window)
    ;; this does not work, I have no clue why:
    (org-roam-buffer-toggle-display)))

;; * Delve Mode

(defvar delve-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lister-mode-map)
    (define-key map "v" #'delve-view)
    (define-key map "o" #'delve-open)
    (define-key map (kbd "C-l") #'delve-sublist-to-top)
    (define-key map "."  #'delve-initial-list)
    map)
  "Key map for `delve-mode'.")


(define-derived-mode delve-mode
  lister-mode "Delve"
  "Major mode for browsing your org roam zettelkasten."
  ;; Setup lister first since it deletes all local vars:
  (lister-setup	(current-buffer) #'delve-mapper
		nil                             ;; initial data
		(concat "DELVE Version " delve-version-string) ;; header
		nil ;; footer
		nil ;; filter
		t   ;; no major-mode
		)
  ;; Now add delve specific stuff:
  (setq-local lister-local-action #'delve-action))

;; * Interactive entry points

(defvar delve-toggle-buffer nil
  "The last created lister buffer.
Calling `delve-toggle' switches to this buffer.")

;;;###autoload
(defun delve ()
  "Delve into the org roam zettelkasten."
  (interactive)
  (with-current-buffer (setq delve-toggle-buffer (delve-new-buffer))
    (delve-mode)
    (lister-highlight-mode)
    (delve-initial-list))
  (switch-to-buffer delve-toggle-buffer))

;;;###autoload
(defun delve-toggle ()
  (interactive)
  (if (and delve-toggle-buffer
	   (buffer-live-p delve-toggle-buffer))
      (switch-to-buffer delve-toggle-buffer)
    (delve)))


(provide 'delve)
;;; delve.el ends here
