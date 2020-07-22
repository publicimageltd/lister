;;; zettelbrowse.el --- Delve into the depths of your zettelkasten       -*- lexical-binding: t; -*-

;; Copyright (C) 2020  

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

;; Delve into the depths of your zettelkasten.

;;; Code:

;; * Dependencies

;; add current buffer's directory to the load path:
(require 'cl-lib)
(cl-eval-when (eval)
  (if (not (member default-directory load-path))
      (add-to-list 'load-path default-directory)))

(require 'lister)
(require 'lister-highlight)
(require 'org-roam)

;; * Item data types

(defstruct (delve-generic-item (:constructor delve-make-generic-item))
  type)

(defstruct (delve-tag (:constructor delve-make-tag)
		      (:include delve-generic-item (type 'tag)))
  tag
  count)

(defstruct (delve-zettel (:constructor delve-make-zettel)
			 (:include delve-generic-item (type 'zettel)))
  title
  file
  tags
  mtime
  atime)

;; * Item types and mapper function

(defvar delve-types '(zettel tag string)
  "List defining the possible item types in a delve list.")

(defun delve-represent-zettel (zettel)
  (list (propertize (delve-zettel-title zettel) 'face 'org-document-title)
	(concat "("
		(if (delve-zettel-tags zettel)
		    (propertize (string-join (delve-zettel-tags zettel) ", ") 'face 'org-level-1)
		  "No tags")
		")  "
		(format-time-string "  Last modified: %D %T" (delve-zettel-mtime zettel)))))

(defun delve-represent-tag (tag)
  (list (concat "Tag: " (propertize (delve-tag-tag tag) 'face 'org-level-1)
		(when (delve-tag-count tag)
		  (format " (%d)" (delve-tag-count tag))))))

(defun delve-mapper (data)
  "Transform DATA into a printable list."
  (let* ((type (delve-generic-item-type data)))
    (cl-case type
      (zettel (delve-represent-zettel data))
      (tag    (delve-represent-tag data))
      (string (list "STRING ITEM:" (format "%s" data)))
      (t      (list (format "UNKNOWN TYPE: %s"  type))))))

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
  (lister-setup (generate-new-buffer delve-buffer-name)
		#'delve-mapper
		nil
		(concat "DELVE Version " delve-version-string)))

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

If the element in PATTERN is an integer, use it as an index to
return the correspondingly indexed element of the original item.

If the element in PATTERN is a list, use the first element of
this list as the index and the second as a mapping function. In
this case, insert the corresponding item after passing it to the
function. 

A third option is to use a sexp instead of a function. In this
case, the sexp will be eval'd with the variable `it' bound to the
original item's element.

If the elemen in PATTERN is a symbol, just pass it through.

Examples:

 (delve-db-rearrange [1 0] '((a b) (a b)))   -> ((b a) (b a))
 (delve-db-rearrange [0] '((a b c) (a b c))) ->  ((a) (a))

 (delve-db-rearrange [1 (0 1+)] '((1 0) (1 0)))      -> ((0 2) (0 2))
 (delve-db-rearrange [1 (0 (1+ it))] '((1 0) (1 0))) -> ((0 2) (0 2))

 (delve-db-rearrange [:count 1] '((0 20) (1 87))) -> ((:count 20) (:count 87))
"
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
`delve-db-rearrange'. For each element, the pattern also
specifies a keyword which is used to create the new object. The
object is created by using the keywords and their result as
key-value-pairs for MAKE-FN."
  (seq-map (lambda (item)
	     (apply make-fn item))
	   (delve-db-rearrange keyed-pattern l)))
  
  
;; * Org Roam Queries

(defun delve-sort-query-results (sort-fn accessor l)
  (seq-sort (lambda (i1 i2)
	      (funcall sort-fn (funcall accessor i1) (funcall accessor i2)))
	    l))

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

(defun delve-query-zettel-with-tag (tag)
  "Return a list of all zettel tagged TAG."
  (thread-last 
      (delve-db-safe-query [:select [ tags:file titles:title titles:file tags:tags files:meta]
				    :from tags
				    :left-join titles
				    :on (= tags:file titles:file )
				    :left-join files
				    :on (= tags:file files:file)
				    :where (like tags:tags $r1)]
			   (format "%%%s%%" tag))
    (delve-db-rearrange-into 'delve-make-zettel
			     [:title 1 :file 0
				     :tags 3
				     :mtime (4 (plist-get it :mtime))
				     :atime (4 (plist-get it :atime))])
    ;; (delve-sort-query-results #'string-lessp #'delve-zettel-title)))
    ;;    (delve-sort-query-results #'time-less-p #'delve-zettel-mtime)))
    (delve-sort-query-results (lambda (e1 e2) (time-less-p (car e2) (car e1))) #'delve-zettel-mtime)))

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
			     [:title 0 :file 1 :tags 3
				     :mtime (2 (plist-get it :mtime))
				     :atime (2 (plist-get it :atime))])))

(defun delve-query-backlinks (zettel)
  "Return all zettel linking to ZETTEL."
  (thread-last
      (delve-db-safe-query [:select [ titles:title titles:file files:meta tags:tags
						   links:to links:from
						   files:file tags:file ]
				    :from links
				    :left-join titles
				    :on (= links:from titles:file)
				    :left-join files
				    :on (= links:from files:file)
				    :left-join tags
				    :on (= links:from tags:file)
				    :where (= links:to $s1)
				    :order-by (asc links:from)]
			   (delve-zettel-file zettel))
    (delve-db-rearrange-into 'delve-make-zettel
			     [:title 0 :file 1 :tags 3
				     :mtime (2 (plist-get it :mtime))
				     :atime (2 (plist-get it :atime))])))
;; * For testing purposes

(defvar delve-test-buffer nil)

(defun delve-populate-buffer (buf)
  (let* ((tags (delve-query-roam-tags)))
    (lister-insert-sequence buf nil tags)
    (when tags (lister-goto buf :first))))

;; TODO auch POS muss als Argument übergeben werden, darf nicht implizit bleiben
(defun delve-insert-zettel-with-tag (buf tag)
  (let* ((zettel (delve-query-zettel-with-tag tag)))
    (if zettel
	(lister-insert-sublist-below buf (point) zettel)
      (user-error "No zettel found matching tag %s" tag))))

;; TODO auch POS muss als Argument übergeben werden, darf nicht implizit bleiben
(defun delve-insert-backlinks (buf zettel)
  (let* ((backlinks (delve-query-backlinks zettel)))
    (if backlinks
	(lister-insert-sublist-below buf (point) backlinks)
      (user-error "No backlinks found."))))

(defun delve-action (data)
  "Action on pressing the enter key."
  (if (lister-sublist-below-p (current-buffer) (point))
      (lister-remove-sublist-below (current-buffer) (point))
    (cl-case (type-of data)
      (delve-tag     (delve-insert-zettel-with-tag (current-buffer) (delve-tag-tag data)))
      (delve-zettel  (delve-insert-backlinks (current-buffer) data)))))

(defun delve-visit ()
  "Visit the item on point."
  (interactive)
  (let* ((data (lister-get-data (current-buffer) :point)))
    (cl-case (type-of data)
      (delve-zettel (find-file-other-window (delve-zettel-file data)))
      (t            (user-error "Cannot visit anything.")))))

(defun delve-toggle ()
  (interactive)
  (when (and (not (null delve-test-buffer))
	     (not (buffer-live-p delve-test-buffer)))
    (setq delve-test-buffer (delve-new-buffer))
    (with-current-buffer delve-test-buffer
      ;; (lister-highlight-mode) ;; TODO Bug: Faces werden nicht wieder hergestellt
      (setq lister-local-action #'delve-action))
    ;; TODO Eigenen major mode ableiten 
    (define-key lister-mode-map "o" #'delve-visit)
    (delve-populate-buffer delve-test-buffer))
  (switch-to-buffer delve-test-buffer))


(provide 'delve)
;;; delve.el ends here
