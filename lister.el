;;; lister.el --- yet another list printer             -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020

;; Author:  <joerg@joergvolbers.de>

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

;; === Overview:
;;
;; This library provides functions to support displaying a list in a
;; buffer. The functions help to insert in the buffer and to modify it
;; on a per-item basis.
;;
;; The buffer displaying the list items will be put in the major mode
;; `lister'. Most of all, this major mode restricts the movement of
;; the cursor to the very beginning of each item. Thus the displayed
;; list items can be accessed separately by the user using regular
;; cursor functions. There is no special keymap needed. E.g., to move
;; to the next item, no matter how long it is, just move down with the
;; cursor key; or to move up one item, just move up with the cursor
;; keys.
;;
;; Each list item in the buffer is associated with the data object it
;; is representing. Thus, it is easy to define functions which react
;; to the user pressing a key, e.g. <return>.
;;
;; The list can have a static `header' and a `footer'. Footer and
;; header can be updated without interfering with the list itself.
;;
;; === How to use:
;;
;; Most API functions require a `lister buffer` as their first
;; argument. The buffer has to in `lister-mode' or a mode derived from
;; it. Minimally, it also stores a function which maps the list data
;; to a stringifies list item. The lister buffer also holds some other
;; internal data. The usual way to make sure that the lister buffer is
;; set up correctly is to use the function `lister-setup'.
;;
;; The original data which is represented by the item is also stored
;; in the buffer, along with the string representation of the item. It
;; can be retrieved via `lister-get-data'.
;;
;; The usual approach is to build a first list using `lister-setup'.
;;
;; Most of these functions which deal with list items are generic
;; functions. They often accept different arguments, e.g., an explicit
;; buffer position, a marker, or a meaningful symbol such as :point,
;; :last or :first. See the documentation of the functions for further
;; informations.
;;
;; To insert a list item:
;; - `lister-insert'
;;
;; To add a list items at the end of the list:
;; - `lister-add'
;;
;; To remove a list item:
;; - `lister-remove'
;;
;; To replace an existing item with a new one:
;; - `lister-replace'
;;
;; To move to an item:
;; - `lister-goto'
;;

;; TODO
;; - Add keyboard functions to "mark" a list item
;; (dired-like) -- (`lister-mark-item')
;;
;; - Extend documentation
;;
;; - Add convenience functions to handle the normal case to 'enter' an item (by calling hooks with point at the item)
;;
;; - Add function to reconstruct data from list (so that you can delete stuff in the buffer)
;;
;; - Add debugging info for the macro `with-lister-buffer'
;;; Code:


(require 'cl-lib)
(require 'seq)

;; * Variables

(defvar-local lister-local-mapper nil
  "Function which converts any DATA object to a list of strings.")

(defvar-local lister-local-header-marker nil
  "Stores the marker for the upper left corner of the header.")

(defvar-local lister-local-footer-marker nil
  "Stores the marker for the upper left corner of the footer.")

(defvar-local lister-local-marker-list nil
  "Stores a list of marker positions for each lister list item.")

(defvar lister-left-margin 2
  "Add this left margin when inserting a item.
Set this to nil if no left margin is wanted.")

(defvar lister-right-margin nil
  "Add this right margin when inserting a item.
Set this to nil if no right margin is wanted.")

(defvar lister-top-margin nil
  "Add this top margin when inserting an item.
Set this to nil if no top margin is wanted.")

(defvar lister-bottom-margin nil
  "Add this bottom margin when inserting an item.
Set this to nil if no bottom margin is wanted.")

(defvar lister-mark-face-or-property
  '(:background "darkorange3"
    :foreground "white")
  "Additional text property highlighting marked items.
Any marked item will be highlighted by adding these properties.
Useful values are, for example, 

 (:background \"dark orange\") ;; sets a dark orange background

or 

  (:weight bold)

Alternatively, the value can be the name of a face.")

;; * Adding and removing faces or face properties

(defun lister-add-face-property (beg end face-or-plist)
  "A wrapper around `add-face-text-property'.
Use this in conjunction with `lister-remove-face-property'. Do
not change the text property of the face directly, unless you
know what you do."
  (let* ((current-props (get-text-property beg 'face))
	 (new-props     (if current-props
			    face-or-plist
			  ;; We always want a list of either plists or
			  ;; face names (i.e. '((:underline t))). So
			  ;; if this is the first face plist or face
			  ;; name to add, wrap it in a list.
			  (list face-or-plist))))
    (add-face-text-property beg end new-props)))

(defun lister-remove-face-property (beg end face-or-plist)
  "Remove FACE-OR-PLIST from the face property from BEG to END.
Use this in conjunction with `lister-add-face-property'. Do not
change the text property of the face directly, unless you know
what you do."
  (let* ((face-prop-value   (get-text-property beg 'face))
	 (current-props     (if (listp face-prop-value) face-prop-value (list face-prop-value)))
	 ;; this works because our wrapper around
	 ;; add-face-text-propery makes sure the value of the face
	 ;; property is always a list, even if we've added just a
	 ;; single item, such as a face name:
	 (new-props         (seq-remove
			     (apply-partially #'equal face-or-plist)
			     current-props)))
    (if new-props 
	(add-text-properties beg end (list 'face new-props))
      (remove-text-properties beg end '(face)))))

;; * Helper functions to work with lister buffers

(defun lister-buffer-p (buf)
  "Return BUF if it is ready to be used for lister lists.
Throw an informative error if BUF is not in `'lister mode' or if
the local mapper function is undefined."
  (with-current-buffer buf
    (or
     (and (derived-mode-p 'lister-mode)
	  lister-local-mapper
	  buf)
     (error
      (if (not (eq major-mode 'lister-mode))
	  "Buffer %s has to be in lister mode; execution aborted." 
	"Buffer %s has to have a local mapper function; execution aborted.")
      buf))))

(defalias 'assert-lister-buffer 'lister-buffer-p)

(defmacro with-lister-buffer (buf &rest body)
  "Execute BODY in BUF.
Throw an error if BUF is not a lister buffer."
  (declare (indent 1) (debug t))
  `(with-current-buffer (lister-buffer-p ,buf)
     ,@body))

;; * Building the list with lines

;; These are the core primitives. The following functions either
;; insert, remove or replace lines of text, usually passed to these
;; functions as a list of strings.

(defun lister-strflat (seq)
  "Recursively stringify all items in L, flattening any sublists.

To \"stringify\" means:

For each item, if the list item is a string, return it unchanged.
If the list item is a function, return its return value. The
function has to accept no argument and to return a string value.

Quoting cars such as (quote xy) or (function z) will be silently
dropped, keeping only the quoted item.

Empty lists or nil values will be skipped."
  (seq-reduce (lambda (acc e)
		(cond
		 ;; ignore nils:
		 ((null e)  acc)
		 ;; don't pass quoting cars to the result:
		 ((eq e 'function) acc)
		 ((eq e 'quote)    acc)
		 ((eq e 'closure)  acc)
		 ;; flatten lists
		 ((and (listp e) (not (functionp e))) (append acc (lister-strflat e)))
		 ;; actual work:
		 (t (append acc (list (if (functionp e) (funcall e) e))))))
	      seq '()))

(defun lister-add-side-margins (s)
  "Add padding left and right to S by adding spaces.
Margins are taken from the variables `lister-left-margin' and
`lister-right-margin'."
  (concat
   (and lister-left-margin
	(make-string lister-left-margin ? ))
   s
   (and lister-right-margin
	(make-string lister-right-margin ? ))))

(defun lister-add-vertical-margins (strings)
  "Add vertical padding to a list of STRINGS by adding empty strings.
Margins are taken from the variables `lister-top-margin' and
`lister-bottom-margin'."
  (append
   (and lister-top-margin
	(make-list lister-top-margin ""))
   strings
   (and lister-bottom-margin
	(make-list lister-bottom-margin ""))))

(defun lister-insert-lines (buf pos lines)
  "Insert list LINES at POS in BUF.

LINES is a list or string. If LINES is a string, insert it with
newline added. If LINES is list, insert each element of LINES
with newline added. Each item can be either a string, which is
inserted directly, or a function, to insert its return value.
Nested lists will be flattened. Empty lists will be skipped.

Mark the beginning of the newly inserted text with the text
property 'item. Store the number of inserted lines in the text
property 'nlines. Move point to the end of the newly inserted
text. 

Return the marker of the first position."
  (with-current-buffer buf
    (let* ((beg pos)
	   (item-list-unpadded   (if (stringp lines) (list lines)
				   (lister-strflat lines)))
	   (item-list            (lister-add-vertical-margins
				  (mapcar #'lister-add-side-margins item-list-unpadded)))
	   (inhibit-read-only t))
      (goto-char beg)
      (insert (string-join item-list "\n") "\n")
      (let* ((end (point)))
	(put-text-property beg (1+ beg) 'item t)
	(put-text-property beg (1- end) 'cursor-intangible t)
	(put-text-property beg (1+ beg) 'nlines (length item-list))
	(lister-make-marker buf beg)))))

(defun lister-remove-lines (buf pos)
  "Remove all item lines beginning at POS in BUF.

Use the text property 'nlines to determine the number of lines to
be deleted."
  (with-current-buffer buf
    (let* ((inhibit-read-only t))
      (delete-region pos (lister-end-of-lines buf pos)))))

(defun lister-replace-lines (buf pos new-lines)
  "Replace the item lines at POS with NEW-LINES.

Use the text property 'nlines to determine the number of lines to
be deleted. Adjust the value of the text property according to
the new item."
  (with-current-buffer buf
    (save-excursion
      (lister-remove-lines buf pos)
      (lister-insert-lines buf pos new-lines))))

(defun lister-end-of-lines (buf pos)
  "Return the end position of the item which starts at POS in BUF.
Use the text property 'nlines to determine the size of the item."
  (with-current-buffer buf
    (save-mark-and-excursion
      (goto-char pos)
      (forward-line (get-text-property pos 'nlines))
      (point))))

;; * Set header or footer of the list

;; Headers or footers are just ordinary lists inserted by
;; `lister-insert-lines'; usually lists of strings. Unlike list items,
;; they are set to be 'intangible' for the cursor, so that point
;; cannot move to them. For this to work, `cursor-intangible-mode' has
;; to be enabled.

;; Since header and footer are inserted with the same functions as
;; list items, they are also marked with the text property 'item.

(defun lister-set-intangible (buf pos-or-marker)
  "Mark position POS-OR-MARKER as intangible."
  (let* ((inhibit-read-only t)
	 (pos (if (markerp pos-or-marker) (marker-position pos-or-marker) pos-or-marker)))
    (with-current-buffer buf
      (let* ((beg (if (eq pos (point-min)) pos (1- pos)))
	     (end (if (eq pos (point-max)) pos (1+ pos))))
	(put-text-property beg end 'cursor-intangible t)
	(put-text-property beg end 'field t)
	(put-text-property beg end 'rear-sticky t)
	(put-text-property beg end 'front-sticky t)))))

(defun lister-set-header (lister-buf header)
  "Set HEADER before the first item in LISTER-BUF.

Replace the existing header, if any, or just insert it at the
top.

HEADER is a string or a list. It it is a string, insert it as
such. If it is a list, each item can be either a string, which is
inserted directly, or a function, to insert its return
value.Nested lists will be flattened. Empty lists will be
skipped. 

Each inserted string is inserted with an additional newline."
  (with-lister-buffer lister-buf
    ;; either replace existing header or insert new one at bottom:
    (setq lister-local-header-marker
	  (if lister-local-header-marker 
	      (lister-replace-lines lister-buf (marker-position lister-local-header-marker) header)
	    (lister-insert-lines lister-buf (point-min) header)))
    ;; set header to be intangible for the cursor:
    (lister-set-intangible lister-buf lister-local-header-marker)))

(defun lister-set-footer (lister-buf footer)
  "Set FOOTER after the last item of LISTER-BUF.

Replace the existing footer, if any, or just add it at the
end.

FOOTER is a string or a list. It it is a string, insert it as
such. If it is a list, each item can be either a string, which is
inserted directly, or a function, to insert its return
value.Nested lists will be flattened. Empty lists will be
skipped. 

Each inserted string is inserted with an additional newline."
  ;; either replace existing footer or insert new one at top:
  (with-lister-buffer lister-buf
    (setq lister-local-footer-marker
	  (if lister-local-footer-marker
	      (lister-replace-lines lister-buf (marker-position lister-local-footer-marker) footer)
	    (lister-insert-lines lister-buf (point-max) footer)))
    ;; set footer as intangible for the cursor:
    (lister-set-intangible lister-buf lister-local-footer-marker)))

;; * Insert items

;; The following sections define functions to insert, add, remove and
;; replace list items.

;; A list item is simply a string representation of a DATA item. The
;; following functions all insert a list item at a given position. The
;; data item itself is passed as a DATA object. The object will be
;; turned into a string representation by the buffer local mapper
;; function (`lister-local-mapper').

(cl-defgeneric lister-insert (lister-buf position data)
  "Insert a representation of DATA at POSITION in LISTER-BUF.

DATA can be any kind of lisp object. A mapper function creates a
string representation (or a list of strings) for the data object.
The mapper function accepts only one argument and returns a
string or a list of strings. The function name has to be stored
in the buffer local variable `lister-local-mapper'.

POSITION can be either a buffer position (no marker!) or the
special key :point.

If POSITION is a an integer, insert item at this buffer position.

If POSITION is the symbol :point, insert it at point.

All modifications apply to LISTER-BUF. The representation of DATA
is created by the mapper function stored as a buffer local
variable (`lister-local-mapper'). This function updates the local
variable which holds the marker list.")

;; This is the real function, all other variants are just wrappers:
(cl-defmethod lister-insert (lister-buf (position integer) data) 
  "Insert a representation of DATA at buffer position POS in LISTER-BUF.

DATA can be any kind of lisp object. A mapper function creates a
string representation (or a list of strings) for the data object.
The mapper function accepts only one argument and returns a
string or a list of strings. The function name has to be stored
in the buffer local variable `lister-local-mapper'.

POS has to be an integer.

Return a marker set to position POS.

This function updates the local variable which holds the marker
list (`lister-local-marker-list')."
  (with-lister-buffer lister-buf
    (let* ((item   (funcall lister-local-mapper data))
	   (marker (lister-insert-lines lister-buf position item)))
      (lister-set-data lister-buf marker data)
      ;; update marker list:
      (setq lister-local-marker-list 
	    (seq-sort #'<
		      (append
		       lister-local-marker-list
		       (list marker))))
      marker)))

(cl-defmethod lister-insert (lister-buf (position (eql :point)) data)
  "Insert a representation of DATA at point in LISTER-BUF.

DATA can be any kind of lisp object. A mapper function creates a
string representation (or a list of strings) for the data object.
The mapper function accepts only one argument and returns a
string or a list of strings. The function name has to be stored
in the buffer local variable `lister-local-mapper'.

Return the marker pointing to the beginning of the newly inserted
list item.

This function updates the local variable which holds the marker
list. (`lister-local-marker-list')"
  (let* ((pos (with-current-buffer lister-buf
		(point))))
    (lister-insert lister-buf pos data)))

;; * Add

(defun lister-add (lister-buf data)
  "Add a list item representing DATA to the end of the list in LISTER-BUF.

DATA can be any kind of lisp object. A mapper function creates a
string representation (or a list of strings) for the data object.
The mapper function accepts only one argument and returns a
string or a list of strings. The function name has to be stored
in the buffer local variable `lister-local-mapper'.

Return the marker pointing to the beginning of the item.

All modifications apply to LISTER-BUF. The representation of DATA
is created by the mapper function stored as a buffer local
variable. This function updates the local variable which holds
the marker list."
  (lister-insert lister-buf (lister-next-free-position lister-buf) data))


;; * Remove

(cl-defgeneric lister-remove (lister-buf position)
  "Remove the item on POSITION in LISTER-BUF.

POSITION can be either a marker, a list index position, or the
symbol :point.

If POSITION is a marker, remove the item at the marker position.

If POSITION is an integer, treat it as an index number, the first
item counting as 0. Remove the item determined by the index
position.

If POSITION is the symbol :point, remove the item at point.")

;; This is the real function, all other variants are just wrappers:
(cl-defmethod lister-remove (lister-buf (position marker))
  "Remove the item at the POSITION defined by the passed marker."
  (with-lister-buffer lister-buf
    (lister-remove-lines lister-buf position)
    (setq lister-local-marker-list 
	  (seq-remove (lambda (m)
			(eq m position))
		      lister-local-marker-list))))

(cl-defmethod lister-remove (lister-buf (position integer))
  "Remove the item at index POSITION."
  (lister-remove lister-buf (lister-marker-at lister-buf position)))

(cl-defmethod lister-remove (lister-buf (position (eql :point)))
  "Remove the item at point."
  (when-let* ((marker (lister-current-marker lister-buf)))
    (lister-remove lister-buf marker)))

;; * Replace

(cl-defgeneric lister-replace (lister-buf position data)
  "Replace the item at POSITION with a new item representing DATA.

DATA can be any kind of lisp object. A mapper function creates a
string representation (or a list of strings) for the data object.
The mapper function accepts only one argument and returns a
string or a list of strings. The function name has to be stored
in the buffer local variable `lister-local-mapper'.

POSITION can be either a marker, a list index position, or the
special key :point.

If POSITION is a marker, replace the item at the position defined
by the marker.

If POSITION is an integer, treat it as an index pointing to the
item where the data is to be stored at. The first item of the
list has the index position 0. See also `lister-marker-at'.

If POSITION is the symbol :point, store data at the item at
point.")

;; This is the real function, all other variants are just wrappers:
(cl-defmethod lister-replace (lister-buf (position marker) data)
  "Replace the item at marker POSITION with a new DATA item."
  (with-lister-buffer lister-buf
    (let* ((buffer-pos (marker-position position)))
      (lister-remove-lines lister-buf buffer-pos)
      (lister-insert lister-buf buffer-pos data)
      (goto-char buffer-pos))))

(cl-defmethod lister-replace (lister-buf (position integer) data)
  "Replace the item at index POSITION with a new DATA item."
  (lister-replace (lister-buf (lister-marker-at lister-buf position))))

(cl-defmethod lister-replace (lister-buf (position (eql :point)) data)
  "Replace the item at point with a new DATA item."
  (when-let* ((marker (lister-current-marker lister-buf)))
    (lister-replace lister-buf marker data)))

;; * Return mark state of an item

(cl-defgeneric lister-get-mark-state (lister-buf position)
  "For the list in LISTER-BUF, find out if the item at POSITION is marked.")

;; This is the real function, all other variants are just wrappers:
(cl-defmethod lister-get-mark-state (lister-buf (position marker))
  "In LISTER-BUF, find out if the item at POSITION is marked."
  (with-current-buffer lister-buf
    (get-text-property position 'mark)))

(cl-defmethod lister-get-mark-state (lister-buf (position integer))
  "In LISTER-BUF, check if the item at index POSITION is marked."
  (lister-get-mark-state lister-buf (lister-marker-at position)))

(cl-defmethod lister-get-mark-state (lister-buf (position (eql :point)))
  "In LISTER-BUF, check if the item at point is marked."
  (lister-get-mark-state lister-buf (lister-current-marker lister-buf)))


;; * Mark an item

(cl-defgeneric lister-mark-item (lister-buf position value)
  "In LISTER-BUF, set the item's mark at POSITION to VALUE.")

;; This is the real function, all other variants are just wrappers:
(cl-defmethod lister-mark-item (lister-buf (position marker) value)
    "In LISTER-BUF, set the item's mark at POSITION to VALUE."
  (with-current-buffer lister-buf
    (let* ((inhibit-read-only t))
      (put-text-property position (1+ position)  ;; works with markers AND integers 
			 'mark value)
      (lister-display-mark-state lister-buf position))))

(cl-defmethod lister-mark-item (lister-buf (position (eql :point)) value)
    "In LISTER-BUF, set the item's mark at POSITION to VALUE."
  (when-let* ((m (lister-current-marker lister-buf)))
    (lister-mark-item lister-buf m value)))

(cl-defmethod lister-mark-item (lister-buf (position integer) value)
    "In LISTER-BUF, set the item's mark at POSITION to VALUE."
  (lister-mark-item lister-buf
		    (lister-marker-at lister-buf position)
		    value))

(defun lister-mark-all-items (lister-buf value)
  "Set all items to the marking state VALUE in LISTER-BUF."
  (with-lister-buffer lister-buf
    (seq-do (lambda (m) (lister-mark-item lister-buf m value)) lister-local-marker-list)))

(defun lister-display-mark-state (lister-buf marker)
  "In LISTER-BUF, display the 'mark' state of the item at MARKER."
  (with-lister-buffer lister-buf
    (let* ((state    (lister-get-mark-state lister-buf marker))
	   (face-fun (if state 'lister-add-face-property 'lister-remove-face-property))
	   (beg      marker)
	   (end      (lister-end-of-lines lister-buf beg)))
      (funcall face-fun beg end lister-mark-face-or-property))))
      ;; (if state
      ;; 	  (add-text-properties beg end '(line-prefix "|"))
      ;; 	(remove-text-properties beg end '(line-prefix "|"))))))

;; * Collecting marked items

(defun lister-marked-items (lister-buf)
  "Get all markers pointing to marked items in LISTER-BUF."
  (with-lister-buffer lister-buf
    (seq-filter (apply-partially #'lister-get-mark-state lister-buf)
		lister-local-marker-list)))

(defun lister-map-marked-items (lister-buf fn)
  "Collect the results of calling FN on each marked item.
FN has to accept a marker object as its sole argument."
  (seq-map fn (lister-marked-items lister-buf)))

(defun lister-get-marked-data (lister-buf)
  "Collect all data from the marked items in LISTER-BUF."
  (lister-map-marked-items lister-buf
			   (apply-partially #'lister-get-data lister-buf)))

;; * Set data

(cl-defgeneric lister-set-data (lister-buf position data)
  "Store the lisp object DATA at POSITION in LISTER-BUF.

POSITION can be either a marker, an index position, or the symbol
:point.

If POSITION is a marker, store the data at the position defined
by the marker.

If POSITION is an integer, treat it as an index pointing to the
item where the data is to be stored at. The first item of the
list has the index position 0. See also `lister-marker-at'.

If POSITION is the symbol :point, store data at the item at
point.")

;; This is the real function, all other variants are just wrappers:
(cl-defmethod lister-set-data (lister-buf (position marker) data)
  "Store DATA at the position defined by MARKER in LISTER-BUF."
  (with-lister-buffer lister-buf
    (let ((inhibit-read-only t))
      (put-text-property position (1+ position)
			 'cursor-sensor-functions
			 '(lister-sensor-function))
      (put-text-property position (1+ position)
			 'data data))))

(cl-defmethod lister-set-data (lister-buf (position integer) data)
  "In LISTER-BUF, store DATA in the item at index POSITION."
  (lister-set-data lister-buf 
		   (lister-marker-at lister-buf position)
		   data))

(cl-defmethod lister-set-data (lister-buf (position (eql :point)) data)
  "In LISTER-BUF, store DATA in the item at point."
  (when-let* ((marker (lister-current-marker lister-buf)))
    (lister-set-data lister-buf marker data)))

;; * Get data

(cl-defgeneric lister-get-data (lister-buf position)
  "Retrieve the data stored at POSITION in LISTER-BUF.

POSITION can be either a marker, a list index, or the symbol
:point.

If POSITION is a marker, return the data at the marker position.

If POSITION is an integer, treat it as an index number, starting
from 0. Return the data stored there. 

If POSITION is the symbol :point, return the data of the item at
point.

The object has to be stored by `lister-set-data', which see.")

;; This is the real function, all other variants are just wrappers:
(cl-defmethod lister-get-data (lister-buf (position marker))
  "Retrieve the data stored at marker POSITION in LISTER-BUF."
  (with-lister-buffer lister-buf
    (get-text-property position 'data)))

(cl-defmethod lister-get-data (lister-buf (position integer))
  "Retrieve the data stored at INDEX position."
  (lister-get-data lister-buf
		   (lister-marker-at lister-buf position)))

(cl-defmethod lister-get-data (lister-buf (position (eql :point)))
  "Retrieve the data of the item at point."
  (when-let* ((marker (lister-current-marker lister-buf)))
    (lister-get-data lister-buf marker)))

;; * Goto

(cl-defgeneric lister-goto (lister-buf position)
  "In LISTER-BUF, move point to POSITION.

POSITION can be either a marker, a list index number, or one of
the symbols :last or :first.

If POSITION is marker, move point to the marker position.

If POSITION is an integer, treat it as an index number for the
list items, counting from 0. Move point to the item designated by
that index position.

If POSITION is the symbol :first, move point to the first list
item, ignoring the header.

If POSITION is the symbol :last, move point to the last list
item, ignoring the header.")

;; TODO Add error handling for the cases where desired position is not
;; available.

;; This is the real function, all other variants are just wrappers:
(cl-defmethod lister-goto (lister-buf (position marker))
  "Move point in LISTER-BUF to the marker POSITION."
  (with-lister-buffer lister-buf
    (let ((previous-point (point)))
      (goto-char position)
      (lister-sensor-function (selected-window) previous-point 'entered))))

(cl-defmethod lister-goto (lister-buf (position integer))
  "Move point in LISTER-BUF to the index POSITION."
  (lister-goto lister-buf
	       (lister-marker-at lister-buf position)))

(cl-defmethod lister-goto (lister-buf (position (eql :last)))
  "Move point to the last item in LISTER-BUF."
  (with-lister-buffer lister-buf
    (when-let* ((ml lister-local-marker-list))
      (lister-goto lister-buf (car (last ml))))))

(cl-defmethod lister-goto (lister-buf (position (eql :first)))
  "Move point to the first item in LISTER-BUF."
  (lister-goto lister-buf 0))

;; * Marker Handling

(defun lister-marker-at (lister-buf index)
  "Return marker for item at index position INDEX in LISTER-BUF.
The first item as the index 0, the second item the index 1, etc.
If the index is out of range, throw an error."
  (with-lister-buffer lister-buf
    (let* ((ml lister-local-marker-list))
      (if (and (>= index 0)
	       (< index (length ml)))
	  (nth index ml)
	(error "lister-marker-at: requested index position %s out of range." index)))))

(defun lister-current-marker (lister-buf)
  "Return MARKER of the item at point in LISTER-BUF.
Only return a marker if point is on the beginning of ITEM.
Return nil if no marker is available."
  (with-lister-buffer lister-buf 
    (save-excursion
      (when (get-text-property (point) 'item)
	(seq-find (lambda (m)
		    (eq (marker-position m) (point)))
		  lister-local-marker-list)))))

(defun lister-first-lines (buf)
  "Return position of the first item in BUF.
An 'item' can be the header, a list element or the footer."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (while (and (< (point) (point-max))
		  (not (get-text-property (point) 'item buf)))
	(goto-char (next-single-property-change (point)
						'item nil
						(point-max))))
      (when (get-text-property (point) 'item buf)
	(point)))))

(defun lister-item-positions (buf)
  "Create a list of all item positions in BUF.
An 'item' can be the header, a list element or the footer. The
footer, if any, is the first element of the list; the header, if
any, the last one."
  (with-current-buffer buf
    (save-excursion
      (when-let* ((pos (lister-first-lines buf)))
	(goto-char pos)
	(let* ((result   (list pos))
	       (lines     nil))
	  (while (setq lines (get-text-property (point) 'nlines buf))
	    (forward-line lines)
	    (when (get-text-property (point) 'item buf)
	      (push (point) result)))
	  (reverse result))))))

(defun lister-marker-list (buf)
  "Create a list of markers for each list item in BUF.
The result does not discriminate between header, list item or
footer. Each element is returned as an 'item'. The footer, if
any, is the first element of the list; the header, if any, the
last one."
  (mapcar (apply-partially #'lister-make-marker buf)
	  (lister-item-positions buf)))

;; * Treat list items as indexed items

(cl-defgeneric lister-index (lister-buf position)
  "Return the index number of the item at POSIITION in LISTER-BUF.

POSITION can be either a marker, a valid buffer position, or the
special symbol :point.

If POSITION is a marker or integer, return the index number of
the item at POSITION.

If POSITION is the symbol :point, return the index number of the
item at point.")

;; This is the real function, all other variants are just wrappers:
(cl-defmethod lister-index (lister-buf (position marker))
  "Return the index number of the item at MARKER position.".
  (with-lister-buffer lister-buf
    (seq-position lister-local-marker-list
		  position
		  #'equal)))

(cl-defmethod lister-index (lister-buf (position integer))
  "Return the index number of the item at buffer POSITION."
  (lister-index lister-buf
		;; make a marker on the fly:
		(lister-make-marker lister-buf position)))

(cl-defmethod lister-index (lister-buf (position (eql :point)))
  "Return the index number of the item at point."
  (lister-index lister-buf (with-current-buffer lister-buf (point))))

(defun lister-next-free-position (lister-buf) 
  "Return the next free position for a new list item in LISTER-BUF."
  (with-lister-buffer lister-buf
    (let* ((ml     lister-local-marker-list))
      (cond
       ;; if there are any items, return the last item position:
       ;; (this is independent of an existing footer)
       (ml                      (lister-end-of-lines lister-buf (marker-position (car (last ml)))))
       ;; now is there a footer? return its position to insert next item there: 
       (lister-local-footer-marker     (lister-end-of-lines lister-buf (marker-position lister-local-footer-marker)))
       ;; no footer, so insert after header, which is the end of the buffer:
       (lister-local-header-marker     (point-max))
       ;; nothing there, just go to the beginning:
       (t          (point-min))))))

;; * Creating Markers

(defun lister-make-marker (buf pos)
  "Return a suitable marker for POS in BUF."
  (let ((marker (make-marker)))
    (set-marker marker pos buf)
    (set-marker-insertion-type marker t)
    marker))

(defun lister-recreate-marker-list (lister-buf)
  "Create and store a new marker list for LISTER-BUF.
Also recreates the markers for the header and the footer of the list."
  (with-lister-buffer lister-buf
    ;; get a freshly created list of each 'item',
    ;; including header and footer:
    (let* ((ml (lister-marker-list lister-buf)))
      ;; move header and footer markers to their own variables:
      (when lister-local-header-marker
	(setq lister-local-header-marker (pop ml)))
      (when lister-local-footer-marker
	(setq lister-local-footer-marker (car (last ml)))
	(setq ml (butlast ml)))
      ;; store list:
      (setq lister-local-marker-list ml))))

;; * Cursor Sensor Function

(defvar lister-enter-item-hook nil
  "List of functions to call when point enters an existing item.
If the function is called, the lister buffer is set current, and
point is on the current item. 

To avoid recursion, `cursor-sensor-inhibit' is set to `t' when
calling the functions.

Use `lister-add-enter-callback' to add a function to this hook.")

(defvar lister-leave-item-hook nil
  "List of functions to call when point leaves an existing item.
If the function is called, the lister buffer is set current, and
point is on the item which is left. 

To avoid recursion, `cursor-sensor-inhibit' is set to `t' when
calling the functions.

Use `lister-add-leave-callback' to add a function to this hook.")

(defvar lister----counter 0)

(defvar-local lister--ignore-next-sensor-event nil
  "Ignore the next cursor sensor event.")

(defun lister-sensor-function (win previous-point direction)
  "Use WIN, PREVIOUS-POINT and DIRECTION to call callback functions.

This is a dispatcher function which is be used by the
`cursor-sensor-function' property. To use the cursor sensor
function for list items, add callbacks to
`lister-add-enter-callback' and `lister-add-leave-callback',
respectively."
  (with-current-buffer (window-buffer win)
    (when (derived-mode-p 'lister-mode)
      (let ((cursor-sensor-inhibit t)
	    (inhibit-read-only t))
	;; FOR DEBUGGING:
	;; (message "(%3d) Direction: %s; previous-point: %d; point: %d %s"
	;; 	 (incf lister----counter)
	;; 	 direction
	;; 	 previous-point
	;; 	 (point)
	;; 	 (if lister--ignore-next-sensor-event "(WILL BE IGNORED"
	;; 	   (if (eobp) "(BOUNCE BACK)" " ")))
	(if lister--ignore-next-sensor-event
	    (setq lister--ignore-next-sensor-event nil)
	  (if (eobp)
	      ;; never leave the last list item:
	      (progn 
		(goto-char previous-point)
		;; goto-char will cause a new `enter'-event, 
		;; let us ignore it:
		(setq lister--ignore-next-sensor-event t))
	    (when (eq direction 'left)
	      (save-excursion
		(goto-char previous-point)
		(run-hooks 'lister-leave-item-hook)))
	    (when (eq direction 'entered)
	      (run-hooks 'lister-enter-item-hook))))))))

(defun lister-add-enter-callback (lister-buf fn-name)
  "Let FN-NAME be called when entering a list item."
  (with-current-buffer lister-buf
    (add-hook 'lister-enter-item-hook fn-name nil t)))

(defun lister-add-leave-callback (lister-buf fn-name)
  "Let FN-NAME be called when entering a list item."
  (with-current-buffer lister-buf
    (add-hook 'lister-leave-item-hook fn-name nil t)))


;; * Lister Major Mode

(defun lister-key-test ()
  (interactive)
  (message "Hi there, this is a test! Did you read me?"))

(defun lister-key-toggle-mark ()
  "Toggle mark of item at point."
  (interactive)
  (let* ((current-state (lister-get-mark-state (current-buffer) :point)))
    (lister-mark-item (current-buffer) :point (not current-state))))

(defun lister-key-mark-all-items ()
  "Mark all items of the current list."
  (interactive)
  (lister-mark-all-items (current-buffer) t))

(defun lister-key-unmark-all-items ()
  "Umark all items of the current list."
  (interactive)
  (lister-mark-all-items (current-buffer) nil))

(defvar lister-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "m" 'lister-key-toggle-mark)
    (define-key map "*" 'lister-key-mark-all-items)
    (define-key map "u" 'lister-key-unmark-all-items)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    map)
  "Key map for `lister-mode'.")

(define-derived-mode lister-mode
  special-mode "Lister"
  "Major mode for selecting list items."
  :group 'lister
  (cursor-sensor-mode)
  (cursor-intangible-mode))

;; * Set a (new) list 

(defun lister-set-list (lister-buf data-list)
  "In LISTER-BUF, insert DATA-LIST, leaving header and footer untouched.
To set the header or the footer, use `lister-set-header' and
`lister-set-footer'."
  (with-lister-buffer lister-buf
    ;; delete old list:
    (when-let* ((ml lister-local-marker-list)
		;; (nth 0 ml) is always the first item,
		;; because header marker is stored in
		;; its own buffer local variable:
		(beg (nth 0 ml))
		(end (or lister-local-footer-marker 
			 (point-max)))
		(inhibit-read-only t))
      (delete-region beg end))
    ;; insert new list:
    (setq lister-local-marker-list
	  (mapcar (apply-partially #'lister-add lister-buf) data-list))))

;; * Set up a lister buffer

(defun lister-setup (buf mapper-fn &optional data-list header footer major-mode-fn)
  "Set up BUF to display DATA-LIST using MAPPER-FN.

DATA-LIST is a list of data objects which will be passed to
MAPPER-FN. MAPPER-FN must accept only one argument, the data
object, and return a list of strings. See also
`lister-insert-lines' for the exact format of the return value.

HEADER is a list of strings which will be inserted at the top of
the list.

FOOTER is a list of strings which will be inserted at the end of
the list.

Set the major mode to `lister-mode' or call MAJOR-MODE-FN as a
function to set the major mode. The major mode has to be a mode
derived from `lister-mode'. Store all passed variables as buffer
local variables. Move point to the first list item.

Return BUF."
  (with-current-buffer buf
    ;; first of all, set the major mode
    (funcall (or major-mode-fn 'lister-mode))
    ;; prepare the buffer:
    (setq lister-local-mapper mapper-fn)
    (setq lister-enter-item-hook nil
	  lister-leave-item-hook nil)
    (setq lister--ignore-next-sensor-event nil)
    (let ((cursor-sensor-inhibit t)
	  (inhibit-read-only t))
      (erase-buffer))
    ;; ready to add header, list and footer:
    (when header
      (lister-set-header buf header))
    (when data-list
      (seq-each (apply-partially #'lister-add buf) data-list))
    (when footer
      (lister-set-footer buf footer))
    ;; TODO Check why this should be necessary
    (lister-recreate-marker-list buf)
    ;;
    (when data-list
      (lister-goto buf :first))
    buf))

;; * Highlight minor mode

(defvar lister-highlight-face-or-property
  '(:foreground "yellow")
  "Text property or name of face to add when highlighting an
  item.")

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

(provide 'lister)
;;; lister.el ends here
