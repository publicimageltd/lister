;;; lister.el --- a not very functional list printer            -*- lexical-binding: t; -*-

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

;;; Code:


(require 'cl-lib)
(require 'seq)

;; * Variables

(cl-defstruct lister-viewport
  buffer        ;; associated buffer
  mapper        ;; converts DATA to a list of strings
  header-marker ;; position of the header 
  footer-marker ;; position of the footer
  marker-list   ;; marker positions for each subsequent item
  )

;; * Helper

(defsubst lister-curry (fn &rest args)
  (lambda (&rest more) (apply fn (append args more))))

(defsubst lister-rcurry (fn &rest args)
  (lambda (&rest more) (apply fn (append more args))))

(defsubst lister-compose (fn &rest more-fn)
  (seq-reduce (lambda (f g)
		(lambda (&rest args)
		  (funcall f (apply g args))))
	      more-fn
	      fn))


;; * Building the list with lines

;; These are the core primitives. The following functions either
;; insert, remove or replace lines of text, usually passed to these
;; functions as a list of strings.

;; For convenience, a function can be used instead of a string. The
;; function accepts no argument and returns a string, of course.

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

(defun lister-insert-lines (buf pos lines)
  "Insert list LINES at POS in BUF.

LINES is a list. Each item can be either a string, which is
printed directly, or a function, to print its return value.
Nested lists will be flattened. Empty lists will be skipped.

Insert each element of LINES with newline added. Mark the
beginning of the newly inserted text with the text property
'item. Store the number of inserted lines in the text property
'nlines. Move point to the end of the newly inserted text. Return
the marker of the first position."
  (with-current-buffer buf
    (let* ((beg               pos)
	   (item-list         (lister-strflat lines))
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
  "Return the end position of the item beginning at POS in BUF.

Use the text property 'nlines to determine the number of lines to
be deleted."
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

;; TODO Add option to let HEADER begin at arbitrary position (instead
;; of (point-min))
(defun lister-set-header (viewport header)
  "Set HEADER before the first item of VIEWPORT.

Replace the existing header, if any, or just insert it at the
top.

HEADER is a list. Each list item can be either a string, which is
printed directly, or a function, to print its return value.
Nested lists will be flattened. Empty lists will be skipped."
  (let ((buffer (lister-viewport-buffer viewport)))
    (if-let ((header-marker (lister-viewport-header-marker viewport)))
	;; replace existing header:
	(setf (lister-viewport-header-marker viewport)
	      (lister-replace-lines
	       buffer
	       (marker-position header-marker)
	       header))
      ;; or insert new one at top:
      (setf (lister-viewport-header-marker viewport)
	    (lister-insert-lines
	     buffer
	     (with-current-buffer buffer (point-min))
	     header)))
    ;; header is intangible for the cursor:
    (lister-set-intangible
     buffer
     (lister-viewport-header-marker viewport))))

(defun lister-set-footer (viewport footer)
  "Set FOOTER after the last item of VIEWPORT.

Replace the existing footer, if any, or just add it at the
end.

FOOTER is a list. Each list item can be either a string, which is
printed directly, or a function, to print its return value.
Nested lists will be flattened. Empty lists will be skipped."
  (let ((buffer (lister-viewport-buffer viewport)))
    (if-let ((footer-marker (lister-viewport-footer-marker viewport)))
	;; replace old footer:
	(setf (lister-viewport-footer-marker viewport)
	      (lister-replace-lines
	       buffer
	       (marker-position footer-marker)
	       footer))
      ;; or add it to the end:
      (setf (lister-viewport-footer-marker viewport)
	    (lister-insert-lines
	     buffer
	     (with-current-buffer buffer (point-max))
	     footer)))
    ;; footer is intangible for the cursor:
    (lister-set-intangible
     buffer
     (lister-viewport-footer-marker viewport))))

;; * Insert items

;; The following sections define functions to insert, add, remove and
;; replace list items.

;; An item is simply a string representation of DATA. The following
;; functions all insert an item at a given position. The item is
;; passed by passing a DATA object. The object will be turned into a
;; string representation by the mapper function of the viewport.

(cl-defgeneric lister-insert (viewport position data)
  "Insert a representation of DATA at POSITION in VIEWPORT.

POSITION can be either a buffer position (no marker!) or the
special key :point.

If POSITION is a an integer, insert item at this buffer position.

If POSITION is the symbol :point, insert it at point.

All modifications apply to the buffer associated with VIEWPORT.
The representation of DATA is created by the mapper function of
VIEWPORT. The function updates the marker list of VIEWPORT.")

;; This is the real function, all other variants are just wrappers:
(cl-defmethod lister-insert (viewport (position integer) data) 
  "Insert a representation of DATA at buffer position POS in VIEWPORT.

POS has to be an integer. Return a marker set to POS.

All modifications apply to the buffer associated with VIEWPORT.
The representation of DATA is created by the mapper function of
VIEWPORT. The function updates the marker list of VIEWPORT. "
  (let* ((buf    (lister-viewport-buffer viewport))
	 (item   (funcall (lister-viewport-mapper viewport) data))
	 (marker (lister-insert-lines buf position item)))
    (lister-set-data viewport marker data)
    ;; update marker list:
    (setf (lister-viewport-marker-list viewport)
	  (seq-sort #'<
		    (append
		     (lister-viewport-marker-list viewport)
		     (list marker))))
    marker))

(cl-defmethod lister-insert (viewport (position (eql :point)) data)
  "Insert a representation of DATA at point in VIEWPORT.
Return the marker pointing to the beginning of the list item."
  (let* ((pos (with-current-buffer (lister-viewport-buffer viewport)
		(point))))
    (lister-insert viewport
		   pos
		   data)))

;; * Add

(defun lister-add (viewport data)
  "Add a list item representing DATA to the end of the list of VIEWPORT.

Return the marker pointing to the beginning of the item.

All modifications apply to the buffer associated with VIEWPORT.
The representation of DATA is created by the mapper function of
VIEWPORT."
  (lister-insert viewport (lister-next-free-position viewport) data))


;; * Remove

(cl-defgeneric lister-remove (viewport position)
  "Remove the item on POSITION in VIEWPORT.

POSITION can be either a marker, a list index position, or the
symbol :point.

If POSITION is a marker, remove the item at the marker position.

If POSITION is an integer, treat it as an index number, the first
item counting as 0. Remove the item determined by the index
position.

If POSITION is the symbol :point, remove the item at point.

All modifications apply to the buffer associated with VIEWPORT.
The representation of DATA is created by the mapper function of
VIEWPORT.")

;; This is the real function, all other variants are just wrappers:
(cl-defmethod lister-remove (viewport (position marker))
  "Remove the item at marker POSITION."
  (lister-remove-lines (lister-viewport-buffer viewport)
		       position)
  (setf (lister-viewport-marker-list viewport)
	(seq-remove (lambda (m)
		      (eq m position))
		    (lister-viewport-marker-list viewport))))

(cl-defmethod lister-remove (viewport (position integer))
  "Remove the item at index POSITION."
  (lister-remove viewport (lister-marker-at viewport position)))

(cl-defmethod lister-remove (viewport (position (eql :point)))
  "Remove the item at point."
  (when-let* ((marker (lister-current-marker viewport)))
    (lister-remove viewport marker)))

;; * Replace

(cl-defgeneric lister-replace (viewport position data)
  "Replace the item at POSITION with a new item representing DATA.

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
(cl-defmethod lister-replace (viewport (position marker) data)
  "Replace the item at marker POSITION with a new DATA item."
  (let* ((buffer-pos (marker-position position)))
    (lister-remove-lines (lister-viewport-buffer viewport) buffer-pos)
    (lister-insert viewport buffer-pos data)
    (goto-char buffer-pos)))

(cl-defmethod lister-replace (viewport (position integer) data)
  "Replace the item at index POSITION with a new DATA item."
  (lister-replace (viewport (lister-marker-at viewport position))))

(cl-defmethod lister-replace (viewport (position (eql :point)) data)
  "Replace the item at point with a new DATA item."
  (when-let* ((marker (lister-current-marker viewport)))
    (lister-replace viewport marker data)))

;; * Set data

(cl-defgeneric lister-set-data (viewport position data)
  "Store DATA in text property 'data at POSITION.

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
(cl-defmethod lister-set-data (viewport (position marker) data)
  "Store DATA at the buffer position defined by MARKER."
  (with-current-buffer (lister-viewport-buffer viewport)
    (let ((inhibit-read-only t))
      (put-text-property position (1+ position)
			 'cursor-sensor-functions
			 '(lister-sensor-function))
      (put-text-property position (1+ position)
			 'data data))))

(cl-defmethod lister-set-data (viewport (position integer) data)
  "Store DATA in the item at index POSITION."
  (lister-set-data viewport
		   (lister-marker-at viewport position)
		   data))

(cl-defmethod lister-set-data (viewport (position (eql :point)) data)
  "Store DATA in the item at point."
  (when-let* ((marker (lister-current-marker viewport)))
    (lister-set-data viewport marker data)))

;; * Get data

(cl-defgeneric lister-get-data (viewport position)
  "Retrieve the data stored at POSITION.

POSITION can be either a marker, a list index, or the symbol
:point.

If POSITION is a marker, return the data at the marker position.

If POSITION is an integer, treat it as an index number, starting
from 0. Return the data stored there. 

If POSITION is the symbol :point, return the data of the item at
point.")

;; This is the real function, all other variants are just wrappers:
(cl-defmethod lister-get-data (viewport (position marker))
  "Retrieve the data stored at marker POSITION."
  (with-current-buffer (lister-viewport-buffer viewport)
    (get-text-property position 'data)))

(cl-defmethod lister-get-data (viewport (position integer))
  "Retrieve the data stored at INDEX position."
  (lister-get-data viewport
		   (lister-marker-at viewport position)))

(cl-defmethod lister-get-data (viewport (position (eql :point)))
  "Retrieve the data of the item at point."
  (when-let* ((marker (lister-current-marker viewport)))
    (lister-get-data viewport marker)))

;; * Goto

(cl-defgeneric lister-goto (viewport position)
  "In VIEWPORT, move point to POSITION.

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
(cl-defmethod lister-goto (viewport (position marker))
  "Move point in VIEWPORT to the marker POSITION."
  (with-current-buffer (lister-viewport-buffer viewport)
    (let ((previous-point (point)))
      (goto-char position)
      (lister-sensor-function (selected-window) previous-point 'entered))))

(cl-defmethod lister-goto (viewport (position integer))
  "Move point in VIEWPORT to the index POSITION."
  (lister-goto viewport
	       (lister-marker-at viewport position)))

(cl-defmethod lister-goto (viewport (position (eql :last)))
  "Move point to the last item in VIEWPORT."
  (when-let* ((ml (lister-viewport-marker-list viewport)))
    (lister-goto viewport (car (last ml)))))

(cl-defmethod lister-goto (viewport (position (eql :first)))
  "Move point to the first item in VIEWPORT."
  (when-let* ((ml (lister-viewport-marker-list viewport)))
    (lister-goto viewport (car ml))))

;; * Marker Handling

(defun lister-marker-at (viewport index)
  "Return marker for item at index position INDEX.
The first item as the index 0, the second item the index 1, etc.
If the index is out of range, throw an error."
  (let* ((ml (lister-viewport-marker-list viewport)))
    (if (and (>= index 0)
	     (< index (length ml)))
	(nth index ml)
      (error "lister-marker-at: requested index %s out of range." index))))

(defun lister-current-marker (viewport)
  "Return MARKER of the item at point in VIEWPORT

Only return a marker if point is on the beginning of ITEM.

Return nil if no marker is available."
  (with-current-buffer (lister-viewport-buffer viewport)
    (save-excursion
      (when (get-text-property (point) 'item)
	(seq-find (lambda (m)
		    (eq (marker-position m) (point)))
		  (lister-viewport-marker-list viewport))))))

(defun lister-first-lines (buf)
  "Return position of the first item in BUF."
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
  "Create a list of all item positions in BUF."
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
  "Return a list of marker pointing to each item in BUF."
  (mapcar (lister-curry #'lister-make-marker buf)
	  (lister-item-positions buf)))

;; * Treat list items as indexed items

(cl-defgeneric lister-index (viewport position)
  "Return the index number of the item at POSIITION in VIEWPORT.

POSITION can be either a marker, a valid buffer position, or the
special key :point.

If POSITION is a marker or integer, return the index number of
the item at POSITION.

If POSITION is the symbol :point, return the index number of the
item at point.

All positions apply to the buffer associated with VIEWPORT.")

;; This is the real function, all other variants are just wrappers:
(cl-defmethod lister-index (viewport (position marker))
    "Return the index number of the item at MARKER position.".
  (seq-position (lister-viewport-marker-list viewport)
		position
		#'equal))

(cl-defmethod lister-index (viewport (position integer))
  "Return the index number of the item at buffer POSITION."
  (lister-index viewport (lister-make-marker
			  (lister-viewport-buffer viewport)
			  position)))

(cl-defmethod lister-index (viewport (position (eql :point)))
  "Return the index number of the item at point."
  (lister-index viewport (point)))

(defun lister-next-free-position (viewport)
  "Return the next free position for a new list item in VIEWPORT."
  (let* ((ml     (lister-viewport-marker-list viewport))
	 (buffer (lister-viewport-buffer viewport))
	 (header (lister-viewport-header-marker viewport))
	 (footer (lister-viewport-footer-marker viewport)))
    (cond
     ((last ml)  (lister-end-of-lines buffer (marker-position (car (last ml)))))
     (footer     (lister-end-of-lines buffer (marker-position footer)))
     (header     (point-max))
     (t          (point-min)))))

;; * Creating Markers

(defun lister-make-marker (buf pos)
  "Return a suitable marker for POS in BUF."
  (let ((marker (make-marker)))
    (set-marker marker pos buf)
    (set-marker-insertion-type marker t)
    marker))

(defun lister-recreate-marker-list (viewport)
  "Create and store a new marker list for VIEWPORT."
  (let* ((ml (lister-marker-list (lister-viewport-buffer viewport))))
    ;; move header and footer markers to their own viewport slots:
    (when (lister-viewport-header-marker viewport)
      (setf (lister-viewport-header-marker viewport)
	    (pop ml)))
    (when (lister-viewport-footer-marker viewport)
      (setf (lister-viewport-footer-marker viewport)
	    (car (last ml)))
      (setq ml (butlast ml)))
    ;; store list in viewport:
    (setf (lister-viewport-marker-list viewport) ml)))

;; * Cursor Sensor Function

(defvar-local lister-enter-item-hook nil
  "List of functions to call when point enters an existing
  item.")

(defun lister-sensor-function (win previous-point type)
  (when (eq type 'entered)       
    (run-hooks 'lister-enter-item-hook)))

;; * Lister Major Mode

(define-derived-mode lister-mode
  special-mode "Lister"
  "Major mode for selecting list items."
  (cursor-sensor-mode)
  (cursor-intangible-mode))

;; * Setup 

(defun lister-set-list (viewport data-list)
  "Insert DATA-LIST in VIEWPORT, leaving header and footer untouched.
Return the viewport.

To set the header or the footer, use `lister-set-header' and
`lister-set-footer'."
  (with-current-buffer (lister-viewport-buffer viewport)
    ;; delete old list:
    (when-let* ((ml  (lister-viewport-marker-list viewport)))
      (let* ((beg (nth 0 ml)) ;; (nth 0 ml) is always the first item,
			      ;; because header marker is stored in
			      ;; its own special slot.
	     (end (or (lister-viewport-footer-marker viewport)
		      (point-max)))
	     (inhibit-read-only t))
	(delete-region beg end)))
    ;; insert new list:
    (setf (lister-viewport-marker-list viewport)
	  (mapcar (lister-curry #'lister-add viewport) data-list)))
  viewport)

(defun lister-setup (buf mapper-fn &optional data-list header footer)
  "Erase BUF, insert DATA-LIST using MAPPER-FN, and optionally add HEADER and FOOTER.

DATA-LIST is a list of data objects which will be passed to
MAPPER-FN. MAPPER-FN must accept only one argument, the data
object, and return a list of strings. See also
`lister-insert-lines' for the exact format of the return value.

HEADER is a list of strings which will be inserted at the top of
the list.

FOOTER is a list of strings which will be inserted at the end of
the list.

Move point to the first list item. Return a newly created
viewport structure."
  (let* ((viewport (make-lister-viewport
		    :buffer buf
		    :mapper mapper-fn)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
	(erase-buffer)))
    ;;
    (when header
      (lister-set-header viewport header))
    (when data-list
      (seq-each (lister-curry #'lister-add viewport) data-list))
    (when footer
      (lister-set-footer viewport footer))
    ;;
    (lister-recreate-marker-list viewport)
    ;;
    (when data-list
      (lister-goto viewport 0))
    ;;
    viewport))

(provide 'lister)
;;; lister.el ends here
