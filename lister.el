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
;; to a stringified list item. The lister buffer also holds some other
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
;;
;; - Kopple den Filter mit der "visibility":
;;   - Insert fügt IMMER ein
;;   - Testet aber jeweils auch sofort auf visibility
;;     - Diese Funktion muss ausgelagert werden, da sie von der Filterfunktion gebraucht wird.
;;     - Unsichtbare Items werden ganz normal weiterbehandelt, ändert sich nichts.
;;
;;   - Add-filter ruft filterfunktion auf:
;;   - Filterfunktion löscht alle invisibility, geht alle items durch und ruft ggf. "hide" auf
;;
;; - Ersetze "marker on the fly" durch (lister-pos-as-marker ....)
;;
;; - Extend documentation
;;;; - Add convenience functions to handle the normal case to 'enter' an item (by calling hooks with point at the item)
;;
;;; Code:


(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;; * Variables

(defvar-local lister-local-mapper nil
  "Function which converts any DATA object to a list of strings.")

(defvar-local lister-filter-predicates nil
  "List of functions to test if a list item should be printed.
The functions are called successively until the last one is
reached or one of the functions fails.")

(defvar-local lister-local-header-marker nil
  "Stores the marker for the upper left corner of the header.")

(defvar-local lister-local-footer-marker nil
  "Stores the marker for the upper left corner of the footer.")

(defvar-local lister-local-marker-list nil
  "Stores a list of marker positions for each lister list item.")

(defvar-local lister-local-data-list nil
  "Copy of the complete list which is displayed.")

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

(defun lister-marker-pos (marker-or-pos)
  "Return the position of MARKER-OR-POS."
  (if (markerp marker-or-pos)
      (marker-position marker-or-pos)
    marker-or-pos))

(defun lister-pos-as-marker (lister-buf marker-or-pos)
  "Return a marker pointing to MARKER-OR-POS."
  (if (markerp marker-or-pos)
      marker-or-pos
    (lister-make-marker lister-buf marker-or-pos)))

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

(defun lister-insert-lines (buf marker-or-pos lines)
  "Insert list LINES at POS in BUF.

MARKER-OR-POS can be either a marker object or a buffer position.

LINES is must be either nil, a list or string. 

If LINES is nil, do nothing and return nil. All modifications to
the buffer will be only done when lines has a non-nil value.

If LINES is a string, insert it with newline added. 

If LINES is list, insert each element of LINES with newline
added. Each item can be either a string, which is inserted
directly, or a function, to insert its return value. Nested lists
will be flattened. Empty lists will be skipped.

Mark the beginning of the newly inserted text with the text
property 'item. Store the number of inserted lines in the text
property 'nlines. Move point to the end of the newly inserted
text. 

Return the marker of the first position."
  (when lines
    (with-current-buffer buf
      (let* ((item-list-unpadded   (if (stringp lines)
				       (list lines)
				     (lister-strflat lines)))
	     (item-list            (lister-add-vertical-margins
				    (mapcar #'lister-add-side-margins item-list-unpadded)))
	     (beg                 (lister-marker-pos marker-or-pos))
	     (inhibit-read-only t))
	(goto-char beg)
	;; Mark the whole item except the newline character as being
	;;'intangible'. Assumes rear-stickiness.
	;; Leaving newline out allows cursor movement:
	(insert (propertize (string-join item-list "\n")
			    'cursor-intangible t
			    'field t)
		"\n") ;; <- this leaves the "tangible" gap for the next item!
	;; Store some useful information at the beginning of the item,
	;; which is also its "marker position" used to reference the
	;; item:
	(put-text-property beg (1+ beg) 'item t)
	(put-text-property beg (1+ beg) 'nlines (length item-list))
	(lister-make-marker buf beg)))))

(defun lister-remove-lines (buf marker-or-pos)	
  "Remove all item lines beginning at POS in BUF.

MARKER-OR-POS can be either a marker object or a buffer position.

Use the text property 'nlines to determine the number of lines to
be deleted."
  (with-current-buffer buf
    (let* ((inhibit-read-only t)
	   (cursor-sensor-inhibit t))
      (delete-region marker-or-pos (lister-end-of-lines buf marker-or-pos)))))

(defun lister-replace-lines (buf marker-or-pos new-lines)
  "Replace the item lines at POS with NEW-LINES.

MARKER-OR-POS can be either a marker object or a buffer position.

If NEW-LINES is nil, simply delete the entry at POS.

Use the text property 'nlines to determine the number of lines to
be deleted. Adjust the value of the text property according to
the new item."
  (with-current-buffer buf
    (save-excursion
      (lister-remove-lines buf marker-or-pos)
      (lister-insert-lines buf marker-or-pos new-lines))))

(defun lister-end-of-lines (buf marker-or-pos)
  "Return the end position of the item which starts at POS in BUF.
MARKER-OR-POS can be either a marker object or a buffer position.

Use the text property 'nlines to determine the size of the item."
  (with-current-buffer buf
    (save-mark-and-excursion
      (goto-char marker-or-pos)
      ;; NOTE: get-text-property accepts markers as POS, but this is
      ;; undocumented:
      (let* ((nlines (get-text-property marker-or-pos 'nlines)))
	(if (and nlines (integerp nlines))
	    (forward-line nlines)
	  (error "Did not find text property 'nlines at buffer position %s" (lister-marker-pos marker-or-pos))))
      (point))))

;; * Set header or footer of the list

;; Headers or footers are just ordinary lists inserted by
;; `lister-insert-lines'; usually lists of strings. Unlike list items,
;; they are set to be 'intangible' for the cursor, so that point
;; cannot move to them. For this to work, `cursor-intangible-mode' has
;; to be enabled.

;; Since header and footer are inserted with the same functions as
;; list items, they are also marked with the text property 'item.

(defun lister-set-header-or-footer (lister-buf lines type)
  "Insert LINES as a header or footer in LISTER-BUF, depending on TYPE.
TYPE must be either the symbol 'header or 'footer.
Setting LINES to `nil' effectively deletes the item."
  (with-lister-buffer lister-buf
    (let (marker-var default-pos)
      (pcase type
	('header (setq marker-var   'lister-local-header-marker
		       default-pos  'point-min))
	('footer (setq marker-var   'lister-local-footer-marker
		       default-pos  'point-max))
	(_       (error "unknown type %s, expected 'header or 'footer." type)))
      (set marker-var
	   (if (symbol-value marker-var)
	       (lister-replace-lines lister-buf
				     (symbol-value  marker-var)
				     lines)
	     (lister-insert-lines lister-buf
				  (funcall default-pos)
				  lines)))
      (when-let* ((m (symbol-value marker-var))
		  (inhibit-read-only t))
	  (put-text-property m (1+ m)  'cursor-intangible t)
	  (put-text-property m (1+ m)  'front-sticky t)))))

(defun lister-set-header (lister-buf header)
  "Set HEADER before the first item in LISTER-BUF.

Replace the existing header, if any, or just insert it at the
top.

HEADER is a string or a list. It it is a string, insert it as
such. If it is a list, each item can be either a string, which is
inserted directly, or a function, to insert its return
value. Nested lists will be flattened. Empty lists will be
skipped. 

Each inserted string is inserted with an additional newline."
  (lister-set-header-or-footer lister-buf header 'header))

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
  (lister-set-header-or-footer lister-buf footer 'footer))

;; * Show / hide items

(defun lister-set-item-invisibility (lister-buf marker-or-pos value)
  "Set the property 'invisible of the item at MARKER-OR-POS to VALUE.
Assumes a properly set up LISTER-BUF."
  (with-lister-buffer lister-buf
    (let* ((inhibit-read-only t)
	   (cursor-sensor-inhibit t)
	   (beg (lister-marker-pos marker-or-pos))
	   (end (lister-end-of-lines lister-buf marker-or-pos)))
      (put-text-property beg end 'invisible value)
      (put-text-property beg end 'front-sticky value))))

(defun lister-show-item (lister-buf marker-or-pos)
  "Make sure the item at MARKER-OR-POS is visible."
  (lister-set-item-invisibility lister-buf marker-or-pos nil))

(defun lister-hide-item (lister-buf marker-or-pos)
  "Make sure the item at MARKER-OR-POS is invisible."
  (lister-set-item-invisibility lister-buf marker-or-pos t))

(defun lister-invisible-markers (lister-buf) 
  "Return a list of markers pointing only to hidden items."
  (with-lister-buffer lister-buf
    (seq-filter (lambda (m)
		  ;; Since the marker position is the place for
		  ;; accessing the item with the cursor, we can safely
		  ;; assume that if the marker position is invisible,
		  ;; the whole item is invisible:
		  (text-property-any m (1+ m) 'invisible t))
		lister-local-marker-list)))

(defun lister-visible-markers (lister-buf) 
  "Return a list of markers pointing only to visible items."
  (with-lister-buffer lister-buf
    (seq-filter (lambda (m)
		  ;; Since the marker position is the place for
		  ;; accessing the item with the cursor, we can safely
		  ;; assume that if the marker position is invisible,
		  ;; the whole item is invisible:
		  (text-property-any m (1+ m) 'invisible nil))
		lister-local-marker-list)))

(defun lister-show-all-items (lister-buf)
  "Make all items visible again."
  (with-lister-buffer lister-buf
    (when lister-local-marker-list
      (let* ((inhibit-read-only t)
	     (beg (lister-marker-pos (car lister-local-marker-list)))
	     (end (lister-end-of-lines lister-buf (car (last lister-local-marker-list)))))
	(remove-text-properties beg end '(invisible nil))))))

(defun lister-possibly-hide-item (lister-buf marker-or-pos data)
  "Hide item at MARKER-OR-POS depending on DATA.
Pass DATA through `lister-filter-predicates'. If it passes, do
nothing. Else hide the item."
  (unless (lister-filter-data data lister-filter-predicates)
    (lister-hide-item lister-buf marker-or-pos)))

;; * Filtering

;; Items will be filtered by checking it against all predicates
;; defined in `lister-filter-predicates'

(defun lister-filter-data (data fn-list)
  "Return t iff DATA passes all test functions in FN-LIST.
Also return t if FN-LIST is empty."
  (let ((res t))
    (cl-loop
     for fn in fn-list
     while (setq res (funcall fn data))
     finally return res)))

(defun lister-add-filter (lister-buf fn &optional data-list no-update)
  "Add FN as a filter predicate and redisplay DATA-LIST in LISTER-BUF.

If no DATA-LIST is given, use the local copy instead. 

If DATA-LIST is non-nil, set it as the root data list and store a
local copy. This means effectively that the filtered list is the
new data list. There are no means to restore the old data list.
If you want just to use filters to narrow the display, do not
pass a non-nil argument for DATA-LIST.

If NO-UPDATE is non-nil, only add the filter, do not update the
display and ignore the argument DATA-LIST completely.

FN is the filter function. It must accept one argument and return
t if the item should be displayed.

The filter will be added to the end of the predicate list. To set
a filter and delete all other possibly implemented filters, use
`lister-set-filter'. To clear all filters, use
`lister-clear-filter'."
  (with-lister-buffer lister-buf
    (add-to-list 'lister-filter-predicates fn t)
    (unless no-update
      (lister-set-list lister-buf data-list))))

(defun lister-set-filter (lister-buf fn &optional data-list no-update)
  "Set FN as a filter predicate and redisplay DATA-LIST in LISTER-BUF.

If no DATA-LIST is given, use the local copy instead.

If DATA-LIST is non-nil, set it as the root data list and store a
local copy. This means effectively that the filtered list is the
new data list. There are no means to restore the old data list.
If you want just to use filters to narrow the display, do not
pass a non-nil argument for DATA-LIST.

FN is the filter function. It must accept one argument and return
t if the item should be displayed. Passing the pseudo-predicate
`nil' effectively removes all predicates.

If NO-UPDATE is non-nil, only add the filter, do not update the
display and ignore the argument DATA-LIST completely.

To add a filter without possibly deleting existing ones, use
`lister-add-filter'. To clear all filters, use
`lister-clear-filter'."
  (with-lister-buffer lister-buf
    (setq lister-filter-predicates (when fn (list fn))))
  (unless no-update
    (lister-set-list lister-buf data-list)))

(defun lister-clear-filter (lister-buf &optional data-list no-update)
  "Remove all filter from LISTER-BUF and display DATA-LIST.

If no DATA-LIST is given, use the local copy instead.

If DATA-LIST is non-nil, set it as the root data list and store a
local copy. This means effectively that the filtered list is the
new data list. There are no means to restore the old data list.
If you want just to use filters to narrow the display, do not
pass a non-nil argument for DATA-LIST.

If NO-UPDATE is non-nil, only remove the filter, do not update the
display and ignore the argument DATA-LIST completely.

To set one single filter, removing already installed ones, use
`lister-set-filter'. To add a filter without possibly deleting
existing ones, use `lister-add-filter'. "
  (lister-set-filter lister-buf nil data-list no-update))

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

DATA can be any kind of lisp object. If DATA is nil, do nothing.

DATA is eventually inserted by a mapper function. This function
creates a string representation (or a list of strings) for DATA.
The mapper function accepts only one argument and returns a
string or a list of strings. The function name has to be stored
in the variable `lister-local-mapper' locally stored in
LISTER-BUF.

POS has to be an integer representing a buffer position.

DATA will be only printed if it passes all tests from the list
`lister-filter-predicates' (see there).

Return a marker set to position POS.

This function updates the local variable which holds the marker
list (`lister-local-marker-list')."
  (when data
    (with-lister-buffer lister-buf
      (when-let* ((valid-data-p   (lister-filter-data data lister-filter-predicates))
		  (item           (funcall lister-local-mapper data))
		  (marker         (lister-insert-lines lister-buf position item)))
	(lister-set-data lister-buf marker data)
	;; update marker list:
	(lister-add-marker lister-buf marker)
	marker))))

(cl-defmethod lister-insert (lister-buf (position marker) data)
    "Insert a representation of DATA at MARKER in LISTER-BUF.

DATA can be any kind of lisp object. If DATA is nil, do nothing.

DATA is eventually inserted by a mapper function. This function
creates a string representation (or a list of strings) for DATA.
The mapper function accepts only one argument and returns a
string or a list of strings. The function name has to be stored
in the variable `lister-local-mapper' locally stored in
LISTER-BUF.

MARKER has to be marker.

DATA will be only printed if it passes all tests from the list
`lister-filter-predicates' (see there).

Return the marker pointing to the beginning of the newly inserted
item.

This function updates the local variable which holds the marker
list (`lister-local-marker-list')."
    (lister-insert lister-buf (marker-position position) data))

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
  (ignore position) ;; silence byte compiler warning
  (let* ((pos (with-current-buffer lister-buf (point))))
    (lister-insert lister-buf pos data)))

;; * Add

(defun lister-add (lister-buf data)
  "Add a list item representing DATA to the end of the list in LISTER-BUF.

DATA will be inserted by `lister-insert'. See there for possible
values of DATA and how filtering applies.

Return the marker pointing to the beginning of the newly added
item, or nil.

All modifications apply to LISTER-BUF. The representation of DATA
is created by the mapper function stored as a buffer local
variable. This function updates the local variable which holds
the marker list."
  (lister-insert lister-buf
		 (lister-next-free-position lister-buf)
		 data))


;; * Remove

(cl-defgeneric lister-remove (lister-buf position)
  "Remove the item on POSITION in LISTER-BUF.

POSITION can be either a marker or the symbol :point.

If POSITION is a marker, remove the item at the marker position.

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

(cl-defmethod lister-remove (lister-buf (position (eql :point)))
  "Remove the item at point."
  (ignore position) ;; silence byte compiler
  (if-let* ((marker (lister-current-marker lister-buf)))
      (lister-remove lister-buf marker)
    (error "lister-remove: no item found at point")))

;; * Replace

(cl-defgeneric lister-replace (lister-buf position data)
  "Replace the item at POSITION with a new item representing DATA.

DATA can be any kind of lisp object. A mapper function creates a
string representation (or a list of strings) for the data object.
The mapper function accepts only one argument and returns a
string or a list of strings. The function name has to be stored
in the buffer local variable `lister-local-mapper'.

POSITION can be either a marker or the special key :point.

If POSITION is a marker, replace the item at the position defined
by the marker.

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

(cl-defmethod lister-replace (lister-buf (position (eql :point)) data)
  "Replace the item at point with a new DATA item."
  (ignore position) ;; silence byte compiler
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

(cl-defmethod lister-get-mark-state (lister-buf (position (eql :point)))
  "In LISTER-BUF, check if the item at point is marked."
  (ignore position) ;; silence byte compiler
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
  (ignore position) ;; silence byte compiler
  (when-let* ((m (lister-current-marker lister-buf)))
    (lister-mark-item lister-buf m value)))

(defun lister-mark-all-items (lister-buf value)
  "Set all items to the marking state VALUE in LISTER-BUF."
  (with-lister-buffer lister-buf
    (seq-do (lambda (m) (lister-mark-item lister-buf m value)) lister-local-marker-list)))

(defun lister-mark-some-items (lister-buf marked-data value)
  "In LISTER-BUF, mark items which are members of MARKED-DATA.
Comparison is done with `equal'. VALUE should be either t to set
the mark or nil to remove it."
  (let* ((ml (with-current-buffer lister-buf lister-local-marker-list)))
    (cl-loop for m in ml
	     do
	     (when (member (lister-get-data lister-buf m) marked-data)
	       (lister-mark-item lister-buf m value)))))

(defun lister-display-mark-state (lister-buf marker)
  "In LISTER-BUF, display the 'mark' state of the item at MARKER."
  (with-lister-buffer lister-buf
    (let* ((state    (lister-get-mark-state lister-buf marker))
	   (face-fun (if state 'lister-add-face-property 'lister-remove-face-property))
	   (beg      marker)
	   (end      (lister-end-of-lines lister-buf beg)))
      (funcall face-fun beg end lister-mark-face-or-property))))

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

POSITION can be either a marker or the symbol :point.

If POSITION is a marker, store the data at the position defined
by the marker.

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

(cl-defmethod lister-set-data (lister-buf (position (eql :point)) data)
  "In LISTER-BUF, store DATA in the item at point."
  (ignore position) ;; silence byte compiler
  (when-let* ((marker (lister-current-marker lister-buf)))
    (lister-set-data lister-buf marker data)))

;; * Get data

(cl-defgeneric lister-get-data (lister-buf position)
  "Retrieve the data stored at POSITION in LISTER-BUF.

POSITION can be either a marker or the symbol :point.

If POSITION is a marker, return the data at the marker position.

If POSITION is the symbol :point, return the data of the item at
point.

The object has to be stored by `lister-set-data', which see.")

;; This is the real function, all other variants are just wrappers:
(cl-defmethod lister-get-data (lister-buf (position marker))
  "Retrieve the data stored at marker POSITION in LISTER-BUF."
  (with-lister-buffer lister-buf
    (get-text-property position 'data)))

(cl-defmethod lister-get-data (lister-buf (position (eql :point)))
  "Retrieve the data of the item at point."
  (ignore position) ;; silence byte compiler
  (when-let* ((marker (lister-current-marker lister-buf)))
    (lister-get-data lister-buf marker)))

(defun lister-get-all-data (lister-buf)
  "Collect all data values in LISTER-BUF."
  (with-lister-buffer lister-buf
    (seq-map (apply-partially #'lister-get-data lister-buf)
	     lister-local-marker-list)))

;; TODO Add test for visbibility
(defun lister-get-visible-data (lister-buf)
  "Collect all visible data values in LISTER-BUF."
  (with-lister-buffer lister-buf
    (seq-map (apply-partially #'lister-get-data lister-buf)
	     lister-local-marker-list)))
;; * Goto

(cl-defgeneric lister-goto (lister-buf position)
  "In LISTER-BUF, move point to POSITION.

POSITION can be either a marker or one of the symbols :last or
:first.

If POSITION is marker, move point to the marker position.

If POSITION is the symbol :first, move point to the first list
item, ignoring the header.

If POSITION is the symbol :last, move point to the last list
item, ignoring the header.")

;; This is the real function, all other variants are just wrappers:
(cl-defmethod lister-goto (lister-buf (position marker))
  "Move point in LISTER-BUF to the marker POSITION."
  (unless position
    (error "lister-goto: expected marker, called with `nil'"))
  (with-lister-buffer lister-buf
    (let ((previous-point (point)))
      (goto-char position)
      (lister-sensor-function (selected-window)
			      previous-point
			      'left)
      (lister-sensor-function (selected-window)
			      previous-point
			      'entered))))

(cl-defmethod lister-goto (lister-buf (position (eql :last)))
  "Move point to the last item in LISTER-BUF."
  (ignore position) ;; silence byte compiler
  (with-lister-buffer lister-buf
    (if-let* ((last-marker (car (last lister-local-marker-list))))
	(lister-goto lister-buf last-marker)
      (error "lister-goto: item list empty, cannot go to last item"))))

(cl-defmethod lister-goto (lister-buf (position (eql :first)))
  "Move point to the first item in LISTER-BUF."
  (ignore position) ;; silence byte compiler
  (with-lister-buffer lister-buf
    (if-let* ((first-marker (car lister-local-marker-list)))
	(lister-goto lister-buf first-marker)
      (error "lister-goto: item list empty, cannot go to first item"))))

;; * Marker Handling

;; FIXME possibly only used once
(defun lister-add-marker (lister-buf marker-or-pos)
  "Add MARKER-OR-POS to the local markerlist of LISTER-BUF."
  (with-lister-buffer lister-buf
    (let* ((the-marker (lister-pos-as-marker lister-buf marker-or-pos)))
      (setq lister-local-marker-list
	    (thread-last (append lister-local-marker-list (list the-marker))
	      (seq-sort #'<))))))

;; FIXME will be probably unused
(defun lister-remove-marker (lister-buf marker-or-pos)
  "Remove MARKER-OR-POS from the local marker list of LISTER-BUF."
  (with-lister-buffer lister-buf  
    (let* ((the-marker (lister-pos-as-marker lister-buf marker-or-pos)))
      (setq lister-local-marker-list
    	    (thread-last (seq-remove (apply-partially #'equal the-marker) lister-local-marker-list)
    	      (seq-sort #'<))))))

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

(defun lister-next-free-position (lister-buf) 
  "Return the next free position for a new list item in LISTER-BUF."
  (with-lister-buffer lister-buf
    (let* ((ml     lister-local-marker-list))
      (cond
       ;; if there are any items, return the last item position:
       ;; (this is independent of an existing footer)
       (ml                      (lister-end-of-lines lister-buf (marker-position (car (last ml)))))
       ;; now is there a footer? return its position to insert next item there: 
       (lister-local-footer-marker     (marker-position lister-local-footer-marker))
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

;; * Treat visible items as an indexed list

(defun lister-index-position (lister-buf marker-or-pos)
  "Return the index position of MARKER-OR-POS.
The index only refers to visible items."
  (with-lister-buffer lister-buf
    (let* ((m (lister-pos-as-marker lister-buf marker-or-pos)))
      (seq-position (lister-visible-markers lister-buf) m #'equal))))

(defun lister-index-marker (lister-buf index-position)
  "Return the marker of INDEX-POSITION, if available."
  (with-lister-buffer lister-buf
    (seq-elt (lister-visible-markers lister-buf) index-position)))

;; * Cursor Sensor Function

(defvar-local lister-enter-item-hook nil
  "List of functions to call when point enters an existing item.

When the callback function is called, the lister buffer is set
current and point is on the current item. Use `lister-get-data'
to access the data.

To avoid recursion, `cursor-sensor-inhibit' is set to `t' during
the execution of the callback functions.

Use `lister-add-enter-callback' to add a function to this buffer
local hook.")

(defvar-local lister-leave-item-hook nil
  "List of functions to call when point leaves an existing item.

When the callback function is called, the lister buffer is set
current and point is on the current item. Use `lister-get-data'
to access the data.

To avoid recursion, `cursor-sensor-inhibit' is set to `t' during
the execution of the callback functions.

Use `lister-add-leave-callback' to add a function to this buffer
local hook.")

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
	    (when (and (eq direction 'left)
		       (not (eq previous-point (point-max))))
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
  "Let FN-NAME be called when leaving a list item."
  (with-current-buffer lister-buf
    (add-hook 'lister-leave-item-hook fn-name nil t)))


;; * Lister Major Mode

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

(defun lister-set-list (lister-buf &optional data-list ignore-point)
  "In LISTER-BUF, insert DATA-LIST, leaving header and footer untouched.

If DATA-LIST is nil, use the local copy instead. This usually
means that you must have initialized the buffer with
`lister-setup'.

If DATA-LIST is non-nil, use this value and overwrite the local
copy. 

Try to keep point at the current position. If IGNORE-POINT is
non-nil, do not set point explicitly.

To set the header or the footer, use `lister-set-header' and
`lister-set-footer'."
  (with-lister-buffer lister-buf
    (let (old-index
	  (cursor-sensor-inhibit t))
      ;; delete old list:
      (when-let* ((ml lister-local-marker-list)
		  ;; (nth 0 ml) is always the first item,
		  ;; because header marker is stored in
		  ;; its own buffer local variable:
		  (beg (nth 0 ml))
		  (end (or lister-local-footer-marker 
			   (point-max)))
		  (inhibit-read-only t))
	(setq old-index (if (lister-current-marker lister-buf)
			    (lister-index-position lister-buf (point))
			  0))			  
	(delete-region beg end)
	(setq lister-local-marker-list nil))
      ;; insert new list:
      (setq lister-local-data-list
	    (or data-list lister-local-data-list))
      (setq lister-local-marker-list
	    ;; filtering might cause add to return nil,
	    ;; so weed it out:
	    (seq-filter #'identity 
			(mapcar
			 (apply-partially #'lister-add lister-buf)
			 lister-local-data-list)))
      ;; keep point at place:
      (unless ignore-point
	(if-let* ((vms      (lister-visible-markers lister-buf))
		  (maxindex (1- (length vms)))
		  (pos      (min maxindex old-index))
		  (m        (lister-index-marker lister-buf pos)))
	    (lister-goto lister-buf m)
	  (goto-char (point-min)))))))

;; * Set up a lister buffer

;;;###autoload
(defun lister-setup (buf mapper-fn &optional data-list
			 header footer
			 filter-functions
			 major-mode-fn)
  "Set up BUF to display DATA-LIST using MAPPER-FN.

DATA-LIST is a list of data objects which will be passed to
MAPPER-FN. MAPPER-FN must accept only one argument, the data
object, and return a list of strings. See also
`lister-insert-lines' for the exact format of the return value.

Optional argument HEADER is a list of strings which will be
inserted at the top of the list.

Optional argument FOOTER is a list of strings which will be
inserted at the end of the list.

Optional argument FILTER-FUNCTIONS defines a function or a list
of functions which will be used to filter the list before
printing. See `lister-filter-predicates'.

Set the major mode to `lister-mode' or call optional argument
MAJOR-MODE-FN as a function to set the major mode. The major mode
has to be a mode derived from `lister-mode'. 

Store all passed variables as buffer local variables. Move point
to the first list item. Return BUF."
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
    (setq lister-filter-predicates              
	  (when filter-functions
	    (if (listp filter-functions)
		filter-functions
	      (list filter-functions))))
    (setq lister-local-data-list nil)
    ;; ready to add header, list and footer:
    (when header
      (lister-set-header buf header))
    (when footer
      (lister-set-footer buf footer))
    (when data-list
      (lister-set-list buf data-list t))
    ;; move to first item:
    (when (lister-visible-markers buf)
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
