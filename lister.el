;;; lister.el --- Yet another list printer             -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020

;; Author:  <joerg@joergvolbers.de>
;; Version: 0.4
;; Package-Requires: ((seq "2.20") (emacs "26.1"))
;; Keywords: hypermedia
;; URL: https://github.com/publicimageltd/lister

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
(require 'subr-x)

;; * Variables

(defvar-local lister-local-mapper nil
  "Function which converts any DATA object to a list of strings.")

(defvar-local lister-local-action nil
  "Function which gets called 'on' an item to do something with it.")

(defvar-local lister-local-filter-term nil
  "Pseudo lisp term which is used as a filter.
The filter is constructed by wrapping this term into a lambda
expression with the argument DATA.")

(defvar-local lister-local-filter-active nil
  "Only filter the items if this buffer local variable is t.")

(defvar-local lister-local-header-marker nil
  "Stores the marker for the upper left corner of the header.")

(defvar-local lister-local-footer-marker nil
  "Stores the marker for the upper left corner of the footer.")

(defvar-local lister-local-marker-list nil
  "Stores a list of marker positions for each lister list item.")

(defvar-local lister-left-margin 2
  "Add this left margin when inserting a item.
Set this to nil if no left margin is wanted.")

(defvar-local lister-top-margin nil
  "Add this top margin when inserting an item.
Set this to nil if no top margin is wanted.")

(defvar-local lister-bottom-margin nil
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

(defvar-local lister-enter-item-hook nil
  "List of functions to call when point enters an existing item.
Use `lister-add-enter-callback' to add a function to this buffer
local hook. Do not use `add-hook'. 

When the callback function is called, the lister buffer is set
current and point is on the current item. Use `lister-get-data'
to access the data.

To avoid recursion, `cursor-sensor-inhibit' is set to `t' during
the execution of the callback functions.")

(defvar-local lister-leave-item-hook nil
  "List of functions to call when point leaves an existing item.

When the callback function is called, the lister buffer is set
current and point is on the current item. Use `lister-get-data'
to access the data.

To avoid recursion, `cursor-sensor-inhibit' is set to `t' during
the execution of the callback functions.

Use `lister-add-leave-callback' to add a function to this buffer
local hook.")

(defvar-local lister--ignore-next-sensor-event nil
  "Internal variable. Ignore the next cursor sensor event.")

;; -----------------------------------------------------------
;; * Useful stuff
;; -----------------------------------------------------------

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
Throw an error if BUF is not in `lister mode' or a major mode
derived from it. Also cancel if the local mapper function is not
defined."
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

(defmacro with-lister-buffer (buf &rest body)
  "Execute BODY in BUF.
Throw an error if BUF is not a lister buffer."
  (declare (indent 1) (debug t))
  `(with-current-buffer (lister-buffer-p ,buf)
     ,@body))

(defmacro save-lister-position (buf &rest body)
  "Execute BODY in BUF, trying to keep point at the current index position.
If POINT is not on a list item, move it to the first availbale
visible list item or to the beginning of the buffer."
  (declare (indent 1) (debug t))
  `(with-current-buffer (lister-buffer-p ,buf)
     (let* ((cursor-sensor-inhibit t)
	    (@old-index (or (lister-index-position lister-buf (point))
			    0)))
       ,@body
       (if-let* ((@visible-markers (lister-visible-markers ,buf)))
	   (lister-goto ,buf
			(lister-index-marker
			 ,buf
			 (min (1- (length @visible-markers)) @old-index)))
	 (goto-char (point-min))))))

(defun lister-pos-as-integer (marker-or-pos)
  "Return the integer value of MARKER-OR-POS."
  (if (markerp marker-or-pos)
      (marker-position marker-or-pos)
    marker-or-pos))

(defun lister-pos-as-marker (lister-buf marker-or-pos)
  "Return a marker pointing to MARKER-OR-POS."
  (if (markerp marker-or-pos)
      marker-or-pos
    (lister-make-marker lister-buf marker-or-pos)))

;; -----------------------------------------------------------
;; * Building the list with lines
;; -----------------------------------------------------------

;; These are the core primitives. The following functions either
;; insert, remove or replace lines of text, usually passed to these
;; functions as a list of strings.

(defun lister-validate-lines (lines &optional warning)
  "Pass LINES if it is a valid item or replace it with a warning string."
  (if (or (null lines) (not (listp lines)))
      (list (or warning "DATA ITEM UNDEFINED."))
    lines))

(defun lister-strflat (l)
  "Recursively stringify all items in L, flattening any sublists.
If L is a string, just wrap it in a list. Else, flatten L and
remove any empty lists, quoting cars such as (quote xy) or (function
z), and nil values. Replace function names with the result of
calling these functions with no args.

NOTE: The function has to be defined with defun, since it is
recognized via `functionp'.

Examples:  

 \"string\" -> (\"string\")

\(\"first row\" nil \"second row\" nil nil \"third row\") 
   ->  (\"first row\" \"second row\" \"third row\") 

\(current-time-string) -> (\"<current time as string>\")
 
\('current-time-string) -> (\"<current time as string>\")

 '(\"A\" '(\"B\" \"C\"))) -> (\"A\" \"B\" \"C\")"
  (if (stringp l)
      (list l)
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
		l '())))

(defun lister-indent-line (str n &optional offset)
  "Indent STR by adding N+OFFSET spaces.
N and OFFSET must an integer or nil."
  (concat (and n (make-string n ? ))
	  (and offset (make-string offset ? ))
	  str))

(defun lister-indent-lines (strings n &optional offset)
  "Indent all STRINGS by adding N+OFFSET spaces.
N and OFFSET must be an integer or nil"
  (mapcar (lambda (s) (lister-indent-line s n offset)) strings))

(defun lister-add-vertical-margins (lister-buf strings)
  "Pad a list of STRINGS vertically by adding empty strings.
Margins are taken from the buffer local variables
`lister-top-margin' and `lister-bottom-margin'."
  (with-current-buffer lister-buf
    (append
     (and lister-top-margin
	  (make-list lister-top-margin ""))
     strings
     (and lister-bottom-margin
	  (make-list lister-bottom-margin "")))))

(cl-defun lister-insert-lines (buf marker-or-pos lines level)
  "Insert list LINES with padding at POS in BUF.
MARKER-OR-POS can be either a marker object or a buffer position.
LINES must be a list. LEVEL, an integer, adds extra padding to
the item (e.g. to mark it as a subitem).

Mark the inserted text as `intangible', but leave a gap for the
cursor to access the item. Store some important values at the
position of the gap. Move point to the end of the newly inserted
text.

Return the marker pointing to the gap position."
  (when lines
    (with-current-buffer buf
      (let* ((padded-item-list  (lister-indent-lines lines lister-left-margin level))
	     (item-list         (lister-add-vertical-margins buf padded-item-list))
	     (beg               (lister-pos-as-integer marker-or-pos))
	     (inhibit-read-only t))
	(goto-char beg)
	;; Mark the whole item except the newline character as being
	;; 'intangible'. Assumes rear-stickiness.
	;; Leaving newline out allows cursor movement:
	(insert (propertize (string-join item-list "\n")
			    'cursor-intangible t
			    'field t)
		"\n") ;; <- this leaves the "tangible" gap for the next item!
	;;
	;; Store some useful information at the beginning of the item,
	;; which is also its "marker position" used to reference the
	;; item:
	(put-text-property beg (1+ beg) 'item t)
	(put-text-property beg (1+ beg) 'level level)
	(put-text-property beg (1+ beg) 'nlines (length item-list))
	(put-text-property beg (1+ beg) 'nchars (- (point) beg))
	(lister-make-marker buf beg)))))

(defun lister-remove-lines (buf marker-or-pos)	
  "Remove the 'lines' element beginning at MARKER-OR-POS in BUF.
A 'lines' element can be the header, a list item or the footer."
  (with-current-buffer buf
    (let* ((inhibit-read-only t)
	   (cursor-sensor-inhibit t))
      (delete-region marker-or-pos (lister-end-of-lines buf marker-or-pos)))))

(defun lister-replace-lines (buf marker-or-pos new-lines)
  "Replace the 'lines' element at POS with NEW-LINES.
A 'lines' element can be the header, a list item or the footer.
If NEW-LINES is nil, simply delete the entry at POS."
  (let ((level (get-text-property marker-or-pos 'level buf)))
    (lister-remove-lines buf marker-or-pos)
    (lister-insert-lines buf marker-or-pos new-lines level)))

(defun lister-end-of-lines (buf marker-or-pos)
  "Return the end position of the 'lines' element starting at POS.
A 'lines' element can be the header, a list item or the footer.
Use the text property symbol `nchars' to determine the size of
the item."
  (if-let* ((nchars (lister-get-prop buf marker-or-pos 'nchars)))
      (+ marker-or-pos nchars)
    (error "Did not find text property 'nchar at buffer position %d"
	   (lister-pos-as-integer marker-or-pos))))

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
    (let (marker-var
	  default-pos
	  (item (lister-strflat lines)))		   
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
				     item)
	     (lister-insert-lines lister-buf
				  (funcall default-pos)
				  item
				  0)))
      (when-let* ((m (symbol-value marker-var))
		  (inhibit-read-only t))
	(put-text-property m (1+ m)  'cursor-intangible t)
	(put-text-property m (1+ m)  'front-sticky t)))))

(defun lister-set-header (lister-buf header)
  "Insert or replace HEADER before the first item in LISTER-BUF."
  (lister-set-header-or-footer lister-buf header 'header))

(defun lister-set-footer (lister-buf footer)
  "Insert or replace FOOTER after the last item of LISTER-BUF."
  (lister-set-header-or-footer lister-buf footer 'footer))

;; -----------------------------------------------------------
;; * Filtering
;; -----------------------------------------------------------

;; Showing and hiding items

(defun lister-set-item-invisibility (lister-buf marker-or-pos value)
  "Set the (in)visibility of the item at MARKER-OR-POS.
The VALUE t hides the item, nil makes it visible."
  (with-lister-buffer lister-buf
    (let* ((inhibit-read-only t)
	   (cursor-sensor-inhibit t)
	   (beg (lister-pos-as-integer marker-or-pos))
	   (end (lister-end-of-lines lister-buf marker-or-pos)))
      (put-text-property beg end 'invisible value)
      ;; this closes the gap for the marker:
      (put-text-property beg (1+ beg) 'front-sticky value))))

(defun lister-show-item (lister-buf marker-or-pos)
  "Set the item at MARKER-OR-POS as visible."
  (lister-set-item-invisibility lister-buf marker-or-pos nil))

(defun lister-hide-item (lister-buf marker-or-pos)
  "Set the item at MARKER-OR-POS as invisible."
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
  "Make all items in LISTER-BUF visible again."
  (with-lister-buffer lister-buf
    (when lister-local-marker-list
      (let* ((inhibit-read-only t)
	     (beg (lister-pos-as-integer (car lister-local-marker-list)))
	     (end (lister-end-of-lines lister-buf (car (last lister-local-marker-list)))))
	(remove-text-properties beg end '(invisible nil))
	;; re-open the gap for the marker:
	;;
	;; This might be too precise, we could simply change all text
	;; properties in the buffer in one run. But is the latter way
	;; really faster?
	(cl-dolist (m lister-local-marker-list)
	  (remove-text-properties m (1+ m) '(front-sticky nil)))))))

(defun lister-possibly-hide-item (lister-buf marker-or-pos data)
  "Hide item at MARKER-OR-POS if applying the filter on DATA returns nil.
Show item if the result of applying the local filter term returns
non-nil."
  (unless (lister-apply-filter data lister-local-filter-term)
    (lister-hide-item lister-buf marker-or-pos)))

;; Filtering 

(defun lister-apply-filter (data term)
  "Pass DATA as an argument to TERM and return the result.
If TERM is nil, return t.
If DATA is nil, also return t."
  (when data 
    (if term
	;; TODO errorhandling
	(funcall `(lambda (data) ,term) data)
      t)))

;; TODO This does not work as intended.
(defun lister-add-filter-term (term fn op)
  "Combine TERM and FN with boolean operator OP.
Accepted operators are the symbols `or', `and', `xor' and `not'.
FN will be expanded to the list `(fn data)'.
If OP is the symbol `not', ignore FN and negate TERM.
Return the new term.

Examples:
                       nil 'fn 'and -> (and (fn data))
          (and (fn data)) 'fn2 'and -> (and (fn data) (fn2 data))
\(and (fn data) (fn2 data)) 'fn3 'or -> (or (and (fn data) (fn2 data)) (fn3 data))
                       nil 'fn 'not -> (not (fn data))"
  (unless (member op '(not and or xor))
    (error "Unknown operator '%s'; use either 'not', 'and' or 'or'." op))
  (if (null term)
      `(,op (,fn data))
    (let* ((current-op (car term)))
      (if (and (eq op current-op)
	       (not (eq op 'not)))
	  (append term `((,fn data)))
	`(,op ,term)))))

(defun lister-add-filter (lister-buf fn &optional op)
  "Combine FN with the current filter in LISTER-BUF.

FN is the filter function. It must accept a data object as its
only argument and return t if the item should be displayed.

Combination is done with `and' unless another operator OP is
passed explicitly. See `lister-add-filter-term'."
  (with-lister-buffer lister-buf
    (setq lister-local-filter-term 
	  (lister-add-filter-term lister-local-filter-term fn 'and))))

(defun lister-negate-filter (lister-buf)
  "Negate the current filter term in LISTER-BUF."
  (with-current-buffer lister-buf
    (unless lister-local-filter-term
      (error "Filter term expected"))
    (setq lister-local-filter-term
	  ;; TODO Use pcase to detect and eliminate double negation: (not (not _))
	  (lister-add-filter-term lister-local-filter-term nil 'not))))

(defun lister-set-filter (lister-buf fn &optional op)
  "Set FN as the only filter predicate in LISTER-BUF.

FN is the filter function. It must accept a data object as its
only argument and return t if the item should be displayed.

Set FN as the first filter in a boolean combination of filters.
Use the boolean operator `and' or instead use OP, if specified."
  (with-lister-buffer lister-buf
    (setq lister-local-filter-term
	  (lister-add-filter-term nil fn 'and))))

(defun lister-clear-filter (lister-buf)
  "Remove all filter from LISTER-BUF."
  (with-lister-buffer lister-buf
    (setq lister-local-filter-term nil)))

(defun lister-activate-filter (lister-buf)
  "Activate the filter in LISTER-BUF and update the display."
  (with-lister-buffer lister-buf
    (unless lister-local-filter-active
      (setq lister-local-filter-active t)
      (cl-dolist (m lister-local-marker-list)
	(lister-possibly-hide-item lister-buf m
				   (lister-get-data lister-buf m))))))

(defun lister-deactivate-filter (lister-buf)
  "Deactivate the filter in LISTER-BUF and update the display."
  (with-lister-buffer lister-buf
    (when lister-local-filter-active
      (setq lister-local-filter-active nil)
      (save-lister-position lister-buf
	(lister-show-all-items lister-buf)))))

(defun lister-update-filter (lister-buf)
  "Update the list by re-applying the current filter."
  (lister-deactivate-filter lister-buf)
  (lister-activate-filter lister-buf))

;; * Finding properties in other items

(defun lister-item-min (lister-buf)
  "Return the first allowed position for an item"
  (with-lister-buffer lister-buf
    (if lister-local-header-marker
	(lister-end-of-lines lister-buf lister-local-header-marker)
      (point-min))))

(defun lister-item-max (lister-buf)
  "Return the position of the last char of the last item."
  (with-lister-buffer lister-buf
    (if lister-local-footer-marker
	(1- (marker-position lister-local-footer-marker))
      (point-max))))

;; FIXME Don't know why we'd need to look at the NEXT item. Leaving
;; this option just in case.
(defun lister-looking-at-prop (lister-buf pos-or-marker prop direction)
  "Return the value of PROP, looking at either the previous or next item.
DIRECTION can be the symbol `previous' or the symbol `next'. 

This function assumes that POS-OR-MARKER is pointing to the
cursor gap of an item."
  (let (pos)
    (if (eq direction 'previous)
	;; looking up:
	(progn
	  (setq pos (previous-single-property-change pos-or-marker
						     prop
						     lister-buf
						     (lister-item-min lister-buf)))
	  (setq pos (and pos (max 1 (1- pos)))))
      ;; looking down:
      (setq pos (next-single-property-change pos-or-marker
					     prop
					     lister-buf
					     (lister-item-max lister-buf)))
      (setq pos (and pos (next-single-property-change pos
						      prop
						      lister-buf
						      (lister-item-max lister-buf)))))
    (when pos (get-text-property pos prop lister-buf))))

(defun lister-determine-level (lister-buf pos-or-marker level)
  "Determine the indentation level for new items at POS-OR-MARKER.
LEVEL can be nil, an integer or the symbols `:previous' or `:current'.

It is assumed that the return value will be used to insert a new
item at POS-OR-MARKER.

If LEVEL is an integer, check it against the level of the
previous (visible or invisible) item. If LEVEL is below or equal
this previous item's level, return it unchanged. If LEVEL is
greater, return the previous items's level + 1, thus making sure
that no 'level gap' is introduced when inserting.

If LEVEL is nil or the symbol `:previous', return the level of
the previous item, thus preserving its indentation for the new
item.

If LEVEL is the symbol `:current', return the level of the item
at point or 0 if there is no such item."
  (let* ((item-level (get-text-property pos-or-marker 'level lister-buf))
	 (prev-level (lister-looking-at-prop lister-buf pos-or-marker 'level 'previous)))
    (cond
     ((null prev-level)      0) ;; there's no previous level, thus no indentation
     ((null level)           prev-level)
     ((eq level :previous)   prev-level)
     ((eq level :current)    (or item-level 0))
     ((> level prev-level)   (1+ prev-level))
     (t                      level))))

;; -----------------------------------------------------------
;; * Insert, add, remove or replace list items
;; -----------------------------------------------------------

;; Insert

(cl-defgeneric lister-insert (lister-buf position data &optional level)
  "Insert DATA as item at POSITION in LISTER-BUF.
POSITION can be either a buffer position or the symbol `:point'.

Insert DATA at the indentation level LEVEL. For the possible
values of LEVEL, see `lister-determine-level'.

Return the marker of the inserted item's cursor gap position.")

;; This is the real function, all other variants are just wrappers:
(cl-defmethod lister-insert (lister-buf (position integer) data &optional level)
  "Insert DATA at buffer position POS in LISTER-BUF."
  (with-lister-buffer lister-buf
    (let* ((raw-item       (funcall lister-local-mapper data))
	   (item           (lister-validate-lines (lister-strflat raw-item)))
	   (new-level      (lister-determine-level lister-buf position level))
	   (marker         (lister-insert-lines lister-buf
						position
						item
						new-level)))
      (lister-set-data lister-buf marker data)
      (lister-set-prop lister-buf marker 'cursor-sensor-functions '(lister-sensor-function))
      (when lister-local-filter-active 
	(lister-possibly-hide-item lister-buf marker data))
      ;; update marker list:
      (lister-add-marker lister-buf marker)
      (goto-char marker)
      marker)))

(cl-defmethod lister-insert (lister-buf (position marker) data &optional level)
    "Insert DATA at MARKER in LISTER-BUF."
    (lister-insert lister-buf (marker-position position) data level))

(cl-defmethod lister-insert (lister-buf (position (eql :point)) data &optional level)
  "Insert DATA at point in LISTER-BUF."
  (ignore position) ;; silence byte compiler warning
  (let* ((pos (with-current-buffer lister-buf (point))))
    (lister-insert lister-buf pos data level)))

(defun lister-insert-sequence (lister-buf pos-or-marker seq &optional level)
  "Insert SEQ at POS-OR-MARKER in LISTER-BUF.
Insert SEQ above the item marked by POS-OR-MARKER. If
POS-OR-MARKER is nil, add it to the end of the list.

LEVEL determines the level of hierarchical indentation. See
`lister-determine-level' for all possible values for LEVEL.

SEQ must be either a vector or a list. Nested sequences will be
inserted with added indentation.

Return the position of the last inserted item marker."
  (when seq
    (let* ((seq-type     (type-of seq))
	   (pos          (or pos-or-marker (lister-next-free-position lister-buf)))
	   (new-level    (lister-determine-level lister-buf pos level)))
      (unless (member seq-type '(vector cons))
	(error "Sequence must be a vector or a list."))
      (seq-doseq (item seq)
	(setq pos (if (eq (type-of item) seq-type)
		      (lister-insert-sequence lister-buf pos item (1+ new-level))
		    (lister-end-of-lines lister-buf
					 (lister-insert lister-buf pos item new-level)))))
      pos)))

(defun lister-insert-sublist (lister-buf pos-or-marker seq)
  "Insert SEQ as an indented sublist at POS-OR-MARKER."
  (let* ((current-level (or (get-text-property pos-or-marker 'level lister-buf) 0)))
    (lister-insert-sequence lister-buf pos-or-marker seq (1+ current-level))))

(defun lister-insert-sublist-below (lister-buf pos-or-marker seq)
  "Insert SEQ as an indented sublist below the item at POS-OR-MARKER."
  (when-let* ((next-item      (lister-end-of-lines lister-buf pos-or-marker))
	      (current-level  (get-text-property pos-or-marker 'level lister-buf)))
    (lister-insert-sequence lister-buf next-item seq (1+ current-level))
    (goto-char pos-or-marker)))

;; Add

(defun lister-add (lister-buf data &optional level)
  "Add a list item representing DATA to the end of the list in LISTER-BUF.
Insert DATA at the indentation level LEVEL. For all possible
values of LEVEL, see `lister-determine-level'.

Return the marker of the added item's cursor gap position."
  (lister-insert lister-buf
		 (lister-next-free-position lister-buf)
		 data level))

(defun lister-add-sequence (lister-buf seq &optional level)
  "Add SEQ as items to LISTER-BUF with indentation LEVEL.
SEQ must be either a vector or a list.  Traverse SEQ and store its
elements as data into the newly created list items.  Any element of
the same type as SEQ will be interpreted as a nested list,
i.e. (item1 item2 (subitem1 subitem2) item3).

LEVEL determines the level of indentation. When LEVEL is nil,
insert SEQ at the level defined by the item at point. For all
possible values of LEVEL, see `lister-determine-level'.

Return the last inserted item marker."
  (lister-insert-sequence lister-buf nil seq level))

;; Remove 

(cl-defgeneric lister-remove (lister-buf position)
  "Remove the item at POSITION from LISTER-BUF.
POSITION can be either a buffer position or the symbol `:point'.")

;; This is the real function, all other variants are just wrappers:
(cl-defmethod lister-remove (lister-buf (position marker))
  "Remove the item at POSITION."
  (with-lister-buffer lister-buf
    (setq lister-local-marker-list 
	  (seq-remove (lambda (m) (= m position))
		      lister-local-marker-list)))
  (lister-remove-lines lister-buf position))


(cl-defmethod lister-remove (lister-buf (position (eql :point)))
  "Remove the item at point."
  (ignore position) ;; silence byte compiler
  (if-let* ((marker (lister-current-marker lister-buf)))
      (lister-remove lister-buf marker)
    (error "lister-remove: no item found at point")))

(cl-defmethod lister-remove (lister-buf (position integer))
  "Remove the item at POSITION from LISTER-BUF."
  (lister-remove lister-buf (lister-pos-as-marker position)))

(defun lister-level-at-item-index (lister-buf n)
  "Return the level of the nth item."
  (with-current-buffer lister-buf
    (get-text-property (elt lister-local-marker-list n) 'level)))

(defun lister-sublist-boundaries (lister-buf marker-or-pos)
  "Return the inner boundaries of the sublist containing MARKER-OR-POS.
Returns a list with a marker pointing to the first item of the
sublist, a second marker pointing to the last item of the
sublist, and the integer positions of the index positions
corresponding to these two items on `lister-local-marker-list'.

Example:
  (#<marker ....> #<marker ...> 0 3) ;; the first four items"
  (with-lister-buffer lister-buf
    (let* ((marker  (lister-pos-as-marker lister-buf marker-or-pos))
	   (n       (seq-position lister-local-marker-list marker #'equal))
	   (last-n  (1- (length lister-local-marker-list)))
	   (level   (get-text-property marker 'level))
	   (beg-n   (cl-loop for i downfrom n to 0
			     ;; to determine ONLY the same level, use =
			     while (<= level (lister-level-at-item-index lister-buf i))
			     finally return (1+ i)))
	   (end-n   (cl-loop for i upfrom n to last-n
			     while (<= level (lister-level-at-item-index lister-buf i))
			     finally return (1- i)))
	   (beg     (elt lister-local-marker-list beg-n))
	   (end     (elt lister-local-marker-list end-n)))
      (list beg end beg-n end-n))))

(defun lister-remove-this-level (lister-buf pos-or-marker)
  "Remove all surrounding items matching the level of the item at POS-OR-MARKER."
  (let* ((beg-end (lister-sublist-boundaries lister-buf pos-or-marker)))
    (with-current-buffer lister-buf
      (setq lister-local-marker-list
	    (append (seq-subseq lister-local-marker-list
				0 (third beg-end))
		    (seq-subseq lister-local-marker-list
				(min (length lister-local-marker-list)
				     (1+ (fourth beg-end))))))
      (let* ((inhibit-read-only t))
	(delete-region (first beg-end)
		       (lister-end-of-lines lister-buf 
					    (second beg-end)))))))

(defun lister-remove-sublist-below (lister-buf pos-or-marker)
  "Remove the sublist below the item at POS-OR-MARKER.
Do nothing if the next item is not a sublist."
  (when-let* ((next-item      (lister-end-of-lines lister-buf pos-or-marker))
	      (current-level  (get-text-property pos-or-marker 'level lister-buf))
	      (next-level     (get-text-property next-item 'level lister-buf)))
    (when (> next-level current-level)
      (lister-remove-this-level lister-buf next-item))))

;; Replace 

(cl-defgeneric lister-replace (lister-buf position data)
  "Replace the item at POSITION with one representing DATA.
POSITION can be either a buffer position or the symbol `:point'.
Preserve the indentation level.")

;; This is the real function, all other variants are just wrappers:
(cl-defmethod lister-replace (lister-buf (position integer) data)
  "Replace the item at POSITION with a new DATA item."
  (let* ((level      (get-text-property position 'level lister-buf)))
    (lister-remove-lines lister-buf position)
    (lister-insert lister-buf position data level)))

(cl-defmethod lister-replace (lister-buf (position (eql :point)) data)
  "Replace the item at point with a new DATA item."
  (ignore position) ;; silence byte compiler
  (if-let* ((marker (lister-current-marker lister-buf)))
      (lister-replace lister-buf marker data)
    (error "lister-replace: no item found at point")))

(cl-defmethod lister-replace (lister-buf (position marker) data)
  "Replace the item at POSITION with a new DATA item."
  (lister-replace lister-buf (lister-pos-as-integer marker) data))

;; Replace the whole list (set list)

(defun lister-set-list (lister-buf seq)
  "In LISTER-BUF, insert SEQ, leaving header and footer untouched.
SEQ can be nested to insert hierarchies."
  (save-lister-position lister-buf
    ;; delete old list:
    (when-let* ((ml lister-local-marker-list)
		;; (nth 0 ml) is always the first item,
		;; because header marker is stored in
		;; its own buffer local variable:
		(beg (nth 0 ml))
		(end (or lister-local-footer-marker 
			 (point-max)))
		(inhibit-read-only t))
      (delete-region beg end)
      (setq lister-local-marker-list nil))
    ;; insert new list:
    (lister-add-sequence lister-buf seq)))

;; -----------------------------------------------------------
;; * Marking and unmarking items
;; -----------------------------------------------------------

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


(cl-defgeneric lister-mark-item (lister-buf position value)
  "In LISTER-BUF, set the item's mark at POSITION to VALUE.")

;; This is the real function, all other variants are just wrappers:
(cl-defmethod lister-mark-item (lister-buf (position marker) value)
  "In LISTER-BUF, set the item's mark at POSITION to VALUE."
  (lister-set-prop lister-buf position 'mark value)
  (lister-display-mark-state lister-buf position)
  (when-let* ((next-item (lister-end-of-lines lister-buf position)))
    (unless (invisible-p next-item)
      (lister-goto lister-buf next-item))))

(cl-defmethod lister-mark-item (lister-buf (position (eql :point)) value)
    "In LISTER-BUF, set the item's mark at POSITION to VALUE."
  (ignore position) ;; silence byte compiler
  (when-let* ((m (lister-current-marker lister-buf)))
    (lister-mark-item lister-buf m value)))

(defun lister-mark-all-items (lister-buf value)
  "Set all items to the marking state VALUE in LISTER-BUF."
  (with-lister-buffer lister-buf
    (save-excursion
      (seq-do (lambda (m) (lister-mark-item lister-buf m value)) lister-local-marker-list))))

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
  "In LISTER-BUF, display the item as marked or not marked.
The item is referred to via the MARKER pointing to its cursor gap
position."
  (with-lister-buffer lister-buf
    (let* ((inhibit-read-only t)
	   (state    (lister-get-mark-state lister-buf marker))
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

;; -----------------------------------------------------------
;; * Setting and getting data
;; -----------------------------------------------------------

;; Generic property handling
(defun lister-set-prop (buf gap-pos prop value)
  "At GAP-POS, store VALUE in text property PROP."
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (put-text-property gap-pos (1+ gap-pos) prop value))))

(defun lister-get-prop (buf gap-pos prop)
  "Get VALUE from GAP-POS."
  (get-text-property gap-pos prop buf))

(defun lister-get-props-at (buf pos &rest props)
  "Return the values of all PROPS at POS in BUF."
  (seq-map (apply-partially #'lister-get-prop buf pos) props))

;; Set data

(cl-defgeneric lister-set-data (lister-buf position data)
  "Store the lisp object DATA at POSITION in LISTER-BUF.
POSITION can be a buffer position or the symbol `:point'.")

;; This is the real function, all other variants are just wrappers:
(cl-defmethod lister-set-data (lister-buf (position integer) data)
  "Store DATA at POSITION in LISTER-BUF."
  (lister-set-prop lister-buf position 'data data))

(cl-defmethod lister-set-data (lister-buf (position (eql :point)) data)
  "In LISTER-BUF, store DATA in the item at point."
  (ignore position) ;; silence byte compiler
  (if-let* ((marker (lister-current-marker lister-buf)))
      (lister-set-data lister-buf marker data)
    (error "lister-set-data: no item at point.")))

(cl-defmethod lister-set-data (lister-buf (position marker) data)
  "Insert DATA at POSITION in LISTER-BUF."
  (lister-set-data lister-buf (lister-pos-as-integer position) data))

;; Get data

(cl-defgeneric lister-get-data (lister-buf position)
  "Return the data stored at POSITION in LISTER-BUF.
POSITION can be either a buffer position or the symbol `:point'.")

;; This is the real function, all other variants are just wrappers:
(cl-defmethod lister-get-data (lister-buf (position marker))
  "Return the data stored at marker POSITION in LISTER-BUF."
  (lister-get-prop lister-buf position 'data))

(cl-defmethod lister-get-data (lister-buf (position integer))
  "Return the data stored at POSITION in LISTER-BUF."
  ;; creating a marker on the fly seems too much, since
  ;; `get-text-property' also accepts markers. Yet this way,
  ;; we also catch type errors.
  (lister-get-data lister-buf (lister-pos-as-marker lister-buf position)))

(cl-defmethod lister-get-data (lister-buf (position (eql :point)))
  "Retrieve the data of the item at point."
  (ignore position) ;; silence byte compiler
  (if-let* ((marker (lister-current-marker lister-buf)))
      (lister-get-data lister-buf marker)
    (error "lister-get-data: no item at point")))

;; Get lists of data:

(defun lister-get-all-data (lister-buf)
  "Collect all data values of all items in LISTER-BUF.
The values are collected in a flat list, ignoring any nested
levels or hierarchies."
  (seq-map (apply-partially #'lister-get-data lister-buf)
	   (buffer-local-value 'lister-local-marker-list lister-buf)))

(defun lister-get-visible-data (lister-buf)
  "Collect the data values of all items visible in LISTER-BUF."
  (seq-map (apply-partially #'lister-get-data lister-buf)
	   (lister-visible-markers lister-buf)))

;; TODO add option to also build a vector list
(cl-defun lister-group-by-level (l level-fn &optional (map-fn #'identity))
  "Build a tree from the flat list L.
L is a list of elements with no nesting. LEVEL-FN has to return
the intended nesting level for each element it is called with (as
an integer). Elements with the same level are treated as one
list; elements with higher levels are stored into sublists of
this list. MAP-FN can be used to additionally transform the
elements when building the tree.

Example:
   (lister-group-by-level '((a 0) (b 1) (c 1) (d 0)) #'second #'first)
 -> (a (b c) d)"
  (let* ((push-item  nil)
	 (item       (car l))
	 (level      (funcall level-fn item))
	 (res        (list (funcall map-fn item))) ;; change this for vectors
	 (walk       (cdr l)))
    (while walk
      (let* ((new-item  (car walk))
	     (new-level (funcall level-fn new-item)))
	  (if (> level new-level)
	      (setq walk nil)
	    (if (= level new-level)
		(setq push-item (funcall map-fn new-item)
		      walk (cdr walk))
	      (setq push-item (lister-group-by-level walk level-fn map-fn)
		    walk (seq-drop walk (length push-item))))
	    ;; change this for vectors 
	    (push push-item res))))
    (reverse res)))

  
(cl-defun lister-get-all-data-tree (lister-buf)
  "Collect all data values in LISTER-BUF, respecting its hierarchy."
  (let* ((data-list (seq-map (lambda (pos)
			       (lister-get-props-at lister-buf pos 'data 'level))
			     (buffer-local-value 'lister-local-marker-list lister-buf))))
      (lister-group-by-level data-list #'second #'first)))

;; -----------------------------------------------------------
;; * Moving point
;; -----------------------------------------------------------

;; Go to an item

(cl-defgeneric lister-goto (lister-buf position)
  "In LISTER-BUF, move point to POSITION.
POSITION is a buffer position or one of the symbols `:last' or
`:first'.")

;; This is the real function, all other variants are just wrappers:
(cl-defmethod lister-goto (lister-buf (position marker))
  "Move point to the marker POSITION."
  (unless position
    (error "lister-goto: expected marker, called with `nil'"))
  (with-lister-buffer lister-buf
    (if (invisible-p position)
	(error "lister-goto: item not visible.")
      (let ((previous-point (point)))
	(goto-char position)
	(lister-sensor-function (selected-window) previous-point 'left)
	(lister-sensor-function (selected-window) previous-point 'entered)))))

(cl-defmethod lister-goto (lister-buf (position integer))
  "Move point to POSITION."
  (lister-goto lister-buf (lister-pos-as-marker lister-buf position)))

(cl-defmethod lister-goto (lister-buf (position (eql :last)))
  "Move point to the last visible item in LISTER-BUF."
  (ignore position) ;; silence byte compiler
  (with-lister-buffer lister-buf
    (if-let* ((last-marker (car (last (lister-visible-markers lister-buf)))))
	(lister-goto lister-buf last-marker)
      (error "lister-goto: no visible item, cannot go to last item"))))

(cl-defmethod lister-goto (lister-buf (position (eql :first)))
  "Move point to the first visible item in LISTER-BUF."
  (ignore position) ;; silence byte compiler
  (with-lister-buffer lister-buf
    (if-let* ((first-marker (car (lister-visible-markers lister-buf))))
	(lister-goto lister-buf first-marker)
      (error "lister-goto: no visible item, cannot go to first item"))))

;; * Treat visible items as an indexed list

(defun lister-index-position (lister-buf marker-or-pos
					 &optional include-invisible)
  "Return the index position of MARKER-OR-POS.
The index only refers to visible items, unless INCLUDE-INVISIBLE
is t. Returns nil if no items are visible, or if MARKER-OR-POS is
not on an item."
  (with-lister-buffer lister-buf
    (when-let* ((mlist (if include-invisible lister-local-marker-list
			 (lister-visible-markers lister-buf))))
      (seq-position mlist
		    (lister-pos-as-marker lister-buf marker-or-pos)
		    #'equal))))

(defun lister-index-marker (lister-buf index-position)
  "Return the marker of INDEX-POSITION, if available."
  (with-lister-buffer lister-buf
    (seq-elt (lister-visible-markers lister-buf) index-position)))

;; -----------------------------------------------------------
;; * Marker 
;; -----------------------------------------------------------

(defun lister-add-marker (lister-buf marker-or-pos)
  "Add MARKER-OR-POS to the local markerlist of LISTER-BUF.
MARKER-OR-POS can be a marker or a pos, or a list of markers or
positions."
  (with-lister-buffer lister-buf    
    (setq lister-local-marker-list
	  (sort 
	   (thread-last (if (listp marker-or-pos) marker-or-pos (list marker-or-pos))
	     (mapcar (apply-partially #'lister-pos-as-marker lister-buf))
	     (append lister-local-marker-list))
	   #'<))))

;; FIXME currently unused, should be expanded to remove whole lists of markers
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
Also return nil if no marker is available."
  (with-lister-buffer lister-buf 
    (save-excursion
      (when (get-text-property (point) 'item)
	(seq-find (lambda (m)
		    (eq (marker-position m) (point)))
		  lister-local-marker-list)))))

(defun lister-first-lines (buf)
  "Return position of the first 'lines' element in BUF.
A 'lines' element can be the header, a list item or the footer."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (while (and (< (point) (point-max))
		  (not (get-text-property (point) 'item)))
	(goto-char (next-single-property-change (point) 'item nil (point-max))))
      (when (get-text-property (point) 'item)
	(point)))))

(defun lister-lines-positions (buf)
  "Return all positions of 'lines' elements in BUF.
A 'lines' element can be the header, a list item or the footer.
The resulting list will be in display order."
  (when-let* ((pos (lister-first-lines buf)))
    (with-current-buffer buf
      (save-excursion
	(goto-char pos)
	(let* ((result   (list pos))
	       (lines     nil))
	  ;; FIXME ersetzen durch 'nchars
	  (while (setq lines (get-text-property (point) 'nlines))
	    (forward-line lines)
	    (when (get-text-property (point) 'item)
	      (push (point) result)))
	  (reverse result))))))

(defun lister-marker-list (buf)
  "Create a list of markers for each 'lines' element in BUF.
A 'lines' element can be the header, a list item or the footer.
The resulting list will be in display order."
  (mapcar (apply-partially #'lister-make-marker buf)
	  (lister-lines-positions buf)))

(defun lister-next-free-position (lister-buf) 
  "Return the next free position for a new list item in LISTER-BUF."
  (with-lister-buffer lister-buf
    (let* ((ml     lister-local-marker-list))
      (cond
       ;; if there are any items, return the last item position:
       ;; (this is independent of an existing footer)
       (ml (lister-end-of-lines lister-buf (marker-position (car (last ml)))))
       ;; now is there a footer? return its position to insert next item there: 
       (lister-local-footer-marker (marker-position lister-local-footer-marker))
       ;; no footer, so insert after header, which is the end of the buffer:
       (lister-local-header-marker (point-max))
       ;; nothing there, just go to the beginning:
       (t (point-min))))))

;; * Creating Markers

(defun lister-make-marker (buf pos)
  "Return a suitable marker for POS in BUF."
  (let ((marker (make-marker)))
    (set-marker marker pos buf)
    (set-marker-insertion-type marker t)
    marker))

;; FIXME This function should never be necessary, it is kept
;; for easier debugging.
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

(defun lister-add-enter-callback (lister-buf callback-fn &optional append)
  "Register CALLBACK-FN as callback on entering an items."
  (with-current-buffer lister-buf
    (add-hook 'lister-enter-item-hook callback-fn append t)))

(defun lister-remove-enter-callback (lister-buf callback-fn)
  "Remove CALLBACK-FN from the list of callback functions."
  (with-current-buffer lister-buf
    (remove-hook 'lister-enter-item-hook callback-fn t)))

(defun lister-add-leave-callback (lister-buf callback-fn)
  "Register CALLBACK-FN as callback on leaving an item."
  (with-current-buffer lister-buf
    (add-hook 'lister-leave-item-hook callback-fn nil t)))

(defun lister-add-leave-callback (lister-buf callback-fn)
  "Remove CALLBACK-FN as callback on leaving an item."
  (with-current-buffer lister-buf
    (add-hook 'lister-leave-item-hook callback-fn nil t)))

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

(defun lister-key-action ()
  "Do something with the item at point."
  (interactive)
  (if-let* ((fn lister-local-action))
      (funcall lister-local-action (point))
    (message "No action defined")))

(defvar lister-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "m" 'lister-key-toggle-mark)
    (define-key map "*" 'lister-key-mark-all-items)
    (define-key map "u" 'lister-key-unmark-all-items)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map (kbd "RET") #'lister-key-action)
    map)
  "Key map for `lister-mode'.")

(define-derived-mode lister-mode
  special-mode "Lister"
  "Major mode for selecting list items."
  :group 'lister
  (cursor-sensor-mode)
  (cursor-intangible-mode))

;; * Set up a lister buffer

;;;###autoload
(defun lister-setup (buf mapper-fn &optional data-list
			 header footer
			 filter-function
			 major-mode-fn)
  "Set up BUF to display DATA-LIST using MAPPER-FN.

DATA-LIST is a list of data objects which will be passed to
MAPPER-FN. 

MAPPER-FN must accept only one argument, the data object, and
must return either a string or a list containing strings or
function symbols. See `lister-insert-lines' for the exact format
of the return value.

Optional argument HEADER is a string or a list of strings to be
inserted at the top of the list. See `lister-insert-lines' for
the exact format.

Optional argument FOOTER is a string or a list of strings to be
inserted at the end of the list. See `lister-insert-lines' for
the exact format.

Optional argument FILTER-FUNCTIONS defines a filter function
which has to turned on using `lister-activate-filter' to become
effective.

Set the major mode to `lister-mode' or call MAJOR-MODE-FN as a
function to set the major mode. The major mode has to be a mode
derived from `lister-mode'.

Move point to the first list item. 

Return BUF."
  (with-current-buffer buf
    ;; first of all, set the major mode
    (funcall (or major-mode-fn 'lister-mode))
    ;; prepare the buffer:
    (setq lister-local-mapper mapper-fn)
    (setq lister-enter-item-hook nil
	  lister-leave-item-hook nil)
    (setq buffer-undo-list t)
    (setq lister--ignore-next-sensor-event nil)
    (let ((cursor-sensor-inhibit t)
	  (inhibit-read-only t))
      (erase-buffer))
    (setq lister-local-filter-term
	  (when filter-function
	    (lister-add-filter-term nil filter-function 'and)))
    ;; ready to add header, list and footer:
    (when header
      (lister-set-header buf header))
    (when footer
      (lister-set-footer buf footer))
    (when data-list
      (lister-set-list buf data-list))
    ;; move to first item:
    (if (lister-visible-markers buf)
	(lister-goto buf :first)
      (goto-char (point-min)))
    buf))

(provide 'lister)
;;; lister.el ends here
