;;; lister.el --- Yet another list printer             -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2021

;; Author:  <joerg@joergvolbers.de>
;; Version: 0.5
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

;; `Lister` is a library for creating interactive "lists" of any
;; kind. In contrast to similar packages like `hierarchy.el` or
;; `tablist.el`, it aims at *not* simply mapping a data structure to
;; a navigatable list. Rather, it treats the list like Emacs treats
;; buffers: It is an empty space to which you can successively add
;; stuff. So in Emacs lingo, `lister` should be rather called
;; `listed` - it is a library for *editing* lists, instead of
;; displaying them.

;; For more information, read the README.org shipped with that package.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'cursor-sensor)

;; -----------------------------------------------------------
;; * Variables
;; -----------------------------------------------------------

;; * Version:

(defvar lister-version "0.5"
  "Version number.")

;; * Local Variables:

(defvar-local lister-local-mapper nil
  "Function which converts any DATA object to a list of strings.")

(defvar-local lister-local-action nil
  "Function which gets called 'on' an item to do something with it.")

(defvar-local lister-local-filter-fn nil
  "A filter function accepting as its only argument the data of each item.
If this function returns nil, the corresponding item will be
hidden.")

(defvar-local lister-local-marking-predicate nil
  "Function which decides if an item can be marked at all.
If there is no function, just treat any item as subject to
marking. The function has to be called with one argument: the
associated data. The item will be exempt from marking if the
return value is nil.")

(defvar-local lister-local-header-marker nil
  "Stores the marker for the upper left corner of the header.")

(defvar-local lister-local-footer-marker nil
  "Stores the marker for the upper left corner of the footer.")

(defvar-local lister-local-marker-list nil
  "Stores a list of marker positions for each lister list item.")

(defvar-local lister-local-left-margin 2
  "Add this left margin when inserting a item.
Set this to nil if no left margin is wanted.")

(defvar-local lister-local-top-margin nil
  "Add this top margin when inserting an item.
Set this to nil if no top margin is wanted.

NOTE: This feature seems quite useless, it will probably be
removed soon.")

(defvar-local lister-local-bottom-margin nil
  "Add this bottom margin when inserting an item.
Set this to nil if no bottom margin is wanted.

NOTE: This feature seems quite useless, it will probably be
removed soon.")

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

(defvar-local lister-sensor-last-item nil
  "Last item on which the sensor function has been applied.")

;; * Global Variables:

(defvar lister-inhibit-cursor-action nil
  "Bind this to inhibit updating the cursor while inserting items.")

(defvar lister-inhibit-marker-list nil
  "Bind this to inhibit updating the marker list while inserting items.")

(defvar lister-cursor-locked nil
  "Execution is within a `lister-with-locked-cursor' macro.
Used internally to avoid duplicate calls of
`lister-with-locked-cursor'. Don't set this variable.")

;; * Customizable Global Variables:

;; TODO Change to defcustom
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

;; -----------------------------------------------------------
;; * Working with text properties

(defun lister-add-face-property (beg end value &optional append)
  "Add VALUE to the face property between BEG and END."
  (add-face-text-property beg end value append))

(defun lister-remove-face-property (beg end value)
  "Remove VALUE from the face property from BEG to END.
This is a slightly modified copy of `font-lock--remove-face-from-text-property'."
  (let ((beg (text-property-not-all beg end 'face nil))
	next prev)
    (while beg
      (setq next (next-single-property-change beg 'face nil end)
	    prev (get-text-property beg 'face))
      (cond ((or (atom prev)
		 (keywordp (car prev))
		 (eq (car prev) 'foreground-color)
		 (eq (car prev) 'background-color))
	     (when (eq value prev)
	       (remove-list-of-text-properties beg next (list 'face))))
	    ((memq value prev)		;Assume prev is not dotted.
	     (let ((new (remq value prev)))
	       (cond ((null new)
		      (remove-list-of-text-properties beg next (list 'face)))
		     ((= (length new) 1)
		      (put-text-property beg next 'face (car new)))
		     (t
		      (put-text-property beg next 'face new))))))
      (setq beg (text-property-not-all next end 'face nil)))))

(defun lister-add-props (buf pos-or-marker &rest props)
  "Add PROPS as text properties at POS-OR-MARKER.
PROPS is a list of properties and values."
  (with-current-buffer buf
    (let* ((inhibit-read-only t)
	   (pos (lister-pos-as-integer pos-or-marker)))
      (add-text-properties pos (1+ pos) props))))

(defun lister-remove-props (buf pos-or-marker &rest props)
  "Remove PROPS from POS-OR-MARKER.
PROPS is a list of property-value-pairs, but only the property
field is used. See `remove-text-properties', which is called."
  (when props
    (with-current-buffer buf
      (let* ((inhibit-read-only t)
	     (pos (lister-pos-as-integer pos-or-marker)))
	(remove-text-properties pos (1+ pos) props)))))

(defun lister-get-prop (buf pos-or-marker prop)
  "Get VALUE from POS-OR-MARKER."
  (let* ((pos (lister-pos-as-integer pos-or-marker)))
    (get-text-property pos prop buf)))

(defun lister-get-props-at (buf pos &rest props)
  "Return the values of all PROPS at POS in BUF."
  (seq-map (apply-partially #'lister-get-prop buf pos) props))

;; * Finding properties in other items

(defun lister-looking-at-prop (lister-buf pos-or-marker prop direction)
  "Looking at the previous or next item, return position of PROP.
If there is no property, return nil.

DIRECTION can be the symbol `previous' or the symbol `next'.

This function assumes that POS-OR-MARKER is pointing to the
cursor gap of an item.

LISTER-BUF is a lister buffer."
  (let (pos)
    (if (eq direction 'previous)
	;; looking back:
	(let* ((limit (lister-item-min lister-buf)))
	  (if (= limit pos-or-marker)
	      (setq pos nil)
	    (setq pos (previous-single-property-change
		       pos-or-marker
		       prop
		       lister-buf
		       limit))
	    (setq pos (and pos (max 1 (1- pos))))))
      ;; looking towards the end:
      (let* ((limit (lister-item-max lister-buf)))
	(if (= limit pos-or-marker)
	    (setq pos nil)
	  (setq pos (next-single-property-change
		     pos-or-marker
		     prop
		     lister-buf
		     limit))
	  (setq pos (and pos
			 (next-single-property-change
			  pos
			  prop
			  lister-buf
			  limit))))))
    ;;
    pos))

;; -----------------------------------------------------------
;; * Safety checks

(defun lister-item-p (lister-buf pos-or-symbol)
  "Check if POS-OR-SYMBOL points to a lister item in LISTER-BUF."
  ;; lister-marker-at checks for text property 'item 
  (not (null (lister-marker-at lister-buf pos-or-symbol))))

(defun lister-buffer-p (buf)
  "Return BUF if it is ready to be used for lister lists.
Throw an error if BUF is not in `lister mode' or a major mode
derived from it. Also cancel if the local mapper function is not
defined."
  (unless buf
    (error "Expected buffer, got nil"))
  (with-current-buffer buf
    (or
     (and (derived-mode-p 'lister-mode)
	  lister-local-mapper
	  buf)
     (error
      (if (not (derived-mode-p 'lister-mode))
	  "Buffer %s has to be in lister mode; execution aborted."
	"Buffer %s has to have a local mapper function; execution aborted.")
      buf))))

(defmacro with-lister-buffer (buf &rest body)
  "Execute BODY in BUF.
Throw an error if BUF is not a lister buffer."
  (declare (indent 1) (debug t))
  `(with-current-buffer (lister-buffer-p ,buf)
     ,@body))

;; -----------------------------------------------------------
;; * Utilities for markers and positions

;; Build marker, convert positions

(defun lister-make-marker (buf pos)
  "Create a suitable marker for POS in lister buffer BUF."
  (let ((marker (make-marker)))
    (set-marker marker pos buf)
    (set-marker-insertion-type marker t)
    marker))

(defun lister-pos-as-integer (marker-or-pos)
  "Get the integer value from MARKER-OR-POS."
  (if (markerp marker-or-pos)
      (marker-position marker-or-pos)
    marker-or-pos))

(defun lister-pos-as-marker (lister-buf marker-or-pos)
  "Return the marker MARKER-OR-POS or create one.
LISTER-BUF is a lister buffer."
  (if (markerp marker-or-pos)
      marker-or-pos
    (lister-make-marker lister-buf marker-or-pos)))

;; Add marker to the buffer local marker list

(defun lister-merge (target new)
  "Merge incrementally sorted marker list NEW into TARGET."
  (if-let ((target-pos (cl-position (car new) target :test #'<)))
      (append (seq-subseq target 0 target-pos)
	      new
	      (seq-subseq target target-pos))
    (append target new)))

;; REVIEW Still probably too cumbersome and not fast enough.
;;        Do a test for the variants.
(defun lister-add-item-marker (lister-buf marker-or-pos)
  "Add MARKER-OR-POS to the local marker list of LISTER-BUF.
MARKER-OR-POS can be a marker or a pos, or a sorted homogenous
list of only markers or only positions.

Do nothing if `lister-inhibit-marker-list' is t.

Some special assumptions apply for reasons of speed: (1.) Both
the buffer local marker list and MARKER-OR-POS are already sorted
incrementally. (2.) There is no overlapping item in both lists,
that is, all markers are different from each other. This way, the
two lists can be simply merged, and it not necessary to sort the
resulting list (even though using 'sort' actually is not that
much slower).

Since markers move when some new text is inserted before them,
condition (2.) is always true when adding markers representing
the new text."
  (unless (or lister-inhibit-marker-list
	      (not marker-or-pos))
    ;; NOTE Actually the whole stuff below might seem totally useless.
    ;; Simply updating the marker list by setting its value to the
    ;; result of `lister-rescan-item-markers` works as well and does not take
    ;; more time (!). 
    (let* ((m-o-p-list    (if (listp marker-or-pos)
			      marker-or-pos
			    (list marker-or-pos)))
	   (marker-as-list (if (markerp (car m-o-p-list))
			       m-o-p-list
			     (mapcar (apply-partially #'lister-pos-as-marker lister-buf)
				     m-o-p-list))))
      (with-current-buffer lister-buf
	(setq lister-local-marker-list
	      (lister-merge lister-local-marker-list marker-as-list))))))

;; Finding positions

(defun lister-eval-pos-or-symbol (lister-buf position-or-symbol)
  "Return a marker position evaluating POSITION-OR-SYMBOL.
POSITION-OR-SYMBOL can itself be a marker, or an integer, or the
symbols `:first', `:last' or `:point'.

LISTER-BUF must be a set up lister buffer.

Note that this function only interpretes POSITION-OR-SYMBOL. It
does not check whether the position found is valid."
  (let* ((pos
	  (cond
	   ;; the two most likely use cases first:
	   ((markerp position-or-symbol)    position-or-symbol)
	   ((integerp position-or-symbol)   position-or-symbol)
	   ;; now the keyword cases:
	   ((eq position-or-symbol :first)  (lister-item-min lister-buf))
	   ((eq position-or-symbol :point)  (with-current-buffer lister-buf (point)))
	   ((eq position-or-symbol :last)
	    (when-let*
		;; This is faster than traversing the whole marker
		;; list to retrieve the last item.
		((last-pos (lister-item-max lister-buf))
		 (last-pos (previous-single-property-change last-pos
							    'item
							    lister-buf
							    (lister-item-min lister-buf))))
	      (1- last-pos)))
	   (t
	    (error "Unknown value for POSITION-OR-SYMBOL: %s"
		   position-or-symbol)))))
    (and pos
	 (lister-pos-as-marker lister-buf pos))))

(defun lister-marker-at (lister-buf position-or-symbol)
  "In LISTER-BUF, return marker according to POSITION-OR-SYMBOL.
Return nil if there is no item at the desired position.

If POSITION-OR-SYMBOL is one of the symbols `:first', `:last' or
`:point', return the position of the first item, the last item or
the item at point, respectively.

If POSITION-OR-SYMBOL is a marker, return it unchanged iff there
is an item.

If POSITION-OR-SYMBOL is an integer, treat it as a buffer
position and return a marker representing it iff there is an
item."
  (when-let* ((m (lister-eval-pos-or-symbol lister-buf
					    position-or-symbol)))
    (and (get-text-property (lister-pos-as-integer m)
			    'item
			    lister-buf)
	 m)))

(defun lister-item-min (lister-buf)
  "Return the first position for a list item in LISTER-BUF.
This is intended to be similar to `point-min'."
  (with-lister-buffer lister-buf
    (if lister-local-header-marker
	(lister-end-of-lines lister-buf lister-local-header-marker)
      (point-min))))

(defun lister-item-max (lister-buf)
  "Return the end of the last item in LISTER-BUF.
This is intended to be similar to `point-max'."
  (with-lister-buffer lister-buf
    (if lister-local-footer-marker
	(1- (marker-position lister-local-footer-marker))
      (point-max))))

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

(defun lister-index-position (lister-buf marker-or-pos)
  "Get index position (starting with 0) of the item at MARKER-OR-POS.
Return nil if MARKER-OR-POS in LISTER-BUF is not pointing to an
item."
  (cl-position marker-or-pos
	       (buffer-local-value 'lister-local-marker-list lister-buf)
	       :test #'=))

(defun lister-index-marker (lister-buf index-position)
  "Get the marker for INDEX-POSITION (starting with 0) in LISTER-BUF.
Return nil if no such position is available."
  (elt (buffer-local-value 'lister-local-marker-list lister-buf)
       index-position))

(cl-defun lister-rescan-item-markers (lister-buf &optional (prop 'item))
  "Get a freshly build list of all item markers in LISTER-BUF.
Items are identified by checking the property PROP. PROP defaults
to `item', meaning that this function matches all regular items."
  (with-current-buffer lister-buf
    (let (res (pos (point-min)) (max (point-max)))
      (while (< pos max)
	(when (get-text-property pos prop)
	  (push (lister-make-marker lister-buf pos) res)
	  (setq pos (next-single-property-change pos prop nil max)))
	(setq pos (next-single-char-property-change pos prop nil max)))
      (reverse res))))

;; -----------------------------------------------------------
;; * MACRO Lock cursor during longer transactions:

(defmacro lister-with-locked-cursor (buf &rest body)
  "Keep cursor at same position after executing BODY.

Turn off the cursor sensor, execute BODY, and then try to set the
cursor back at its old position. If this position is not
available anymore, move cursor to the end of the list. Then
re-activate the cursor sensor. Return the result of BODY.

If this macro is called within the BODY of this macro, just
execute body directly.

BUF is a lister buffer. Note that this function does NOT make BUF
current."
  (declare (indent 1) (debug (sexp body)))
  ;;
  (let* ((line-idx-var    (make-symbol "line-idx"))
	 (buffer-var      (make-symbol "buffer"))
	 (result-var      (make-symbol "result"))
	 (get-point      `(with-current-buffer ,buffer-var (point)))
	 (get-line-idx   `(lister-index-position ,buffer-var ,get-point)))
    ;;
    `(cl-labels ((body-fn () ,@body))
       (if lister-cursor-locked
	   (body-fn)
	 ;; NOTE: binding buf here avoids pitfalls when buffer is
	 ;; changed in body-fn (and buf would be, say,
	 ;; "(current-buffer)")
	 (let (,result-var
	       (,buffer-var ,buf))
	   (lister-sensor-leave ,buffer-var)
	   (let* ((lister-cursor-locked t)         ;; don't nest
		  (lister-inhibit-cursor-action t) ;; no actions
		  (cursor-sensor-inhibit t)        ;; no sensor
		  ;; current line: if nil, assume first pos:
		  (,line-idx-var (or ,get-line-idx 0)))
	     (setq ,result-var (body-fn))
	     ;; we can't use lister-goto, since target line might be
	     ;; hidden now
	     (let (new-pos)
	       ;; jump only to visible lines if filter is active:
	       (if (buffer-local-value 'lister-local-filter-fn ,buffer-var)		   
		   (setq new-pos (when-let ((vis-items (lister-visible-items ,buffer-var)))				   
				   (or (elt vis-items ,line-idx-var)
				       (car (last vis-items)))))
		 ;; else, jump to same line or the last element:
		 (setq new-pos (or (lister-index-marker ,buffer-var ,line-idx-var)
				   (lister-eval-pos-or-symbol ,buffer-var
							      :last))))
	       (with-current-buffer ,buffer-var
		 (goto-char (or new-pos
				(lister-item-min ,buffer-var))))))
	   (lister-sensor-enter ,buffer-var)
	   ,result-var)))))

;; -----------------------------------------------------------
;; * Building the list using 'lines'

;; These are the core primitives. The following functions either
;; insert, remove or replace lines of text, usually passed to these
;; functions as a list of strings.

;; use this instead of flatten-tree for emacsen < 27.1:
(defun lister--flatten (l)
  "Flatten the list L, removing any null values.
This is a simple copy of dash's `-flatten' using `seq'."
  (if (and (listp l) (listp (cdr l)))
      (seq-mapcat #'lister--flatten l)
    (list l)))

(cl-defun lister-strflat (l &optional (format-string "%s"))
  "Recursively stringify all items in L, flattening any sublists.
If L is not a list item, wrap it into a list. Every non-nil item
will be passed to FORMAT-STRING, which defaults to \"%s\"."
  (mapcar (apply-partially #'format format-string)
	  (lister--flatten l)))

(defun lister-add-vertical-margins (lister-buf strings)
  "Pad a list of STRINGS vertically by adding empty strings.
Margins are taken from `lister-local-top-margin' and
`lister-local-bottom-margin', buffer variables local to
LISTER-BUF."
  (with-current-buffer lister-buf
      (append
       (and lister-local-top-margin
	    (make-list lister-local-top-margin ""))
       strings
       (and lister-local-bottom-margin
	    (make-list lister-local-bottom-margin "")))))
  
(cl-defun lister-insert-lines (buf marker-or-pos lines level)
  "Insert flattened list LINES with padding LEVEL at POS in BUF.
MARKER-OR-POS can be either a marker object or a buffer position.
LINES must be a list. LEVEL, an integer, adds extra padding to
the item (e.g. to mark it as a subitem).

If LINES is nil, do nothing.

Mark the inserted text as `intangible', but leave a gap for the
cursor to access the item. Store some important values at the
position of the gap. Move point to the end of the newly inserted
text.

If an item has been inserted, return the marker pointing to its
gap position."
  (when lines
    (with-current-buffer buf
      (let* ((padding           (make-string (+ (or lister-local-left-margin 0) (or level 0)) ? ))
	     (item-list         (lister-add-vertical-margins buf (lister-strflat lines (concat padding "%s"))))
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
	;; item.
	;;
	;; Calling `Lister-set-props' adds too much overhead, we add
	;; the properties directly: 
	(add-text-properties beg (1+ beg)
			     (list 'item t
				   'level (or level 0)
				   'nchars (- (point) beg)))
	(lister-make-marker buf beg)))))

(defun lister-remove-lines (buf marker-or-pos)
  "Remove the 'lines' element beginning at MARKER-OR-POS in BUF.
A 'lines' element can be the header, a list item or the footer."
  (with-current-buffer buf
    (let* ((inhibit-read-only t)
	   (cursor-sensor-inhibit t))
      (delete-region marker-or-pos (lister-end-of-lines buf marker-or-pos)))))

(defun lister-replace-lines (buf marker-or-pos new-lines)
  "In BUF, Replace the 'lines' element at MARKER-OR-POS with NEW-LINES.
A 'lines' element can be the header, a list item or the footer.

If NEW-LINES is nil, delete the entry at MARKER-OR-POS.

Return the marker of the new item or nil if no item has been
inserted."
  (let ((level (get-text-property marker-or-pos 'level buf)))
    (lister-remove-lines buf marker-or-pos)
    (lister-insert-lines buf marker-or-pos new-lines level)))

(defun lister-end-of-lines (buf marker-or-pos &optional no-error)
  "Get the end position of the 'lines' element at MARKER-OR-POS in BUF.
A 'lines' element can be the header, a list item or the footer.

Effectively, the value returned is the position of the cursor gap
of the next item (if there is any).

Internally, the text property symbol `nchars' is used to
determine the size of the item. An error will be thrown if this
text property is not avaible. You can turn that off by setting
NO-ERROR."
  (if-let* ((nchars (lister-get-prop buf marker-or-pos 'nchars)))
      (+ marker-or-pos nchars)
    (if no-error
        (lister-pos-as-integer marker-or-pos)
      (error "Did not find text property 'nchars at buffer position %d"
	     (lister-pos-as-integer marker-or-pos)))))

;; -----------------------------------------------------------
;; * Static items

;; Basic functions for dealing with so-called static items. Best
;; examples for static items are the list header and the list footer.
;; More generally, 'static' items are distinguished from normal list
;; items by two features:
;;
;;  (1.) They have no cursor gap. Thus, they are completely
;; "unreachable" with the usual navigation tools. Accordingly, they
;; are not marked as an item and are not part of the
;; `lister-local-marker-list'. Thus they are effectively hidden from
;; view for all functions which act on regular list items.
;;
;;  (2.) Their content is printed as-is, not using the local mapper.
;;

(defun lister-make-item-static (lister-buf marker-or-pos &rest props)
  "In LISTER-BUF, mark the item at MARKER-OR-POS as a static item.
Basically, that means (a) to close the cursor gap, and (b) to
remove the text property 'item' and to replace it with the text
property 'static'.

Optionally also use PROPS to add extra property-value-pairs to be
set at the cursor gap position (i.e. to make the item easily
detectable by searching for text properties)."
  (apply #'lister-add-props lister-buf marker-or-pos
	 'item nil
	 'static t
	 'cursor-intangible t
	 'front-sticky t
	 props))

;; TODO Write tests
(defun lister-insert-static-item (lister-buf position-or-symbol data &optional level)
  "Insert DATA as a static item at POSITION-OR-SYMBOL in LISTER-BUF.
POSITION-OR-SYMBOL must be either an integer position, a marker,
or one of the symbols `:first', `:last' or `:point'. DATA must be
a list of strings which will be inserted unmodified. Optional
argument LEVEL determines the indentation level of the item,
defaulting to 0.

Return the marker of the inserted item."
  (let ((cursor-sensor-inhibit t))
    (lister-sensor-leave lister-buf)
    (let* ((pos    (lister-eval-pos-or-symbol lister-buf position-or-symbol))
	   (marker (lister-insert-lines lister-buf
					pos
					data
					(lister-determine-level lister-buf
								pos
								(or level 0)))))
      (lister-make-item-static lister-buf marker)
      ;; TODO Hide this static item iff its associate is also hidden.
      (with-current-buffer lister-buf
	(goto-char marker))
      (lister-sensor-enter lister-buf)
      marker)))

;; -----------------------------------------------------------
;; * Set header or footer of the list

;; Headers or footers are static items placed at the end or the
;; beginning of the list. Headers or footers have their own text
;; property, the property 'static is removed.

(defun lister-make-header-or-footer (lister-buf marker-or-pos)
  "Mark the item at MARKER-OR-POS to be a header or footer."
  (lister-make-item-static lister-buf marker-or-pos
			   'header-or-footer t
			   'static nil))

(defun lister-set-header (lister-buf header)
  "Insert or replace HEADER before the first item in LISTER-BUF."
  (with-current-buffer lister-buf
    (and 
     (setq lister-local-header-marker
	   (if lister-local-header-marker
	       (lister-replace-lines lister-buf lister-local-header-marker header)
	     (lister-insert-lines lister-buf (point-min) header 0)))
     (lister-make-header-or-footer lister-buf lister-local-header-marker))))

(defun lister-set-footer (lister-buf footer)
  "Insert or replace FOOTER after the last item of LISTER-BUF."
  (with-current-buffer lister-buf
    (and 
     (setq lister-local-footer-marker
	   (if lister-local-footer-marker
	       (lister-replace-lines lister-buf lister-local-footer-marker footer)
	     (lister-insert-lines lister-buf (point-max) footer 0)))
     (lister-make-header-or-footer lister-buf lister-local-footer-marker))))


;; -----------------------------------------------------------
;; * Filtering

;; Showing and hiding items

(defun lister-set-item-invisibility (lister-buf marker-or-pos value)
  "In LISTER-BUF, show or hide the item at MARKER-OR-POS.
The VALUE t hides the item, nil makes it visible."
  (with-current-buffer lister-buf
    (let* ((inhibit-read-only t)
	   (cursor-sensor-inhibit t)
	   (beg (lister-pos-as-integer marker-or-pos))
	   (end (lister-end-of-lines lister-buf marker-or-pos)))
      (put-text-property beg end 'invisible value)
      ;; this closes the gap for the marker:
      (put-text-property beg (1+ beg) 'front-sticky value))))

(defun lister-show-item (lister-buf marker-or-pos)
  "In LISTER-BUF, set the item at MARKER-OR-POS as visible."
  (lister-set-item-invisibility lister-buf marker-or-pos nil))

(defun lister-hide-item (lister-buf marker-or-pos)
  "In LISTER-BUF, set the item at MARKER-OR-POS as invisible."
  (lister-set-item-invisibility lister-buf marker-or-pos t))

(defun lister-item-invisible-p (lister-buf marker-or-pos)
  "Check if MARKER-OR-POS in LISTER-BUF is visible.

This is just wrapper for calling `invisible-p'. If you have the
buffer current, you might as well that function directly."
  (with-current-buffer lister-buf
    (invisible-p (lister-pos-as-integer marker-or-pos))))

;; Access only the hidden or visible items

(defun lister-invisible-items (lister-buf)
  "Get all markers pointing only to hidden items in LISTER-BUF."
  (with-lister-buffer lister-buf
    (seq-filter (lambda (m)
		  ;; Since the marker position is the place for
		  ;; accessing the item with the cursor, we can safely
		  ;; assume that if the marker position is invisible,
		  ;; the whole item is invisible:
		  (text-property-any m (1+ m) 'invisible t))
		lister-local-marker-list)))

(defun lister-visible-items (lister-buf)
  "Get all markers pointing only to visible items in LISTER-BUF."
  (with-lister-buffer lister-buf
    (seq-filter (lambda (m)
		  ;; Since the marker position is the place for
		  ;; accessing the item with the cursor, we can safely
		  ;; assume that if the marker position is invisible,
		  ;; the whole item is invisible:
		  (text-property-any m (1+ m) 'invisible nil))
		lister-local-marker-list)))

;; * The actual filtering:

;; REVIEW Maybe this could be moved completely into the insert
;;        function. Is it used anywhere else?
(defun lister-maybe-hide-item (lister-buf marker-or-pos data)
  "Hide item at MARKER-OR-POS if it matches the local filter.

Hide item if the result of applying the local filter function
returns nil; else leave it untouched.

Since is for hiding items after inserting it. If you want to
either hide or show an item, use `lister-set-item-invisibility'."
  (when-let ((fn (buffer-local-value 'lister-local-filter-fn lister-buf)))
    (unless (funcall fn data)
      (lister-hide-item lister-buf marker-or-pos))))

(defun lister-filter--all-items (lister-buf filter-fn)
  "In LISTER-BUF, set visibility of each item according to FILTER-FN.

FILTER-FN must accept one argument, the item's data. It is called
with point on the item examined. If FILTER-FN returns t, show
the item; else hide it.

This is an internal function which does NOT store the filter in
the buffer. For setting or removing the filter, use
`lister-set-filter' instead."
  (let* ((w-fn (lambda (data)
		 (lister-set-item-invisibility (current-buffer)
					       (point)
					       (not (funcall filter-fn data))))))
    (lister-walk-all lister-buf w-fn)))

(defun lister-set-filter (lister-buf filter-fn)
  "Activate FILTER-FN, replacing any previous filter settings.

In LISTER-BUF, only show items where FILTER-FN, called with
the item's data, returns non-nil values. Subsequent insertions of
new items will respect this filter.

If FILTER-FN is nil, delete the filter and restore visibility for
all items."
  (with-current-buffer lister-buf
    ;; do not act if there is no filter passed and no filter active
    (when (or filter-fn lister-local-filter-fn)
      ;; else update all items
      (if (setq lister-local-filter-fn filter-fn)
	  ;; that is: either apply the filter per item
	  (lister-filter--all-items lister-buf filter-fn)
	;; or show all items again if there is no filter anymore
	(lister-walk-all lister-buf
			 (lambda (_)
			   (lister-show-item (current-buffer)
					     (point))))))))

;;; -----------------------------------------------------------
;;; * Insert, add, remove or replace list items

;; Utilities for insertion 

;; REVIEW Who calls this function? Is it any other function than
;;        insert-lines?
;;
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
at point or 0 if there is no such item.

If the result value is below 0, always return 0.

LISTER-BUF is a lister buffer."
  (let* ((item-level (get-text-property pos-or-marker 'level lister-buf))
	 (prev-pos   (lister-looking-at-prop lister-buf pos-or-marker 'level 'previous))
	 (prev-level (and prev-pos (get-text-property prev-pos 'level lister-buf))))
    (max 0 
	 (cond
	  ((null prev-level)      0) ;; there's no previous level, thus no indentation
	  ((null level)           prev-level)
	  ((eq level :previous)   prev-level)
	  ((eq level :current)    (or item-level 0))
	  ((> level prev-level)   (1+ prev-level))
	  (t                      level)))))

;; Insert Single Items

(defun lister-insert (lister-buf position-or-symbol data &optional level)
    "Insert DATA as item at POSITION-OR-SYMBOL in LISTER-BUF.
POSITION-OR-SYMBOL must be a buffer position, a marker, or the
symbols `:point', `:first' or `:last'. The indicated position
will not be checked for validity.

Insert DATA at the indentation level LEVEL. For the possible
values of LEVEL, see `lister-determine-level'.

Return the marker of the inserted item's front cursor gap
position (the position 'of' the inserted item itself).

Note that to insert a new item at a position means to move any
existing items at this position further down. Thus, `:last'
effectively inserts an item before the last item. If you want to
add an item to the end of the list, use `lister-add'."
  (let* ((cursor-sensor-inhibit t))
    (lister-sensor-leave lister-buf)
    (let* ((marker-or-pos (lister-eval-pos-or-symbol lister-buf position-or-symbol))
	   (marker        (lister-insert-lines lister-buf
					       marker-or-pos
					       (funcall (buffer-local-value 'lister-local-mapper lister-buf) data)
					       (lister-determine-level lister-buf marker-or-pos (or level 0)))))
      ;;
      (lister-set-data lister-buf marker data)
      (lister-add-props lister-buf marker
			'cursor-sensor-functions
			'(lister-sensor-function))
      (lister-maybe-hide-item lister-buf marker data)
      (lister-add-item-marker lister-buf marker)
      (with-current-buffer lister-buf
	(goto-char marker))
      (lister-sensor-enter lister-buf)
      marker)))

;; Insert sequences of items

(defun lister-insert-sequence (lister-buf pos-or-marker seq &optional level)
  "Insert SEQ at POS-OR-MARKER in LISTER-BUF.
Insert SEQ above the item marked by POS-OR-MARKER. If
POS-OR-MARKER is nil, add it to the end of the list.

LEVEL determines the level of hierarchical indentation. See
`lister-determine-level' for all possible values for LEVEL.

SEQ must be either a vector or a list. Nested sequences will be
inserted with added indentation.

Return an incrementally sorted list of the newly inserted
markers."
  (when seq
    (let* ((new-marker     nil)
	   (last-pos       nil)
	   (seq-type     (type-of seq)))
      (unless (member seq-type '(vector cons))
	(error "Sequence must be a vector or a list"))
      (lister-sensor-leave lister-buf)
      (let* ((lister-inhibit-cursor-action t)
	     (lister-inhibit-marker-list t)
	     (cursor-sensor-inhibit t)
	     (pos          (or pos-or-marker (lister-next-free-position lister-buf)))
	     (new-level    (lister-determine-level lister-buf pos level)))
	(seq-doseq (item seq)
	  ;; For reasons of speed, we build the new marker list in the
	  ;; 'wrong' decremental order and reverse it afterwards.
	  ;; Accessing the last inserted marker via (car) is muuuuch
	  ;; faster than using (car (last)), since the latter has to
	  ;; traverse the whole list.
	  (setq new-marker (append
			     (if (eq (type-of item) seq-type)
				 (reverse (lister-insert-sequence lister-buf pos item (1+ new-level)))
			       (list (lister-insert lister-buf pos item new-level)))
			    new-marker))
	  (setq pos (lister-end-of-lines lister-buf (setq last-pos (car new-marker))))))
      (setq new-marker (reverse new-marker))
      (lister-add-item-marker lister-buf new-marker)
      (lister-sensor-enter lister-buf last-pos)
      new-marker)))

(defun lister-insert-sublist-below (lister-buf pos-or-marker seq)
  "Insert SEQ as an indented sublist below the item at POS-OR-MARKER."
  (when-let* ((next-item      (lister-end-of-lines lister-buf pos-or-marker)))
    (let* ((current-level  (get-text-property pos-or-marker 'level lister-buf))
	   ;; we don't want the cursor to pop up at the end of the inserted
	   ;; list, since it would call the sensor functions. So we handle
	   ;; this on our own instead of letting lister-insert-sequence do
	   ;; it
	   (lister-inhibit-cursor-action t))
      (lister-insert-sequence lister-buf next-item seq (1+ current-level)))
    ;; lister-goto calls both sensor-leave and sensor-enter
    (lister-goto lister-buf pos-or-marker)))

;; Add single item to the end of the list

(defun lister-add (lister-buf data &optional level)
  "Add a list item representing DATA to the end of the list in LISTER-BUF.
Insert DATA at the indentation level LEVEL. For all possible
values of LEVEL, see `lister-determine-level'.

Return the marker of the added item's cursor gap position."
  (lister-insert lister-buf
		 (lister-next-free-position lister-buf)
		 data level))

;; Add sequence of items to the end of the list

(defun lister-add-sequence (lister-buf seq &optional level)
  "Add SEQ as items to LISTER-BUF with indentation LEVEL.
SEQ must be either a vector or a list.  Traverse SEQ and store its
elements as data into the newly created list items.  Any element of
the same type as SEQ will be interpreted as a nested list,
i.e. (item1 item2 (subitem1 subitem2) item3).

LEVEL determines the level of indentation. When LEVEL is nil,
insert SEQ at the level defined by the item at point. For all
possible values of LEVEL, see `lister-determine-level'.

Return a list of newly inserted markers."
  (lister-insert-sequence lister-buf nil seq level))

;; Remove item

(defun lister-remove (lister-buf position-or-symbol)
  "Remove the item at POSITION-OR-SYMBOL from LISTER-BUF.
POSITION can be either a buffer position, a marker, or one of the
symbols `:point', `:last' or `:first'. Do nothing if the position
does not indicate an item.

Also call the sensor functions before and after removing the
item; update the buffer local marker list and move point if the
removed item was at the end of the list.

The automatic correction of point is turned off when
`lister-inhibit-cursor-action' is set to t."
  (when-let* ((pos-marker (lister-marker-at lister-buf position-or-symbol)))
    (let* ((cursor-pos         (with-current-buffer lister-buf (point)))
	   (pos                (marker-position pos-marker)))
      ;; call sensor functions for leaving the item at point:
      (unless lister-inhibit-cursor-action 
	(when (= cursor-pos pos)
	  (lister-sensor-leave lister-buf)))
      ;; remove associated marker from the local marker list
      (with-current-buffer lister-buf
	(setq lister-local-marker-list
	      (cl-remove pos lister-local-marker-list :test #'=)))
      ;; remove the item 
      (lister-remove-lines lister-buf pos)
      ;; move point if it is not on an item anymore:
      (unless (or lister-inhibit-cursor-action
		  (get-text-property pos 'item lister-buf))
	(with-current-buffer lister-buf
	  (goto-char (or (lister-marker-at lister-buf :last)
			 (lister-item-max lister-buf)))))
      ;; if we left the sensor, turn it on again:
      (unless lister-inhibit-cursor-action
	(when (= cursor-pos pos)
	  (lister-sensor-enter lister-buf pos))))))

;; Remove intended sublists

(defun lister-level-at (lister-buf position-or-symbol)
  "Get current indentation level of item at POSITION-OR-SYMBOL.
LISTER-BUF is a lister buffer.

Return nil is there is no valid item at the position indicated."
  (when-let ((m (lister-marker-at lister-buf position-or-symbol)))
    (get-text-property (marker-position m) 'level lister-buf)))

(defun lister-sublist-boundaries (lister-buf marker-or-pos)
  "Return the inner boundaries of the sublist containing MARKER-OR-POS.
Return a list with a marker pointing to the first item of the
sublist, a second marker pointing to the last item of the
sublist, and the integer positions of the index positions
corresponding to these two items.

Example:
  ;; these are the boundaries of the first four items:
  (#<marker ....> #<marker ...> 0 3)"
  ;; FIXME This function uses the marker list heavily, which might be
  ;; costly if there are thousands of items. An alternative approach
  ;; would be to proceed from the item and to move up and down using
  ;; text property searches. It should be faster since the current
  ;; version still needs to access the text properties to determine
  ;; the level.
  (with-lister-buffer lister-buf
    (let* ((marker  (lister-pos-as-marker lister-buf marker-or-pos))
	   (n       (cl-position marker lister-local-marker-list :test #'=))
	   (last-n  (1- (length lister-local-marker-list)))
	   (level   (get-text-property marker 'level))
	   (beg-n   (cl-loop for i downfrom n to 0
			     ;; to determine ONLY the same level, use =
			     while (<= level (get-text-property (elt lister-local-marker-list i) 'level))
			     finally return (1+ i)))
	   (end-n   (cl-loop for i upfrom n to last-n
			     while (<= level (get-text-property (elt lister-local-marker-list i) 'level))
			     finally return (1- i)))
	   (beg     (elt lister-local-marker-list beg-n))
	   (end     (elt lister-local-marker-list end-n)))
      (list beg end beg-n end-n))))

(defun lister-remove-this-level (lister-buf pos-or-marker)
  "Remove all surrounding items matching the level of the item at POS-OR-MARKER."
  (let* ((beg-end (lister-sublist-boundaries lister-buf pos-or-marker)))
    (with-current-buffer lister-buf
      ;; split and recombine marker list:
      ;;
      ;; FIXME Is this necessary? Deleting the region deletes the
      ;; markers. So it might be faster to just rebuild the marker
      ;; list, or to weed out all invalid markers.
      (setq lister-local-marker-list
	    (append (seq-subseq lister-local-marker-list
				0 (cl-third beg-end))
		    (seq-subseq lister-local-marker-list
				(min (length lister-local-marker-list)
				     (1+ (cl-fourth beg-end))))))
      ;; actual deletion:
      (let* ((inhibit-read-only t)
	     (cursor-sensor-inhibit t)
	     (beg       (lister-pos-as-integer (cl-first beg-end)))
	     (end       (lister-end-of-lines lister-buf (cl-second beg-end))))
	(delete-region beg end)))))

(defun lister-sublist-below-p (lister-buf pos-or-marker)
  "Check if the next item is indented with respect to POS-OR-MARKER."
  (when-let* ((next-item      (lister-end-of-lines lister-buf pos-or-marker))
	      (current-level  (get-text-property pos-or-marker 'level lister-buf))
	      (next-level     (get-text-property next-item 'level lister-buf)))
    (> next-level current-level)))

(defun lister-remove-sublist-below (lister-buf pos-or-marker)
  "Remove the sublist below the item at POS-OR-MARKER.
Do nothing if the next item is not a sublist."
  (when (lister-sublist-below-p lister-buf pos-or-marker)
    ;; don't call sensor function if removed items are below point:
    (let* ((lister-inhibit-cursor-action (= (with-current-buffer lister-buf (point))
					    pos-or-marker)))
      (lister-remove-this-level lister-buf (lister-end-of-lines lister-buf pos-or-marker)))))

;; Replace item

(defun lister-replace (lister-buf position-or-symbol data &optional new-level)
  "Replace the item at POSITION-OR-SYMBOL with one representing DATA.
POSITION-OR-SYMBOL can be either a marker, a buffer position or
the symbols `:point', `:first' or `:last'. 

Preserve the indentation level or use NEW-LEVEL."
  (lister-with-locked-cursor lister-buf
    (let* ((pos-marker (lister-marker-at lister-buf position-or-symbol))
	   (level  (or new-level (get-text-property (marker-position pos-marker) 'level lister-buf))))
      (lister-remove lister-buf pos-marker)
      (lister-insert lister-buf pos-marker data level))))

;; NOTE There is currently no function to replace sublists

;; * Replace the whole buffer list (set the list)

(defun lister-set-list (lister-buf seq)
  "In LISTER-BUF, insert SEQ, leaving header and footer untouched.
SEQ can be nested to insert hierarchies."
  ;; delete old list:
  (with-lister-buffer lister-buf
    (let ((inhibit-read-only t)
	  (cursor-sensor-inhibit t))
      (delete-region (lister-item-min lister-buf)
		     (or lister-local-footer-marker
			 (lister-item-max lister-buf))))
    (setq lister-sensor-last-item nil)
    (setq lister-local-marker-list nil))
  ;; insert new list:
  (lister-add-sequence lister-buf seq))

;; -----------------------------------------------------------
;; * Marking and unmarking items

;; REVIEW A more appropriate semantics would to 'highlight' the item.

;; Visually reflect the mark state

(defun lister-display-mark-state (lister-buf marker-or-pos)
  "In LISTER-BUF, display the item as marked or not marked.
The item is referred to via MARKER-OR-POS pointing to its cursor
gap position."
  (with-lister-buffer lister-buf
    (let* ((inhibit-read-only t)
	   (state    (lister-get-mark-state lister-buf marker-or-pos))
	   (beg      (lister-pos-as-integer marker-or-pos))
	   (end      (lister-pos-as-integer (lister-end-of-lines lister-buf beg))))
      (if state
	  (lister-add-face-property beg end lister-mark-face-or-property)
	(lister-remove-face-property beg end lister-mark-face-or-property)))))

;; Query the mark state

(defun lister-get-mark-state (lister-buf pos-or-symbol)
  "In LISTER-BUF, test if the item at POS-OR-SYMBOL is marked.
POS-OR-SYMBOL can be either a marker, a position, or one of the
symbols `:point', `:first' and `:last'.

Return the marker state (nil or true). Also return nil if there
is no item at POS-OR-SYMBOL."
  (when-let* ((m (lister-marker-at lister-buf pos-or-symbol)))
    (get-text-property (lister-pos-as-integer m) 'mark lister-buf)))

(defun lister-all-marked-items (lister-buf)
  "Get all markers pointing to marked items in LISTER-BUF."
  ;; alternative: (lister-rescan-item-markers lister-buf 'mark)
  ;; Probably faster. 
  (seq-filter (apply-partially #'lister-get-mark-state lister-buf)
	      (buffer-local-value 'lister-local-marker-list lister-buf)))

;; Marking a single item

(defun lister-mark-item (lister-buf position-or-symbol value)
  "According to VALUE, either mark or unmark the item at POSITION-OR-SYMBOL.
VALUE must be a boolean value. POSITION-OR-SYMBOL can be a
marker, a buffer position, or one of the symbols `:point',
`:first' or `:last'. LISTER-BUF is a lister buffer.

After (un)marking the item, update the visible display of its
current mark state.

Return t if the item's state has been changed, else nil."
  (let* ((m (lister-marker-at lister-buf position-or-symbol))
	 (f (buffer-local-value 'lister-local-marking-predicate lister-buf)))
    (when (or (null f)
	      (funcall f (lister-get-data lister-buf m)))
      (lister-add-props lister-buf m 'mark value)
      (lister-display-mark-state lister-buf m)
      t)))

;; Marking a list of items

(defun lister-mark-some-items (lister-buf positions value)
  "In LISTER-BUF, mark all items in POSITIONS with VALUE."
  (with-lister-buffer lister-buf
    ;; using 'save-excursion' instead of `lister-with-locked-cursor'
    ;; assumes that marking does not change the item's positions or
    ;; size:
    (save-excursion
      (seq-do (lambda (m) (lister-mark-item lister-buf m value))
	      positions))))

(defun lister-mark-all-items (lister-buf value)
  "Set all items to the marking state VALUE in LISTER-BUF."
  (lister-mark-some-items lister-buf
			  (buffer-local-value 'lister-local-marker-list lister-buf)
			  value))

(defun lister-mark-this-sublist (lister-buf marker-or-pos value)
  "Mark the sublist to which MARKER-OR-POS belongs.
LISTER-BUF has to be a valid lister buffer."
  (let* ((bounds (lister-sublist-boundaries lister-buf marker-or-pos))
	 (index-first (cl-third bounds))
	 (index-last  (cl-fourth bounds))
	 (all-markers (buffer-local-value 'lister-local-marker-list lister-buf)))
    (lister-mark-some-items lister-buf
			    (cl-subseq all-markers
				       index-first
				       (1+ index-last))
			    value)))

;; Walk the marked data

(defun lister-walk-marked-items (lister-buf action &optional pred)
  "In LISTER-BUF, apply ACTION on each marked item.
The function ACTION is executed on each marked item, with point
on the item's cursor gap. It receives the item's data object as
its sole argument. The accumulated results will be passed as the
return value.

Optionally further restrict action on only those marked items
matching PRED."
  (lister-walk-some lister-buf
		    (lister-all-marked-items lister-buf)
		    action pred))


;; -----------------------------------------------------------
;; * Setting and getting stored data

;; Set data

(defun lister-set-data (lister-buf position-or-symbol data)
  "Store the lisp object DATA at POSITION-OR-SYMBOL in LISTER-BUF.
POSITION-OR-SYMBOL can be either a buffer position, a marker, or
 one of the symbols `:point', `:last' or `:first'."
  (when-let* ((m (lister-marker-at lister-buf position-or-symbol)))
    (lister-add-props lister-buf m 'data data)))

;; Get data

(defun lister-get-data (lister-buf position-or-symbol)
  "Return the data stored at POSITION-OR-SYMBOL in LISTER-BUF.
POSITION-OR-SYMBOL can be either a buffer position, a marker, or
 one of the symbols `:point', `:last' or `:first' ."
  (when-let* ((m (lister-marker-at lister-buf position-or-symbol)))
    (lister-get-prop lister-buf m 'data)))

;; Get lists of data:

(defun lister-get-all-data (lister-buf &optional beg end)
  "Collect all data values of all items in LISTER-BUF.
The values are collected in a flat list, ignoring any nested
levels or hierarchies."
  (seq-map (apply-partially #'lister-get-data lister-buf)
	   (lister-items-in-region lister-buf beg end)))

(defun lister-get-visible-data (lister-buf)
  "Collect the data values of all items visible in LISTER-BUF."
  (seq-map (apply-partially #'lister-get-data lister-buf)
	   (lister-visible-items lister-buf)))

;; Get data as tree:

;; TODO add option to also build a vector list
(cl-defun lister-group-by-level (l level-fn &optional (map-fn #'identity))
  "Build a tree from the flat list L.
L i s a list of elements with no nesting. LEVEL-FN is called with
each element and has to return the intended nesting level (as an
integer). Elements with the same associated level are treated as
one list; elements with higher levels are stored into sublists of
this list. 

MAP-FN can be used to additionally transform the elements when
building the tree.

Example:
  ;; use the cadr of each item as the level (level-fn);
  ;; use the car as the item itself (map-fn)  
  (lister-group-by-level '((a 0) (b 1) (c 1) (d 0)) #'cl-second #'cl-first)
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

(defun lister-items-in-region (lister-buf beg end)
  "Get all marker from index pos BEG to and including END.
LISTER-BUF must be a lister buffer; BEG and END are index
positions starting with 0. If either BEG or END is nil, use the
position of the first or last item, respectively."
  (when-let* ((mlist (buffer-local-value 'lister-local-marker-list lister-buf)))
    (if (and (null beg) (null end))
	mlist
      (when-let* ((top (if beg (lister-index-position lister-buf beg) 0))
		  (bot (if end (lister-index-position lister-buf end) (1- (length mlist)))))
	(seq-subseq mlist top
		    ;; the manual says 'end is the last item', the
		    ;; docstring says 'end is exclusive'. The docstring is
		    ;; right.
		    (1+ bot))))))
  
(defun lister-get-all-data-tree (lister-buf &optional beg end)
  "Collect all data values in LISTER-BUF, respecting its hierarchy.
Optionally restrict the result to the items ranging from the
buffer positions BEG and END (END is inclusive). If either BEG or
END is nil, use the position of the first or last item."
  (let* ((data-list (seq-map (lambda (pos)
			       (lister-get-props-at lister-buf pos 'data 'level))
			     (lister-items-in-region lister-buf beg end))))
      (lister-group-by-level data-list #'cl-second #'cl-first)))

;; -----------------------------------------------------------
;; * Walk the lister buffer

(defun lister-walk-some (lister-buf item-positions action
				    &optional predicate)
  "For all ITEM-POSITIONS in LISTER-BUF, execute ACTION and accumulate the result.

ITEM-POSITIONS is a list consisting of either integer positions
or markers.

ACTION will be called with the item's associated data. The
optional argument PREDICATE can be used to further restrict the
items on which ACTION will be executed. 

Both PREDICATE and ACTION are called with point on the item's
cursor gap and the current buffer set to LISTER-BUF, making it
easy to use all common lister functions. The whole loop is
wrapped in a call to `lister-with-locked-cursor', which see.

ACTION will be only executed if the position points to a valid
item (and optionally, if PREDICATE further returns a non-nil
value); positions not pointing to an item will be silently
skipped.

Return the accumulated results of all executed actions."
  (lister-with-locked-cursor lister-buf
    (with-current-buffer lister-buf
      (let ((acc nil)
	    (min (lister-item-min lister-buf))
	    (max (lister-item-max lister-buf)))
	(cl-dolist (item item-positions)
	  (goto-char item)
	  (when (and (>= item min)
		     (<= item max)
		     (get-text-property item 'item))
	    (let ((data (lister-get-data lister-buf item)))
	      (when (or (null predicate)
			(funcall predicate data))
		(setq acc (cons (funcall action data) acc))))))
	;; NOTE This could slow things down if we are traversing very
	;; long lists. For such cases, accumulation should be
	;; optional.
	(reverse acc)))))

(defun lister-walk-all (lister-buf action &optional pred)
  "In LISTER-BUF, execute ACTION for each item matching PRED.
See `lister-walk-some' for more details."
  (lister-walk-some lister-buf 
		    (buffer-local-value 'lister-local-marker-list
					lister-buf)
		    action
		    pred))

;; -----------------------------------------------------------
;; * Moving point API 

;; Go to an item

(defun lister-goto (lister-buf position-or-symbol)
  "In LISTER-BUF, move point to POSITION-OR-SYMBOL.
POSITION-OR-SYMBOL is a marke, a buffer position or one of the
symbols `:last', `:point' or `:first'. Return the position.
Throw an error if the item is not visible."
  (let* ((m (or (lister-marker-at lister-buf position-or-symbol)
		(lister-next-free-position lister-buf))))
    (with-lister-buffer lister-buf
      (if (invisible-p m)
	  (error "Item not visible")
	(goto-char m)
	(lister-sensor-leave lister-buf)
	(lister-sensor-enter lister-buf)
	m))))

;; -----------------------------------------------------------
;; * Cursor Sensor Function

(defun lister-sensor-enter (buf &optional pos)
  "Call the sensor functions on entering POS or point.
POS can be a buffer position or a marker.

Do nothing if `lister-inhibit-cursor-action' is t."
  (with-current-buffer buf
    (when (and (not lister-inhibit-cursor-action)
	       cursor-sensor-mode)
      (let ((cursor-sensor-inhibit t))
	(save-excursion
	  (setq pos (or pos (point)))
	  (when (get-text-property pos 'item)
	    (goto-char pos)
	    (setq lister-sensor-last-item (lister-pos-as-integer pos))
	    (run-hooks 'lister-enter-item-hook)))))))

(defun lister-sensor-leave (buf)
  "Call the sensor functions on leaving the last visited item.

Do nothing if `lister-inhibit-cursor-action' is t."
  (with-current-buffer buf
    (when (and (not lister-inhibit-cursor-action)
	       cursor-sensor-mode
	       lister-sensor-last-item)
      (save-excursion
	(let ((cursor-sensor-inhibit t))
	  (goto-char lister-sensor-last-item)
	  (run-hooks 'lister-leave-item-hook)
	  (setq lister-sensor-last-item nil))))))

(defun lister-sensor-function (win previous-point direction)
  "Run hooks on entering or leaving a lister item.
If `cursor-sensor-mode' is enabled, this function will be called
on entering or leaving the cursor gap of an item. Use the
arguments WIN, PREVIOUS-POINT and DIRECTION to determine what
kind of event has been caused.

Do nothing if `lister-inhibit-cursor-action' is t."
  (with-current-buffer (window-buffer win)
    (when (and (derived-mode-p 'lister-mode)
	       (not lister-inhibit-cursor-action))
      (let ((cursor-sensor-inhibit t)
	    (inhibit-read-only t))
	(cond
	 ((eq direction 'left)    (lister-sensor-leave (current-buffer)))
	 ((eq direction 'entered) (lister-sensor-enter (current-buffer)))
	 ;; special cases to avoid that the cursor stays on the footer
	 ;; or header:
	 ((eobp)                  (goto-char previous-point))
	 ((not (get-text-property (point) 'item)) nil)
	 (t nil))))))

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

(defun lister-remove-leave-callback (lister-buf callback-fn)
  "Remove CALLBACK-FN as callback on leaving an item."
  (with-current-buffer lister-buf
    (add-hook 'lister-leave-item-hook callback-fn nil t)))

;; -----------------------------------------------------------
;; * Lister Major Mode

;; Handle isearch properly

(defvar lister-local-isearch-opoint nil
  "Buffer local variable storing starting point during isearch.")

(defun lister-before-isearch ()
  "Prepare lister buffer for using isearch."
  (cursor-intangible-mode 0)
  (setq-local lister-local-isearch-opoint (point)))

(defun lister-after-isearch ()
  "Make sure point will end on an item after isearch."
  (when (/= (point) lister-local-isearch-opoint)
    (beginning-of-line))
  (cursor-intangible-mode 1)
  (when (not (get-text-property (point) 'item))
    (goto-char lister-local-isearch-opoint)))

;; Keys
(defun lister-key-toggle-mark (&optional sublist)
  "Toggle mark of item at point.
With prefix SUBLIST, mark the whole sublist with the inverted
status of the item at point."
  (interactive "P")
  (let* ((buf (current-buffer))
	 (m (lister-marker-at buf :point))
	 (mark-state (lister-get-mark-state buf m)))
    (if sublist
	(lister-mark-this-sublist buf m (not mark-state))
      (if (not (lister-mark-item buf m (not mark-state)))
	  (user-error "Item cannot be marked")
	;; move forward one line after marking:
	(when-let* ((next-item (lister-end-of-lines buf m)))
	  (unless (invisible-p next-item)
	    (lister-goto buf next-item)))))))

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
  (if (get-text-property (point) 'item)
      (if-let* ((fn lister-local-action))
	  (funcall lister-local-action
		   (lister-get-data (current-buffer) :point))
	(message "No action defined"))
    (user-error "No item at point")))

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
  (cursor-intangible-mode)
  (add-hook 'isearch-mode-hook #'lister-before-isearch nil t)
  (add-hook 'isearch-mode-end-hook #'lister-after-isearch nil t))

;; * Set up a lister buffer

;;;###autoload
(defun lister-setup (buf mapper-fn &optional data-list
			 header footer
			 filter-sexpr
			 no-major-mode)
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

Optional argument FILTER-SEXPR defines a filter function. This
can be a symbol standing for a function which accepts one
argument, the item's data. If the function returns a non-nil
value, the item is displayed. Filter terms can be combined using
the boolean operators `and', `or' and `not'. They are not lisp
function objects, however. For details, see
`lister-filter--expand-sexp'.

Set the major mode to `lister-mode' unless NO-MAJOR-MODE is true.

Move point to the first list item.

Return BUF."
  (with-current-buffer buf
    ;; first of all, set the major mode
    (unless no-major-mode
      (lister-mode))
    ;; prepare the buffer:
    (setq lister-local-mapper mapper-fn)
    (setq lister-enter-item-hook nil
	  lister-leave-item-hook nil)
    (setq buffer-undo-list t)
    (setq lister-sensor-last-item nil)
    (setq lister-local-filter-fn nil)
    (let ((cursor-sensor-inhibit t)
	  (inhibit-read-only t))
      (erase-buffer))
    ;; ready to add header, list and footer:
    (when header
      (lister-set-header buf header))
    (when footer
      (lister-set-footer buf footer))
    (when data-list
      (lister-set-list buf data-list))
    ;; apply filter:
    (lister-set-filter buf filter-sexpr)
    ;; move to first item:
    (if (lister-visible-items buf)
	(lister-goto buf :first)
      (goto-char (point-min)))
    buf))

(provide 'lister)
;;; lister.el ends here
