;;; lister.el --- a not very functional list printer            -*- lexical-binding: t; -*-

;; Copyright (C) 2018  

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


(require 'cl)
(require 'seq)

;; * Variables

(defstruct lister-viewport
  buffer
  mapper 
  header-marker
  footer-marker
  marker-list)

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


;; * Building the List with Lines

(defun lister-indent-item (padding lines)
  "Indent all items (but the first) in LINES by adding PADDING spaces."
  (declare (indent 1))
  (let* ((pad (make-string padding ? )))
    (append
     (list (car lines))
     (mapcar
      (lister-curry #'concat pad)
      (last lines (1- (length lines)))))))

(defun lister-strflat (seq)
  "Recursively stringify all items in L, flattening any sublists within l.

To \"stringify\" means:

If the list item is a string, return it unchanged.
If the list item is a function, insert its result.

Quoting cars such as (quote xy) or (function z) will be silently
dropped, keeping only the quoted item."
  (seq-reduce (lambda (acc e)
		(cond
		 ;; kill all nils:
		 ((null e)  acc)
		 ;; don't pass quoting cars to the result:
		 ((eq e 'function) acc)
		 ((eq e 'quote)    acc)
		 ((eq e 'closure)  acc)
		 ;; flatten lists
		 ((and (listp e) (not (functionp e))) (append acc (lister-strflat e)))
		 ;; actual work:
		 (t (append acc (list (if (functionp e) (funcall e) e))))))
	      seq
	      '()))

(defun lister-insert-lines (buf pos lines)
  "Insert the list LINES at POS in BUF.
Return the marker of the first position. 

LINES is a list. Each item can be either a string, which is
printed directly, or a function, to print its return value.
Nested lists will be flattened.

Each item in LINES is printed with a final newline character
added.

Point is on the end of the newly inserted text.

Also insert a text property `item' with the value `t' at the
beginning of the item."
  (with-current-buffer buf
    (let* ((beg               pos)
	   (item-list         (lister-strflat lines))
	   (inhibit-read-only t))
      (goto-char beg)
      (insert (string-join item-list "\n") "\n")
      (let* ((end (point)))
	(put-text-property beg (1+ beg) 'item t)
	(put-text-property beg (1- end) 'cursor-intangible t)
	(put-text-property beg (1+ beg) 'length (length item-list))
	(lister-make-marker buf beg)))))

;; * Insert

(cl-defgeneric lister-insert (viewport position data)
  "Insert DATA at POSITION in VIEWPORT.

POSITION can be either a buffer position (no marker!) or the special key
:point.")

(cl-defmethod lister-insert (viewport (position integer) data)
  "Insert DATA and its printed representation at buffer position POS in VIEWPORT.

Updates the marker list.
Return a marker with the start position."
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
  "Insert DATA at point in VIEWPORT."
  (let* ((pos (with-current-buffer (lister-viewport-buffer viewport) (point))))
    (lister-insert viewport
		   pos
		   data)))

;; * Add

(defun lister-add (viewport data)
  "Add DATA and its printed representation as new item to the
list in VIEWPORT."
  (lister-insert viewport (lister-next-free-position viewport) data))

;; * Set Header / Footer

(defun lister-replace-lines (buf pos new-lines)
  "Replace the lines at POS with NEW-LINES."
  (with-current-buffer buf
    (save-excursion
      (lister-remove-lines buf pos)
      (lister-insert-lines buf pos new-lines))))

(defun lister-set-header (viewport header)
  "Insert HEADER at the beginning of VIEWPORT."
  (let ((buffer (lister-viewport-buffer viewport)))
    (if-let ((header-marker (lister-viewport-header-marker viewport)))
	(setf (lister-viewport-header-marker viewport)
	      (lister-replace-lines
	       buffer
	       (marker-position header-marker)
	       header))
      (setf (lister-viewport-header-marker viewport)
	    (lister-insert-lines
	     buffer
	     (with-current-buffer buffer (point-min))
	     header)))
    (lister-set-intangible
     buffer
     (lister-viewport-header-marker viewport))))

(defun lister-set-footer (viewport footer)
  "Insert FOOTER at the end of the item list of VIEWPORT"
  (let ((buffer (lister-viewport-buffer viewport)))
    (if-let ((footer-marker (lister-viewport-footer-marker viewport)))
	(setf (lister-viewport-footer-marker viewport)
	      (lister-replace-lines
	       buffer
	       (marker-position footer-marker)
	       footer))
      (setf (lister-viewport-footer-marker viewport)
	    (lister-insert-lines
	     buffer
	     (with-current-buffer buffer (point-max))
	     footer)))
    (lister-set-intangible
     buffer
     (lister-viewport-footer-marker viewport))))

(defun lister-set-intangible (buf pos-or-marker)
  "Marks position POS-OR-MARKER as intangible."
  (let* ((inhibit-read-only t)
	 (pos (if (markerp pos-or-marker) (marker-position pos-or-marker) pos-or-marker)))
    (with-current-buffer buf
      (let* ((beg (if (eq pos (point-min)) pos (1- pos)))
	     (end (if (eq pos (point-max)) pos (1+ pos))))
	(put-text-property beg end 'cursor-intangible t)
	(put-text-property beg end 'field t)
	(put-text-property beg end 'rear-sticky t)
	(put-text-property beg end 'front-sticky t)))))

;; * Remove

(defun lister-remove-lines (buf pos)
  "Removes the item lines at pos in BUF."
  (with-current-buffer buf
    (let* ((inhibit-read-only t))
      (delete-region pos (lister-end-of-lines buf pos)))))

(cl-defgeneric lister-remove (viewport position)
  "Removes the item on POSITION in VIEWPORT.

POSITION can be either a marker, a list index position, or the
special key :point.")

(cl-defmethod lister-remove (viewport (position marker))
  "Removes the item at marker POSITION."
  (lister-remove-lines (lister-viewport-buffer viewport)
		       position)
  (setf (lister-viewport-marker-list viewport)
	(seq-remove (lambda (m)
		      (eq m position))
		    (lister-viewport-marker-list viewport))))

(cl-defmethod lister-remove (viewport (position integer))
  "Removes the item at index POSITION."
  (lister-remove viewport (lister-marker-at viewport position)))

(cl-defmethod lister-remove (viewport (position (eql :point)))
  "Removes the item at point."
  (when-let* ((marker (lister-current-marker viewport)))
    (lister-remove viewport marker)))

;; * Replace

(cl-defgeneric lister-replace (viewport position data)
  "Replace the item at POSITION with a new DATA item.

POSITION can be either a marker, a list index position, or the
special key :point.")

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

;; * Set Data

(cl-defgeneric lister-set-data (viewport position data)
  "Store DATA at POSITION.

POSITION can be either a marker, a list index position, or the
special key :point.")

(cl-defmethod lister-set-data (viewport (position marker) data)
  "Store DATA at the position defined by MARKER."
  (with-current-buffer (lister-viewport-buffer viewport)
      (let ((inhibit-read-only t))
	(put-text-property position (1+ position)
			   'data data))))

(cl-defmethod lister-set-data (viewport (position integer) data)
  "Store DATA at the INDEX position of the list."
  (lister-set-data viewport
		   (lister-marker-at viewport position)
		   data))

(cl-defmethod lister-set-data (viewport (position (eql :point)) data)
  "Store DATA in the item at point."
  (when-let* ((marker (lister-current-marker viewport)))
    (lister-set-data viewport marker data)))

;; * Get Data

(cl-defgeneric lister-get-data (viewport position)
  "Retrieve the data stored at POSITION.

POSITION can be either a marker, a list index, or the special key
:point.")

(cl-defmethod lister-get-data (viewport (position marker))
  "Retrieve the data stored at marker POSITION."
  (with-current-buffer (lister-viewport-buffer viewport)
    (get-text-property position 'data)))

(cl-defmethod lister-get-data (viewport (position integer))
  "Retrieve the data stored at INDEX position."
  (lister-get-data viewport
		   (lister-marker-at viewport position)))

(cl-defmethod lister-get-data (viewport (position (eql :point)))
  "Retrieve the data at point."
  (when-let* ((marker (lister-current-marker viewport)))
    (lister-get-data viewport marker)))

;; * Goto

(cl-defgeneric lister-goto (viewport position)
  "Move point in VIEWPORT to POSITION.

POSITION can be either a marker, a list index number, or one of
the special keys :last or :first")

(cl-defmethod lister-goto (viewport (position marker))
  "Move point in VIEWPORT to the marker POSITION."
  (with-current-buffer (lister-viewport-buffer viewport)
    (goto-char position)))

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
  "Get marker for item at position INDEX."
  (let* ((ml (lister-viewport-marker-list viewport)))
    (if (and (>= index 0)
	     (< index (length ml)))
	(nth index ml)
      (error "lister-marker-at: index %s out of range." index))))

(defun lister-current-marker (viewport)
  "Return the marker at point in VIEWPORT, iff on the beginning
of an item."
  (with-current-buffer (lister-viewport-buffer viewport)
    (save-excursion
      (when (get-text-property (point) 'item)
	(seq-find (lambda (m)
		    (eq (marker-position m) (point)))
		  (lister-viewport-marker-list viewport))))))

(defun lister-marker-list (buf)
  "Return a list of marker pointing to each item in BUF.

Search for the items has to start on the first item, which is
assumed to be right at the beginning of the buffer."
  (mapcar (lister-curry #'lister-make-marker buf)
	  (lister-item-positions buf)))

(defun lister-first-lines (buf)
  "Return position of first line item in BUF (footer or list item)."
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
	  (while (setq lines (get-text-property (point) 'length buf))
	    (forward-line lines)
	    (when (get-text-property (point) 'item buf)
	      (push (point) result)))
	  (reverse result))))))

(cl-defgeneric lister-index (viewport position)
  "Return the index of the item at POSIITION.

POSITION can be either a marker, a valid buffer position, or the
special key :point.")

(cl-defmethod lister-index (viewport (position marker))
  (seq-position (lister-viewport-marker-list viewport)
		position
		#'equal))

(cl-defmethod lister-index (viewport (position integer))
  (lister-index viewport (lister-make-marker
			  (lister-viewport-buffer viewport)
			  position)))

(cl-defmethod lister-index (viewport (position (eql :point)))
  (lister-index viewport (point)))

(defun lister-next-free-position (viewport)
  "Return the next position for a new list item in VIEWPORT."
  (let* ((ml     (lister-viewport-marker-list viewport))
	 (buffer (lister-viewport-buffer viewport))
	 (header (lister-viewport-header-marker viewport))
	 (footer (lister-viewport-footer-marker viewport)))
    (cond
     ((last ml)  (lister-end-of-lines buffer (marker-position (car (last ml)))))
     (footer     (lister-end-of-lines buffer (marker-position footer)))
     (header     (point-max))
     (t          (point-min)))))

(defun lister-end-of-lines (buf pos)
  "Return the end position of the line item beginning at POS in BUF."
  (with-current-buffer buf
    (save-mark-and-excursion
      (goto-char pos)
      (forward-line (get-text-property pos 'length))
      (point))))

;; * Creating Markers

(defun lister-make-marker (buf pos)
  "Return a suitable marker for POS in BUF."
  (let ((marker (make-marker)))
    (set-marker marker pos buf)
    (set-marker-insertion-type marker t)
    marker))

(defun lister-recreate-marker-list (viewport)
  "Completely recreate the marker list of VIEWPORT."
  (let* ((ml (lister-marker-list (lister-viewport-buffer viewport))))
    (when (lister-viewport-header-marker viewport)
      (setf (lister-viewport-header-marker viewport)
	    (pop ml)))
    (when (lister-viewport-footer-marker viewport)
      (setf (lister-viewport-footer-marker viewport)
	    (car (last ml)))
      (setq ml (seq-take ml (1- (length ml)))))
    (setf (lister-viewport-marker-list viewport) ml)))

;; * Lister Major Mode

(define-derived-mode lister-mode
  special-mode "Lister"
  "Major mode for selecting list items."
  (cursor-intangible-mode))

;; * Setup 

(defun lister-set-list (viewport data)
  "Display a list for DATA in VIEWPORT. Do not change header or
footer.

Return the viewport."
  (with-current-buffer (lister-viewport-buffer viewport)
    (when-let* ((ml  (lister-viewport-marker-list viewport)))
      (let* ((beg (nth 0 ml))
	     (end (or (lister-viewport-footer-marker viewport)
		      (point-max)))
	     (inhibit-read-only t))
	(delete-region beg end)))
    (setf (lister-viewport-marker-list viewport)
	  (mapcar (lister-curry #'lister-add viewport) data)))
  viewport)

(defun lister-setup (buf mapper-fn &optional data header footer)
  "Insert DATA (optionally enclosed by HEADER and FOOTER) in emptied BUF.

Return the viewport."
  (let* ((viewport (make-lister-viewport
		    :buffer buf
		    :mapper mapper-fn)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
	(erase-buffer)))
    (when header
      (lister-set-header viewport header))
    (when data
      (seq-each (lister-curry #'lister-add viewport) data))
    (when footer
      (lister-set-footer viewport footer))
    (lister-recreate-marker-list viewport)
    ;; (with-current-buffer buf
    ;;   (lister-set-intangible buf (point-max))
    (when data
      (lister-goto viewport 0))
    viewport))

;; -----------------------------------------------------------
;; Test

(when nil

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
    (let ((viewport (lister-setup (lister-test-buffer)
				  #'lister-mapper
				  '("A" "B" "C"))))
      (setq vp viewport)
      (switch-to-buffer (lister-viewport-buffer viewport))
      (lister-mode)
      (lister-blink-overlays viewport))))

(provide 'lister)
;;; lister.el ends here
