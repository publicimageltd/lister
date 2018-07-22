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

;; * Debugging Helper

(defun lister--marker-list ()
  (interactive)
  (message "Marker list: %s"
	   (string-join
	    (mapcar (lambda (m)
		      (format "%d" (marker-position m)))
		    (lister-viewport-marker-list vp))
	    " ")))

(defun lister-highlight-marker (viewport)
  (with-current-buffer (lister-viewport-buf viewport)
    (seq-doseq (m (lister-viewport-marker-list viewport))
      (overlay-put (make-overlay m (1+ m))
		   'face
		   '(:background "yellow")))))

(defun lister-remove-overlays (viewport)
  (with-current-buffer (lister-viewport-buf viewport)
    (remove-overlays)))

(defun lister-blink-overlays (viewport)
  (switch-to-buffer (lister-viewport-buf viewport))
  (lister-highlight-marker viewport)
  (redisplay)
  (sleep-for 0.5)
  (lister-remove-overlays viewport))

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

;; * Low Level: Insert or remove items 

(defun lister-make-marker-at (buf pos)
  "Return a suitable marker for POS in BUF."
  (let ((marker (make-marker)))
    (set-marker marker pos buf)
    (set-marker-insertion-type marker t)
    marker))

(defun lister-marker-at (viewport index &optional no-error)
  "Get marker for item at position INDEX."
  (let* ((ml (lister-viewport-marker-list viewport)))
    (if (and (>= index 0)
	     (< index (length ml)))
	(nth index ml)
      (unless no-error
	(error "lister-marker-at: index %s out of range." index)))))

(defun lister--marker-or-index (viewport &optional marker-or-index no-error)
  "Convert MARKER-OR-INDEX to a marker, or find, or create, the marker at point."
  (with-current-buffer (lister-viewport-buffer viewport)
    (pcase marker-or-index
      ((and (pred numberp) n) (lister-marker-at viewport marker-or-index no-error))
      ((and (pred markerp) m) m)
      ((pred null)         (seq-find (lambda (mk)
				       (eq (marker-position mk) (point)))
				     (lister-viewport-marker-list viewport)
				     (lister-make-marker-at (current-buffer) (point))))
      (_ (error "Unknown argument for MARKER-OR-INDEX")))))

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

(defun lister-insert-item (buf lines)
  "Insert the list LINES at the current position of BUF.
Return the marker of the first position.

LINES is a list. Each item can be either a string, which is
printed directly, or a function, to print its return value.
Nested lists will be flattened.

Each item in LINES is printed with a final newline character
added.

Also insert a text property `item' with the value `t' at the
beginning of the item."
  (with-current-buffer buf
    (let* ((start-pos         (point))
	   (inhibit-read-only t))
      (insert (string-join
	       (mapcar (lister-rcurry #'concat "\n")
		       (lister-strflat lines))))
      ;; don't change: item spans 2 position
      ;; this influences the calculation of the start-pos in
      ;; `lister-find-index', `lister-item-positions' and `lister-marker-list'
      (put-text-property start-pos (1+ start-pos) 'item t)
      (put-text-property start-pos (1- (point)) 'cursor-intangible t)
      (lister-make-marker-at buf start-pos))))

(defun lister-remove-item (buf marker)
  "Removes the item beginning at MARKER."
  (with-current-buffer buf
    (let* ((inhibit-read-only t)
	   (beg (marker-position marker))
	   (end (lister-next-item-position marker)))
      (delete-region beg end))))

(defun lister-replace-item (marker new-lines)
  "Replace the item at MARKER with the item defined by NEW-LINES.

Return the updated marker."
  (let ((pos (marker-position marker))
	(buf (marker-buffer marker)))
    (lister-remove-item buf marker)
    (goto-char pos)
    (lister-insert-item buf new-lines)
    (set-marker marker pos))
  marker)

;; * Main API: Insert, Remove, Replace Data at Viewports

(defun lister-goto-index (viewport index &optional no-error)
  (with-current-buffer (lister-viewport-buffer viewport)
    (let* ((target-pos (lister-marker-at viewport index no-error)))
      (when target-pos
	  (goto-char target-pos)))))

(defun lister-on-item-p (viewport)
  "Return t if point in viewport buffer is on an item."
  (with-current-buffer (lister-viewport-buffer viewport)
    (not (null (get-text-property (point) 'item)))))

(defun lister-get-data (viewport &optional marker-or-index)
  "Retrieve the data stored at MARKER-OR-INDEX."
  (let ((marker (lister--marker-or-index viewport marker-or-index)))
    (with-current-buffer (lister-viewport-buffer viewport)
      (get-text-property marker
			 'data))))

(defun lister-replace (viewport marker-or-index data)
  "Replace the item at MARKER-OR-INDEX with a new data item."
  (let* ((marker (lister--marker-or-index viewport marker-or-index)))
    (lister-replace-item marker
			 (funcall (lister-viewport-mapper viewport) data))
    ;; no need to update the marker list.
    (lister-set-data viewport marker data)))

(defun lister-remove (viewport marker-or-index)
  "Removes the item pointed to by MARKER, or on position INDEX."
  (let* ((marker (lister--marker-or-index viewport marker-or-index)))
    (lister-remove-item (lister-viewport-buffer viewport) marker)
    (setf (lister-viewport-marker-list viewport)
	  (seq-remove (lister-curry #'eq marker)
		      (lister-viewport-marker-list viewport)))))

(defun lister-insert (viewport data)
  "Insert DATA at the current position in the buffer defined by VIEWPORT.

Updates the marker list.
Return a marker with the start position."
  (let* ((buf    (lister-viewport-buffer viewport))
	 (item   (funcall (lister-viewport-mapper viewport) data))
	 (marker (lister-insert-item buf item)))
    (lister-set-data viewport marker data)
    ;; update marker list:
    (setf (lister-viewport-marker-list viewport)
	  (seq-sort #'< (append (lister-viewport-marker-list viewport) (list marker))))
    marker))

(defun lister-set-data (viewport marker-or-index data)
  "Stores DATA at the position MARKER-OR-INDEX.

This function does not change the list.
To exchange a data item of a list, use `lister-replace'."
  (let ((marker (lister--marker-or-index viewport marker-or-index)))
    (with-current-buffer (lister-viewport-buffer viewport)
      (let ((inhibit-read-only t))
	(put-text-property marker (1+ marker)
			   'data data)))))
;; * Header / Footer

(defun lister-header-at (viewport)
  "Return marker for header item at VIEWPORT"
  (lister-viewport-header-marker viewport))

(defun lister-footer-at (viewport)
  "Return marker for footer item at VIEWPORT"
  (lister-viewport-footer-marker viewport))

(defun lister-set-header (viewport header)
  "Insert HEADER at the beginning of VIEWPORT."
  (let ((old-header-marker (lister-header-at viewport)))
    (with-current-buffer (lister-viewport-buffer viewport)
      ;; replace existing header
      (if old-header-marker
	  (lister-replace-item old-header-marker header)
	;; insert new
	(goto-char (point-min))
	(setf (lister-viewport-header-marker viewport)
	      (lister-insert-item (current-buffer) header)))
      ;; update marker positions -> automatically!
      ;;(lister-recreate-marker-list viewport)
      (lister-set-intangible (current-buffer)
			     (lister-viewport-header-marker viewport)))))

(defun lister-set-footer (viewport footer)
  "Insert FOOTER at the end of the item list of VIEWPORT"
  (let ((old-footer-marker (lister-footer-at viewport)))
    (with-current-buffer (lister-viewport-buffer viewport)
      ;; replace existing footer
      (if old-footer-marker
	  (lister-replace-item old-footer-marker footer)
	;; insert new
	(goto-char (point-max))
	(setf (lister-viewport-footer-marker viewport)
	      (lister-insert-item (current-buffer) footer)))
      ;; no need to update marker positions
      (lister-set-intangible (current-buffer)
			     (lister-viewport-footer-marker viewport)))))

;; * Diverse

(defun lister-recreate-marker-list (viewport)
  "Completely recreate the marker list of VIEWPORT."
  (let* ((ml (lister-marker-list (lister-viewport-buffer viewport))))
    (when (lister-header-at viewport)
      (setf (lister-viewport-header-marker viewport)
	    (car ml))
      (setq ml (rest ml)))
    (when (lister-footer-at viewport)
      (setf (lister-viewport-footer-marker viewport)
	    (car (last ml)))
      (setq ml (seq-take ml (1- (length ml)))))
    (setf (lister-viewport-marker-list viewport) ml)))

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

;; * Positions

(defun lister-next-item-position (marker)
  "Looking at MARKER, return the beginning of the next item or
point max."
  (with-current-buffer (marker-buffer marker)
    (next-single-property-change (1+ marker)
				 'item
				 nil
				 (point-max))))

(defun lister-prev-item-position (marker)
  "Looking at MARKER, return the beginning of the previous item or
point min."
  (with-current-buffer (marker-buffer marker)
    (previous-single-property-change (1+ marker)
				     'item
				     nil
				     (point-min))))


(defun lister-marker-list (buf)
  "Return a list of marker pointing to each item in BUF.

Search for the items has to start on the first item, which is
assumed to be right at the beginning of the buffer."
  (mapcar (lister-curry #'lister-make-marker-at buf)
	  (lister-item-positions buf start-pos)))

(defun lister-item-positions (buf)
  "Create list of all item positions in lister mode buffer BUF.

Search for the items has to start on the first item, which is
assumed to be right at the beginning of the buffer."
  (with-current-buffer buf
    (save-excursion
      (goto-char (or start-pos (point-min)))
      (let* (acc (pos (point)))
	(while (< pos (point-max))
	  (push pos acc)
	  (setq pos (next-single-property-change pos 'item nil (point-max)))
	  (setq pos (next-single-property-change pos 'item nil (point-max))))
	(reverse acc)))))

(defun lister-find-index (buf pos)
  "Find the index position of POS within BUF.

The index position starts with 0.

If n items are inserted, return n if point is at the end of the
buffer (and when there is no list item)."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (let ((n 0) advance-to)
	(while (and
		(not (eobp))
		(>= pos
		   (setq advance-to
			 (next-single-property-change (point)
						      'item
						      nil
						      (point-max)))))
	  (incf n)
	  (goto-char advance-to))
	(/ n 2)))))

;; * Modify item data 

(defun lister-indent-item (padding lines)
  "Indent all items (but the first) in LINES by adding PADDING spaces."
  (declare (indent 1))
  (let* ((pad (make-string padding ? )))
    (append
     (list (car lines))
     (mapcar
      (lister-curry #'concat pad)
      (last lines (1- (length lines)))))))

;; -----------------------------------------------------------
;; * Lister Major Mode

(defvar lister-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "t") (lambda ()
				(interactive)
				(lister-blink-overlays vp)))
    map)
  "Keymap for `lister-mode'.")

(define-derived-mode lister-mode
  special-mode "Lister"
  "Major mode for selecting list items."
  (cursor-intangible-mode))

;; -----------------------------------------------------------
;; * Main Entry Points

(defun lister-set-list (viewport data)
  "Insert a list for DATA in VIEWPORT.

Erase any existing list.
Do not change header or footer."
  (with-current-buffer (lister-viewport-buffer viewport)
    (let* ((inhibit-read-only t)
	   (ml  (lister-viewport-marker-list viewport))
	   (beg (nth 0 ml))
	   (end (or (lister-viewport-footer-marker viewport)
		    (point-max))))
      (when ml (delete-region beg end))
      (setf (lister-viewport-marker-list viewport)
	    (mapcar (lister-curry #'lister-insert viewport) data))))
  viewport)

(defun lister-setup (buf mapper-fn &optional data header footer)
  "Insert DATA (optionally enclosed by HEADER and FOOTER) in emptied BUF.

Return the viewport."
  (let* ((inhibit-read-only t)
	 (viewport (make-lister-viewport
		    :buf buf
		    :mapper mapper-fn)))
    (with-current-buffer buf
      (erase-buffer))
    (when header
      (lister-set-header viewport header))
    (when data
      ;; we recreate the marker list anyways, so just loop
      (seq-each (lister-curry #'lister-insert viewport) data))
    (when footer
      (lister-set-footer viewport footer))
    (lister-recreate-marker-list viewport)
    (with-current-buffer buf
      ;;(lister-set-intangible buf (point-max))
      (when (lister-viewport-marker-list viewport)
	(goto-char (lister-marker-at viewport 0))))
    viewport))

;; -----------------------------------------------------------
;; Test

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
    (lister-blink-overlays viewport)))

(provide 'lister)
;;; lister.el ends here
