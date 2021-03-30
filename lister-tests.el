;;; lister-tests.el --- testsuite for lister.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021

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

;; Provides buttercup tests for lister

;;; Code:

(require 'lister "lister.el")
(require 'buttercup)
(require 'seq)
(require 'cl-lib)
(require 'cl-extra)

(message "Testing lister version %s on Emacs %s" lister-version emacs-version)

;; (setq buttercup-stack-frame-style 'pretty)

;; * Utility functions

(defun lister-test-setup-minimal-buffer ()
  "Set up a minimal buffer, with no margins and a list mapper.
Return the buffer object"
  (let ((new-buf (lister-setup (generate-new-buffer "*LISTER*")
			       #'list)))
    (with-current-buffer new-buf
      (setq lister-local-left-margin 0))
    new-buf))

(defun lister-test-positions-of (l &optional indentation)
  "Return a list of expected positions for inserting L.
L has to be a list of items which can be printed with format
'%s'. The results are only valid in a buffer with no margins, and
if the items are inserted with no indentation level.

Optional argument INDENTATION adds an indentation level of n."
  (let (acc (last-pos 1))
    (cl-dolist (item l)
      (push last-pos acc)
      (setq last-pos (+ last-pos (or indentation 0)
			(1+ (length (format "%s" item))))))
    (reverse acc)))

(defun lister-test-expected-content (l &optional header footer indentation)
  "Return a string of the expected buffer contents when inserting L.
L has to be a list of items which can be printed with format '%s'.
HEADER and FOOTER have to be nil or strings. The results are only
valid in a minimal buffer with no margins, and if the items are
inserted with no indentation.

Optional argument INDENTATION adds an indentation level of n."
  (let ((indent-string (make-string (or indentation 0) ? )))
    (concat
     (when header (format "%s\n" header))
     (when l
       (concat (string-join (mapcar (apply-partially #'concat indent-string)
				    (mapcar (apply-partially #'format "%s")
					    l))
			    "\n")
	       "\n"))
     (when footer (format "%s\n" footer)))))

(defun lister-test-remove-elt-by-index (l n)
  "Remove element with index N (zero-based) from L."
  (append (cl-subseq l 0 n)
	  (cl-subseq l (1+ n))))

(defun lister-test-only-visible-content (buf)
  "Return only content from BUF which is not marked as invisible."
  (with-current-buffer buf
    (goto-char (point-min))
    (let (acc)
      (while (not (eobp))
	(let* ((invisible (get-text-property (point) 'invisible))
	       (next-change
		(or (next-single-property-change (point) 'invisible)
		    (point-max))))
	  (unless invisible
	    (setq acc
		  (concat acc (buffer-substring-no-properties (point) next-change))))
	  (goto-char next-change)))
      acc)))

;;; * Custom Matchers

;; to match marker lists:
(buttercup-define-matcher :to-have-as-marker-positions (buf pos-list)
  (let* ((ml (with-current-buffer (funcall buf)
	       (mapcar #'marker-position lister-local-marker-list)))
	 (expected-list (funcall pos-list)))
    (buttercup--test-expectation
	(equal ml expected-list)
      :expect-match-phrase
      (format "Expected marker list to be '%s', but instead it was '%s'"
	      expected-list ml)
      :expect-mismatch-phrase
      (format "Expected marker list not to be '%s', but it was."
	      expected-list))))

;; to match buffer contents as data trees:
(buttercup-define-matcher :to-have-as-data-tree (buf data-to-be)
  (let* ((data (lister-get-all-data-tree (funcall buf)))
	 (expected-data (funcall data-to-be)))
    (buttercup--test-expectation
	(equal data expected-data)
      :expect-match-phrase
      (format "Expected buffer data tree to be '%s', but instead it was '%s'"
	      expected-data data)
      :expect-mismatch-phrase
      (format "Expected buffer data tree not to be '%s', but it was."
	      expected-data))))

;; to match buffer contents:
(buttercup-define-matcher :to-have-as-content (buf content-to-be)
  (let* ((content (with-current-buffer (funcall buf)
		    (buffer-substring-no-properties
		     (point-min) (point-max))))
	 (expected-content (funcall content-to-be)))
    (buttercup--test-expectation
	(equal content expected-content)
      :expect-match-phrase
      (format "Expected buffer content to be '%s', but instead it was '%s'"
	      expected-content content)
      :expect-mismatch-phrase
      (format "Expected buffer content not to be '%s', but it was."
	      expected-content))))

;; to match only visible buffer contents:
(buttercup-define-matcher :to-have-as-visible-content (buf content-to-be)
  (let* ((content  (lister-test-only-visible-content (funcall buf)))
	 (expected-content (funcall content-to-be)))
    (buttercup--test-expectation
	(equal content expected-content)
      :expect-match-phrase
      (format "Expected buffer content to be '%s', but instead it was '%s'"
	      expected-content content)
      :expect-mismatch-phrase
      (format "Expected buffer content not to be '%s', but it was."
	      expected-content))))

;; to match the position of point:
(buttercup-define-matcher :to-have-point-value-of (buf pos-or-integer)
  (let* ((point (with-current-buffer (funcall buf) (point)))
	 (pos   (let ((poi (funcall pos-or-integer)))
		  (pcase poi
		    ((pred integerp) poi)
		    ((pred markerp) (marker-position poi))
		    (_              (error "invalid position: %s" poi))))))
    (buttercup--test-expectation
	(eq point pos)
      :expect-match-phrase
      (format "Expected point in buffer to be '%d', but instead it was '%d'."
	      pos point)
      :expect-mismatch-phrase
      (format "Expected point in buffer not to be '%d', but it was."
	      pos))))

;; -----------------------------------------------------------
;; The tests.

(describe "Utility functions:"
  (describe "lister--wrap-list"
    (it "wraps a flat list in lists"
      (let ((the-list '(a b c d)))
	(expect (lister--wrap-list the-list)
		:to-equal
		(mapcar #'list the-list))))
    (it "wraps a sublist in the item before it begins"
      (let ((the-list '(a b top (sub-1 sub-2 sub-3))))
	(expect (lister--wrap-list the-list)
		:to-equal
		'((a) (b) (top (sub-1) (sub-2) (sub-3))))))
    (it "wraps a cascade of sublists"
      (let ((the-list '(top1 (top2 (top3 (top4 (s1 s2 s3)))))))
	(expect (lister--wrap-list the-list)
		:to-equal
		'((top1 (top2 (top3 (top4 (s1) (s2) (s3))))))))))

  (describe "lister--reorder-wrapped-list"
    (it "sorts a flat list"
      (let ((the-list '((7) (5) (1) (8) (3) (2)))
	    (the-fn   (apply-partially #'seq-sort-by #'car #'<)))
	(expect (lister--reorder-wrapped-list the-list the-fn)
		:to-equal
		'(1 2 3 5 7 8))))

    (it "sorts wrapped nested lists"
      (let ((the-list  '((8) (6) (4
				  (43
				   (40
				    (401)
				    (402)
				    (408)
				    (405))
				   (48)))
			 (1) (9) (7) (3)))
	    (the-fn   (apply-partially #'seq-sort-by #'car #'<)))
	(expect (lister--reorder-wrapped-list
		 the-list
		 the-fn)
		:to-equal
		'(1 3 4 (43 (40 (401 402 405 408) 48)) 6 7 8 9)))))

  (describe "lister--list-pos-in:"
    (it "returns the gap positions between sorted items"
      (let* ((pos-list (number-sequence 0 20 2))
	     (new-list (number-sequence 1 20 2)))
	(expect
	 (mapcar (lambda (it) (elt pos-list it))
		 (mapcar (lambda (it)
			   (lister--list-pos-in pos-list it))
			 new-list))
	 :to-equal (cdr pos-list))))
    (it "returns the item positions if new values matches the items"
      (let* ((pos-list (number-sequence 0 20 2)))
	(expect (mapcar (lambda (it) (elt pos-list it))
			(mapcar (lambda (it)
				  (lister--list-pos-in pos-list it))
				pos-list))
		:to-equal pos-list)))
    (it "returns (length l) if new value is bigger than all items"
      (let* ((pos-list (number-sequence 0 20)))
	(expect (lister--list-pos-in pos-list 20000)
		:to-be (length pos-list)))))

  (describe "lister--list-insert-at:"
    (it "inserts a new list at specified position"
      (let* ((old-list (number-sequence 0 10))
	     (new-list '("A" "B" "C")))
	(expect (lister--list-insert-at old-list new-list 4)
		:to-equal
		'(0 1 2 3 "A" "B" "C" 4 5 6 7 8 9 10))))
    (it "modifies the first argument"
      (let* ((old-list (number-sequence 0 10))
	     (old-list-copy (append old-list nil))
	     (new-list '("A" "B" "C")))
	(lister--list-insert-at old-list new-list (length old-list))
	(expect old-list
		:to-equal
		(append old-list-copy new-list))))))

(describe "lister-insert-lines:"
  :var (buf)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer)))
  (after-each
    (kill-buffer buf))

  ;; For constructing the expected content, each item's line is
  ;; treated as a separate item: The item ("1" "2") prints the same
  ;; result as a list of two single items ("1") and ("2").

  (describe "lister-strflat"
    (it "wraps a string into a list"
      (expect (lister-strflat "test")         :to-equal '("test")))
    (it "removes nil values in a list"
      (expect (lister-strflat '("1" nil "3")) :to-equal '("1" "3")))
    (it "flattens nested list items"
      (expect (lister-strflat '(("1") ("2"))) :to-equal '("1" "2"))))

  (it "Insert item using a marker position."
    (let* ((marker     (with-current-buffer buf (point-max-marker)))
	   (test-item '("1" "2")))
      (lister-insert-lines buf marker test-item 0)
      (expect buf :to-have-as-content (lister-test-expected-content test-item))
      (expect (get-text-property marker 'item buf)   :to-be  t)
      (expect (get-text-property marker 'nchars buf) :to-be  4)))

  (it "Insert item using an integer position."
    (let* ((marker     (with-current-buffer buf (point-max-marker)))
	   (test-item '("1" "2")))
      (lister-insert-lines buf (marker-position marker) test-item 0)
      (expect buf :to-have-as-content (lister-test-expected-content test-item))))

  (it "Insert item with indentation level 1."
    (let* ((marker    (with-current-buffer buf (point-max-marker)))
	   (test-item '("1" "2")))
      (lister-insert-lines buf (marker-position marker) test-item 1)
      (expect buf :to-have-as-content (lister-test-expected-content test-item nil nil 1))))

  (it "Insert item with indentation level 2."
    (let* ((marker    (with-current-buffer buf (point-max-marker)))
	   (test-item '("1" "2")))
      (lister-insert-lines buf (marker-position marker) test-item 2)
      (expect buf :to-have-as-content (lister-test-expected-content test-item nil nil 2))))

  (it "Insert item with indentation level 3."
    (let* ((marker    (with-current-buffer buf (point-max-marker)))
	   (test-item '("1" "2")))
      (lister-insert-lines buf (marker-position marker) test-item 3)
      (expect buf :to-have-as-content (lister-test-expected-content test-item nil nil 3)))))

;; * CORE: lister-insert, lister-next-free-position, lister-item-min,
;;         lister-item-max

(describe "Some low level item functions:"
  :var (buf)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer)))
  (after-each
    (kill-buffer buf))

  (describe "lister-insert:"
    (it "inserts item content as expected"
      (let ((item "JLKJLKDJFLKDSJFLKDJSLKFJSLKJFSLKJ"))
	(lister-insert buf (point-min) item)
	(expect buf :to-have-as-content (lister-test-expected-content (list item)))))
    (it "inserts item with properties nchars, item, level"
      (let* ((data "SOMEDATASHOULDBEGOOD")
	     (m (lister-insert buf (point-min) data)))
	(expect (get-text-property m 'nchars buf) :to-be (1+ (length data )))
	(expect (get-text-property m 'level buf)  :to-be 0)
	(expect (get-text-property m 'item buf)   :to-be t)))
    (it "inserts item with sensor function"
      (let* ((m (lister-insert buf (point-min) "A")))
	(expect (get-text-property m 'cursor-sensor-functions buf)
		:to-equal
		'(lister-sensor-function))))
    (it "inserts the new marker in the the marker list"
      (let* ((m (lister-insert buf (point-min) "A")))
	(expect (buffer-local-value 'lister-local-marker-list buf)
		:to-equal
		(list m)))))

  ;; now we can use lister-add, which relies on lister-next-free-position:
  (describe "lister-item-min"
    (it "returns the correct position if there is no item"
      (expect (lister-item-min buf) :to-be 1))
    (it "returns the corrrect position after 1 item"
      (lister-add buf "A")
      (expect (lister-item-min buf) :to-be 1))
    (it "returns the corrrect position after 2 items"
      (lister-add buf "A")
      (lister-add buf "A")
      (expect (lister-item-min buf) :to-be 1))
    (it "returns the corrrect position after 3 items"
      (lister-add buf "A")
      (lister-add buf "A")
      (lister-add buf "A")
      (expect (lister-item-min buf) :to-be 1)))

  (describe "lister-item-max"
    (it "returns the correct position if there is no item"
      (expect (lister-item-max buf) :to-be 1))
    (it "returns the corrrect position after 1 item"
      (lister-add buf "A")
      (expect (lister-item-max buf) :to-be 3)) ;; 1+"A"+"\n"
    (it "returns the corrrect position after 2 items"
      (lister-add buf "A")
      (lister-add buf "A")
      (expect (lister-item-max buf) :to-be 5))
    (it "returns the corrrect position after 3 items"
      (lister-add buf "A")
      (lister-add buf "A")
      (lister-add buf "A")
      (expect (lister-item-max buf) :to-be 7)))

  (describe "lister-looking-at-prop:"
    (it "looks at next item and returns nil if there is none"
      (expect (lister-looking-at-prop buf 1 'item 'next) :to-be nil))
    (it "looks at previous item and returns nil if there is none"
      (expect (lister-looking-at-prop buf 1 'item 'previous) :to-be nil))
    (it "looks at previous item and returns its position"
      (let* ((some-items     '("ABER" "DAS" "IST" "SINNLOS"))
	     (some-positions (lister-test-positions-of some-items)))
	(cl-dolist (item  some-items)
	  (lister-add buf item))
	(let ((n 2))
	  (expect (lister-looking-at-prop buf (elt some-positions n) 'item 'previous)
		  :to-be (elt some-positions (1- n))))))
    (it "looks at next item and returns its position"
      (let* ((some-items     '("ABER" "DAS" "IST" "SINNLOS"))
	     (some-positions (lister-test-positions-of some-items)))
	(expect some-positions :to-equal '(1 6 10 14))
	(cl-dolist (item  some-items)
	  (lister-add buf item))
	(expect (lister-get-all-data buf) :to-equal some-items)
	(expect (elt some-positions 2) :to-be 10)
	(let ((n 2))
	  (expect (lister-looking-at-prop buf (elt some-positions n) 'item 'next)
		  :to-be (elt some-positions (1+ n)))))))

  (describe "lister-rescan-item-markers:"
    (it "returns the correct positions for all items:"
      (let* ((some-items '("A" "B" "JAJA" "NEINEIN" "ACH")))
	(cl-dolist (item  some-items)
	  (lister-add buf item))
	(expect (mapcar #'marker-position (lister-rescan-item-markers buf))
		:to-equal
		(lister-test-positions-of some-items)))))

  (describe "lister-items-in-region"
    :var (some-items some-positions)
    (before-each
      (setq some-items (number-sequence 0 20))
      (setq some-positions (lister-test-positions-of some-items))
      (lister-add-sequence buf some-items))

    (it "returns the whole marker list if called with no boundaries"
      (expect (lister-items-in-region buf nil nil)
	      :to-equal
	      (with-current-buffer buf lister-local-marker-list)))
    (it "returns the upper part of a list"
      (let ((n 5))
	(expect (lister-items-in-region buf (elt some-positions n) nil)
		:to-equal
		(seq-subseq (with-current-buffer buf lister-local-marker-list)
			    n))))
    (it "returns the lower part of a list"
      (let ((n 15))
	(expect (lister-items-in-region buf nil (elt some-positions n))
		:to-equal
		(seq-subseq (with-current-buffer buf lister-local-marker-list)
			    0
			    ;; in seq-subseq, "end" is exclusive
			    (1+ n)))))
    (it "returns a part of a list"
      (let ((first 10) (last 15))
	(expect (lister-items-in-region buf
					(elt some-positions first)
					(elt some-positions  last))
		:to-equal
		(seq-subseq (with-current-buffer buf lister-local-marker-list)
			    first
			    ;; in seq-subseq, "end" is exclusive
			    (1+ last)))))))

;; * CORE: lister-marker-at

(describe "lister-marker-at:"
  :var (buf some-items some-positions)
  (before-each
    (setq some-items '("A" "B" "JAJA" "NEINEIN" "ACHJE"))
    (setq some-positions (lister-test-positions-of some-items))
    (setq buf (lister-test-setup-minimal-buffer))
    (cl-dolist (item some-items)
      (lister-add buf item)))
  (after-each
    (kill-buffer buf))

  (it "premise: inserted lists has the right positions"
    (expect (mapcar #'marker-position (lister-rescan-item-markers buf))
	    :to-equal
	    (lister-test-positions-of some-items)))
  (it "points correctly to the first item:"
    (expect (lister-get-data buf (lister-marker-at buf :first))
	    :to-equal (elt some-items 0)))
  (it "points correctly to the last item:"
    (expect (lister-get-data buf (lister-marker-at buf :last))
	    :to-equal
	    (car (reverse some-items))))
  (it "correctly points to some item at point:"
    (let (n)
      (with-current-buffer buf
	(setq n (random (seq-length some-items)))
	(goto-char (elt some-positions n)))
      (expect (lister-get-data buf :point)
	      :to-equal
	      (elt some-items n))))
  (it "accepts a marker as an argument"
    (let* ((position (elt some-positions 3))
	   (m1 (lister-make-marker buf position))
	   (m2 (lister-marker-at buf m1)))
      (expect m1 :to-equal m2)))
  (it "accepts an integer position as an argument"
    (let* ((position (elt some-positions 3))
	   (m (lister-marker-at buf position)))
      (expect (marker-position m) :to-equal position))))

;; * Setting and removing header/footer

(describe "Header and footer:"
  :var (buf header footer data)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer))
    (setq header "HEADER"
	  footer "FOOTER"
	  data   "DATA"))
  (after-each
    (kill-buffer buf))
  ;;
  (describe "Header and footer in empty lists:"

    (describe "lister-set-header"
      (it "inserts a single header."
	(lister-set-header buf header)
	(expect buf :to-have-as-content (lister-test-expected-content nil header)))
      (it "inserts a single header and removes it"
	(lister-set-header buf header)
	(lister-set-header buf nil)
	(expect buf :to-have-as-content (lister-test-expected-content nil))))

    (describe "lister-set-footer"
      (it "inserts a single footer."
	(lister-set-footer buf footer)
	(expect buf :to-have-as-content (lister-test-expected-content nil nil footer)))
      (it "inserts a single footer and removes it"
	(lister-set-footer buf header)
	(lister-set-footer buf nil)
	(expect buf   :to-have-as-content (lister-test-expected-content nil))))

    (describe "set-header/set-footer combined: "
      (it "insert header and footer"
	(lister-set-header buf header)
	(lister-set-footer buf footer)
	(expect buf :to-have-as-content (lister-test-expected-content nil header footer)))
      (it "insert header and footer, then remove footer"
	(lister-set-header buf header)
	(lister-set-footer buf footer)
	(lister-set-footer buf nil)
	(expect buf :to-have-as-content (lister-test-expected-content nil header)))
      (it "insert header and footer, then remove header."
	(lister-set-header buf header)
	(lister-set-footer buf footer)
	(lister-set-header buf nil)
	(expect buf :to-have-as-content (lister-test-expected-content nil nil footer))))

    (describe "set-header/-footer with margins:"
      (it "insert header and footer with left margin"
	(let ((margin 3))
	  (with-current-buffer buf
	    (setq lister-local-left-margin margin))
	  (lister-set-header buf header)
	  (lister-set-footer buf footer)
	  (let ((margined-header (concat (make-string 3 ? ) header))
		(margined-footer (concat (make-string 3 ? ) footer)))
	    (expect buf :to-have-as-content
		    (lister-test-expected-content nil margined-header margined-footer))))))

    (describe "Header and footer in non-empty lists"
      (it "adds one item between header and footer"
	(lister-set-header buf header)
	(lister-set-footer buf footer)
	(lister-add buf data)
	(expect buf :to-have-as-content (lister-test-expected-content (list data) header footer)))
      (it "adds some items between header and footer"
	(let ((some-items '("1" "2" "3")))
	  (lister-set-header buf header)
	  (lister-set-footer buf footer)
	  (cl-dolist (item some-items)
	    (lister-add buf item))
	  (expect buf :to-have-as-content (lister-test-expected-content some-items header footer))))
      (it "lister-set-footer nil removes footer"
	(lister-set-header buf header)
	(lister-set-footer buf footer)
	(lister-add buf data)
	(lister-set-footer buf nil)
	(expect buf :to-have-as-content (lister-test-expected-content (list data) header)))
      (it "lister-set-header nil removes header"
	(lister-set-header buf header)
	(lister-set-footer buf footer)
	(lister-add buf data)
	(lister-set-header buf nil)
	(expect buf :to-have-as-content (lister-test-expected-content (list data) nil footer)))
      (it "removes header and footer, leaving only the list:"
	(let ((some-items '("1" "2" "3")))
	  (lister-set-header buf header)
	  (lister-set-footer buf footer)
	  (cl-dolist (item some-items)
	    (lister-add buf item))
	  (lister-set-header buf nil)
	  (lister-set-footer buf nil)
	  (expect buf :to-have-as-content (lister-test-expected-content some-items)))))))

;; * Adding and replacing items

(describe "Adding and replacing items"
  :var (buf some-items)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer))
    (setq some-items '("1" "2abc" "3")))
  (after-each
    (kill-buffer buf))

  (describe "lister-add"
    (it "returns correct marker positions:"
      (let* ((markers (mapcar (apply-partially #'lister-add buf)
			      some-items)))
	(expect (mapcar #'marker-position markers)
		:to-equal
		(lister-test-positions-of some-items))))
    (it "stores item positions in local marker list:"
      (let* ((markers (mapcar (apply-partially #'lister-add buf)
			      some-items)))
	(expect markers
		:to-equal
		(with-current-buffer buf
		  lister-local-marker-list)))))

  (describe "lister-replace"
    (it "replaces last item using position :last:"
      (cl-dolist (item some-items)
	(lister-add buf item))
      (lister-replace buf :last "REPLACED")
      (expect buf :to-have-as-content
	      (lister-test-expected-content (append (butlast some-items 1)
						    '("REPLACED")))))
    (it "replaces all items while forwarding point"
      (cl-dolist (item some-items)
	(lister-add buf item))
      (with-current-buffer buf
	(let ((new-items '("RAS" "DWA" "TRI")))
	  (goto-char (lister-item-min buf))
	  (cl-dolist (item new-items)
	    (lister-replace buf :point item)
	    (forward-line))
	  (expect buf :to-have-as-content (lister-test-expected-content new-items))))))

  (describe "lister-set-list"
    (it "replaces a whole list keeping header and footer"
      (let* ((header "HEADER")
	     (footer "FOOTER"))
	(cl-dolist (item some-items)
	  (lister-add buf item))
	(lister-set-header buf header)
	(lister-set-footer buf footer)
	(let ((new-data '("A" "B" "C")))
	  (lister-set-list buf new-data)
	  (expect buf
		  :to-have-as-content
		  (lister-test-expected-content new-data header footer)))))

    (it "replaces a whole list with no header or footer"
      (cl-dolist (item some-items)
	(lister-add buf item))
      (lister-set-header buf nil)
      (lister-set-footer buf nil)
      (let ((new-data '("A" "B" "C")))
	(lister-set-list buf new-data)
	(expect buf :to-have-as-content (lister-test-expected-content new-data)))))

  (describe "lister-replace-list"
    (it "replaces a list within a region"
      (let* ((the-items      (number-sequence 0 20))
	     (the-positions  (lister-test-positions-of the-items))
	     (first-n        5)
	     (last-n         15)
	     (first          (elt the-positions first-n))
	     (last           (elt the-positions last-n))
	     (new-list       (append (number-sequence 0 (1- first-n))
				     '("neu")
				     (number-sequence (1+ last-n) 20))))
	(lister-add-sequence buf the-items)
	(lister-replace-list buf '("neu") first last)
	(expect buf :to-have-as-content
		(lister-test-expected-content new-list))
	(expect buf :to-have-as-marker-positions
		(lister-test-positions-of new-list))))

    (it "replaces a list from the beginning up to a an item"
      (let* ((the-items      (number-sequence 0 20))
	     (the-positions  (lister-test-positions-of the-items))
	     (last-n         15)
	     (last           (elt the-positions last-n))
	     (new-list       (append '("neu")
				     (number-sequence (1+ last-n) 20))))
	(lister-add-sequence buf the-items)
	(lister-replace-list buf '("neu") nil last)
	(expect buf :to-have-as-content
		(lister-test-expected-content new-list))
	(expect buf :to-have-as-marker-positions
		(lister-test-positions-of new-list))))


    (it "replaces a list from an item up to the end"
      (let* ((the-items      (number-sequence 0 20))
	     (the-positions  (lister-test-positions-of the-items))
	     (first-n        5)
	     (first          (elt the-positions first-n))
	     (new-list       (append (number-sequence 0 (1- first-n))
				     '("neu"))))
	(lister-add-sequence buf the-items)
	(lister-replace-list buf '("neu") first nil)
	(expect buf :to-have-as-content
		(lister-test-expected-content new-list))
	(expect buf :to-have-as-marker-positions
		(lister-test-positions-of new-list))))))



(describe "lister-get-all-data"
  :var (buf some-items)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer))
    (setq some-items '("A" "JHJH" "OUO" "KLL" "RZGVJ&&&%&%/")))
  (after-each
    (kill-buffer buf))

  (it "returns nil if list is empty"
    (expect (lister-get-all-data buf) :to-be nil))
  (it "returns all data as a flat list"
    (cl-dolist (item some-items)
      (lister-add buf item))
    (expect (lister-get-all-data buf) :to-equal some-items))
  (it "returns only data within a region"
    (cl-dolist (item some-items)
      (lister-add buf item))
    (let* ((positions (lister-test-positions-of some-items))
	   (beg-n     2)
	   (end-n     4))
      (expect (lister-get-all-data buf
				   (elt positions beg-n)
				   (elt positions end-n))
	      :to-equal
	      ;; positions always stand for the beginning of the item,
	      ;; so we have to expand the subsequence by 1
	      (seq-subseq some-items beg-n (1+ end-n)))))
  (it "returns all data even if it is marked as hidden"
    (cl-dolist (item some-items)
      (lister-hide-item buf (lister-add buf item)))
    (expect (lister-get-all-data buf) :to-equal some-items)))

;; * Insert Sequence

(describe "lister-insert-sequence:"
  :var (buf)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer)))
  (after-each
    (kill-buffer buf))
  ;;
  (it "insert a list in the right order"
    (let ((some-items '("A" "B" "C" "D")))
      (lister-insert-sequence buf (lister-marker-at buf :first) some-items)
      (expect (lister-get-all-data buf) :to-equal some-items)))
  (it "inserts nested lists"
    (let ((some-items '("A" "B" ("C" "D" ("E" "F")) "G" "H")))
      (lister-insert-sequence buf (lister-marker-at buf :first) some-items)
      (expect (lister-get-all-data-tree buf) :to-equal some-items))))

(describe "lister-add-sequence"
  :var (buf)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer)))
  (after-each
    (kill-buffer buf))

  (it "inserts two sequences one after another"
    (let ((the-sequence '("A" "B" "C" "D")))
      (lister-add-sequence buf the-sequence)
      (lister-add-sequence buf the-sequence)
      (expect (lister-get-all-data buf) :to-equal (append the-sequence the-sequence)))))


;; * Showing / Hiding items

(describe "Hiding items:"
  :var (buf some-items)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer))
    (setq some-items '("A" "B" "C" "D" "E" "ETC" "PP"))
    (lister-add-sequence buf some-items))
  (after-each
    (kill-buffer buf))

  (describe "lister-hide-item"
    (it "hides an arbitrary item"
      (let* ((positions (lister-test-positions-of some-items))
	     (n (random (length some-items))))
	(lister-hide-item buf (elt positions n))
	(with-current-buffer buf
	  (expect (invisible-p (elt positions n))  :to-be   t))))
    (it "does not hide the other items"
      (let* ((positions (lister-test-positions-of some-items))
	     (n (random (length some-items))))
	(lister-hide-item buf (elt positions n))
	(expect buf :to-have-as-visible-content
		(lister-test-expected-content
		 (lister-test-remove-elt-by-index some-items n))))))

  (describe "lister-show-item"
    (it "unhides an item"
      (let* ((positions (lister-test-positions-of some-items))
	     (n (random (length some-items))))
	(lister-hide-item buf (elt positions n))
	(lister-show-item buf (elt positions n))
	(expect buf :to-have-as-content
		(lister-test-expected-content some-items)))))

  (describe "lister-get-visible-data"
    (it "only returns visible item data"
      (let* ((positions (lister-test-positions-of some-items))
	     (n (random (length some-items))))
	(lister-hide-item buf (elt positions n))
	(expect (lister-get-visible-data buf)
		:to-equal
		(lister-test-remove-elt-by-index some-items n))))))

;; * Movement / Navigation

(describe "Navigation:"
  :var (buf some-items positions)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer))
    (setq some-items '("1" "2" "3" "4" "5" "6" "7"))
    (setq positions (lister-test-positions-of some-items))
    (lister-add-sequence buf some-items))
  (after-each
    (kill-buffer buf))

  (describe "lister-goto "
    (it "jumps to the first item when called with :first"
      (lister-goto buf :first)
      (expect buf :to-have-point-value-of (elt positions 0)))
    (it "jumps to the last item when called with :last"
      (lister-goto buf :last)
      (expect buf :to-have-point-value-of (elt positions (1- (length some-items)))))
    (it "accepts an integer position as argument"
      (let* ((n (random (length some-items)))
	     (int-pos (elt positions n)))
	(lister-goto buf int-pos)
	(expect buf :to-have-point-value-of (elt positions n))))
    (it "accepts :point as argument"
      (let* ((n (random (length some-items)))
	     (int-pos (elt positions n)))
	(with-current-buffer buf
	  (goto-char int-pos))
	(lister-goto buf :point)
	(expect buf :to-have-point-value-of (elt positions n))))
    (it "throws an error if going to an invisible item"
      (let ((pos (elt positions 2)))
	(lister-hide-item buf pos)
	(expect (lister-goto buf pos) :to-throw))))

  (describe "lister-with-locked-cursor: "
    (it "moves cursor if body has shrunk the list"
      (let ((expected-last-item  (elt positions (- (length some-items) 2))))
	(lister-goto buf :last)
	(expect buf :not :to-have-point-value-of expected-last-item)
	(lister-with-locked-cursor buf
	  (lister-remove buf :last))
	(expect buf :to-have-point-value-of expected-last-item)))))

;; * Editing

(describe "Editing:"
  :var (buf some-items)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer))
    (setq some-items '("1" "2" "3" "4" "5" "6" "7"))
    (lister-add-sequence buf some-items))
  (after-each
    (kill-buffer buf))

  (describe "lister-move-item-up"
    (it "throws error if called on top item"
      (lister-goto buf :first)
      (with-current-buffer buf
	(expect (lister-move-item-up (current-buffer) (point)) :to-throw)))
    (it "moves last item up"
      (lister-goto buf :last)
      (with-current-buffer buf
	(lister-move-item-up (current-buffer) (point))
	(expect (lister-get-all-data buf) :to-equal '("1" "2" "3" "4" "5" "7" "6"))))
    (it "moves item from bottom to top"
      (lister-goto buf :last)
      (with-current-buffer buf
	(cl-loop for i from 1 below (length some-items)
		 do
		 (lister-move-item-up (current-buffer) (point)))
	(expect (lister-get-all-data buf) :to-equal '("7" "1" "2" "3" "4" "5" "6")))))

  (describe "lister-move-item-down"
    (it "throws error if called on bottom item"
      (lister-goto buf :last)
      (with-current-buffer buf
	(expect (lister-move-item-down (current-buffer) (point)) :to-throw)))))

;; * Index

(describe "Indexes:"
  :var (buf some-items positions)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer))
    (setq some-items '("A" "K" "8989" "dskjfs" "(NKJKHKJ" ")" "HA!"))
    (setq positions (lister-test-positions-of some-items))
    (lister-add-sequence buf some-items))
  (after-each
    (kill-buffer buf))

  (describe "lister-index-position: "
    (it "returns the correct index number when called with integer positions"
      (cl-loop for n from 0 below (length some-items)
	       do
	       (expect (lister-index-position buf (elt positions n))
		       :to-be
		       n)))
    (it "returns the correct index number when called with marker positions"
      (cl-loop for n from 0 below (length some-items)
	       do
	       (let ((m (with-current-buffer buf
			  (goto-char (elt positions n))
			  (point-marker))))
		 (expect (lister-index-position buf m)
			 :to-be
			 n))))
    (it "returns nil if position is invalid"
      (expect (lister-index-position buf 23000) :to-be nil)))

  (describe "lister-index-marker: "
    (it "returns the correct marker for a given index position"
      (cl-loop for n from 0 below (length some-items)
	       do
	       (let ((m (lister-index-marker buf n)))
		 (expect (marker-position m) :to-be (elt positions n)))))
    (it "returns nil if index position is out of bound"
      (let ((n-max (+ (length some-items) 20)))
	(expect (lister-index-marker buf n-max) :to-be nil)))))


;; * Filter

(describe "Using filter:"
  :var (buf some-items)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer))
    ;; don't change the items, filters rely on the content
    (setq some-items '("AAAAA" "ABBBBBB"
		       "BAAAA" "BBBBBB"
		       "SOMETHING"
		       "CAAAA" "CBBBBB"))
    (lister-add-sequence buf some-items))
  (after-each
    (kill-buffer buf))

  (describe "lister-set-filter"
    (it "does nothing if it is called with nil in a new buffer"
      (spy-on 'lister-filter-all-items)
      (lister-set-filter buf nil)
      (expect 'lister-filter-all-items :not :to-have-been-called))
    (it "hides all items matching the passed filter"
      (let ((filter-fn (lambda (data)
			 (string-match-p "\\`A" data))))
	(lister-set-filter buf filter-fn)
	(expect buf
		:to-have-as-visible-content
		(lister-test-expected-content
		 (seq-filter filter-fn some-items)))))
    (it "clears the filter when called with nil in a buffer with active filter"
      (let ((filter-fn (lambda (data)
			 (string-match-p "\\`A" data))))
	(lister-set-filter buf filter-fn)
	(lister-set-filter buf nil)
	(expect buf :to-have-as-visible-content
		(lister-test-expected-content some-items)))))

  (describe "lister-insert with active filter"
    (it "hides items which match the filter"
      (let ((filter-fn (lambda (data)
			 (string-match-p "\\`A" data)))
	    (new-item  "HIDE ME"))
	(lister-set-filter buf filter-fn)
	(lister-insert buf :first new-item)
	(lister-insert buf :first new-item)
	(lister-insert buf :first new-item)
	(expect buf :to-have-as-visible-content
		(lister-test-expected-content
		 (seq-filter filter-fn
			     (append (list new-item new-item new-item)
				     some-items))))))
    (it "does not hide items which do not match the filter"
      (let ((filter-fn (lambda (data)
			 (string-match-p "\\`A" data)))
	    (new-item  "AAAA DO NOT HIDE ME"))
	(lister-set-filter buf filter-fn)
	(lister-insert buf :first new-item)
	(lister-insert buf :first new-item)
	(lister-insert buf :first new-item)
	(expect buf :to-have-as-visible-content
		(lister-test-expected-content
		 (seq-filter filter-fn
			     (append (list new-item new-item new-item)
				     some-items)))))))

  (describe "lister-set-filter"
    (it "makes newly inserted hidden items appear when deactivating filter"
      (let* ((filter-fn (lambda (data)
			  (string-match-p "\\`A" data)))
	     (new-items '("HIDE ME" "HIDE ME,TOO" "AND WHAT ABOUT ME?"))
	     (all-items (append new-items some-items)))
	(lister-set-filter buf filter-fn)
	(lister-insert-sequence buf (lister-item-min buf) new-items)
	(expect buf :to-have-as-visible-content
		(lister-test-expected-content (seq-filter filter-fn all-items)))
	(lister-set-filter buf nil)
	(expect buf :to-have-as-visible-content
		(lister-test-expected-content all-items)))))

  (describe "lister-with-locked-cursor "
    ;; -set-filter calls -walk, which in turn is wrapping its body in
    ;; lister-with-locked-cursor. So just calling set-filter is enough
    ;; to test that macro.
    (it "keeps the visual line if the item is filtered away in its body"
      ;; we move point to the first item, filter the first two items
      ;; of the list, and then expect point to be on the third item,
      ;; which is visually the first visible item.
      (let* ((filter-fn (lambda (data) ;; do not display the first two
			  (not
			   (string-match-p "\\`A" data))))
	     (expected-pos (elt (lister-test-positions-of some-items) 2)))
	(lister-goto buf :first) ;; this first item will be  matched by filter-fn
	(expect buf :not :to-have-point-value-of expected-pos)
	(lister-set-filter buf filter-fn)
	(expect buf :to-have-point-value-of expected-pos)))
    (it "jumps to the last visible line if the current line has been filtered away in the body"
      ;; we move to the last line, filter the last element, and then
      ;; expect cursor to have moved
      (let* ((filter-fn (lambda (data) ;; do not display the last two
			  (not
			   (string-match-p "\\`C" data))))
	     (filtered-items (seq-filter filter-fn some-items))
	     (expected-pos (elt (lister-test-positions-of some-items)
				(- (length some-items)
				   ;; subtract the number of removed items
				   (- (length some-items) (length filtered-items))
				   ;; and subtract one since the index
				   ;; is zero-based
				   1))))
	(lister-goto buf :last)
	(expect buf :not :to-have-point-value-of expected-pos)
	(lister-set-filter buf filter-fn)
	(expect buf :to-have-point-value-of expected-pos)))
    (it "jumps to the first line if the body filters all items"
      (let ((expected-pos (elt (lister-test-positions-of some-items) 0)))
	(lister-goto buf :last)
	(expect buf :not :to-have-point-value-of expected-pos)
	(lister-set-filter buf (lambda (_) nil))
	(expect buf :to-have-point-value-of expected-pos)))
    (it "stays at the first item if everything is shown again"
      (let ((expected-pos (elt (lister-test-positions-of some-items) 0)))
	(lister-goto buf :first)
	(lister-set-filter buf (lambda (_) nil))
	(lister-set-filter buf (lambda (_) t))
	(expect buf :to-have-point-value-of expected-pos)))))


;; * Levels and indentation

(describe "Levels and indentation:"
  :var (buf some-items)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer))
    ;; don't change the list, it is used in the specs below
    (setq some-items '("Item1" "Item2"           ;; 0, 1
		       ("Subitem1" "Subitem2")   ;; 2
		       "Item3"                   ;; 3
		       ("Another Subitem1" "Another Subitem2") ;; 4
		       )))
  (after-each
    (kill-buffer buf))
  ;;
  (describe "lister-add-sequence"
    (it "assigns nested lists a higher level"
      (lister-add-sequence buf some-items)
      (let ((res (lister-walk-all buf (lambda (_) (lister-get-level-at buf (point))))))
	(expect res :to-equal '(0 0 1 1 0 1 1)))))
  (describe "lister-get-all-data-tree"
    (it "returns the nested list"
      (lister-add-sequence buf some-items)
      (expect (lister-get-all-data-tree buf) :to-equal some-items))
    (it "returns nil if buffer is empty"
      (expect (lister-get-all-data-tree buf) :to-be nil)))

  (describe "lister-add"
    (it "inserts according to requested level"
      (lister-add buf "Item1")
      (lister-add buf "Subitem1" 1)
      (lister-add buf "Item2" 0)
      (expect (lister-get-all-data-tree buf)
	      :to-equal '("Item1" ("Subitem1") "Item2")))
    (it "silently forces level of top item to be 0"
      (lister-add buf "Item1" 1)
      (expect (lister-get-level-at buf :first) :to-be 0))
    (it "silently aligns exceeding indentation"
      (lister-add buf "Item1" 0)
      (let ((m (lister-add buf "Item2" 3)))
	(expect (lister-get-level-at buf m) :to-be 1))))

  (describe "lister-remove-this-level"
    (it "removes all subsequent items sharing the same level"
      (lister-add-sequence buf some-items)
      ;; remove from within the first nested element = line #3
      (lister-remove-this-level buf (lister-index-marker buf 2))
      (expect (lister-get-all-data-tree buf)
	      :to-equal
	      (lister-test-remove-elt-by-index some-items 2)))
    (it "removes all subsequent items sharing the same level at the end of a list"
      (lister-add-sequence buf some-items)
      ;; remove from within the second nested element = line #5
      (lister-remove-this-level buf (lister-index-marker buf 5))
      (expect (lister-get-all-data-tree buf)
	      :to-equal
	      (lister-test-remove-elt-by-index some-items 4))))


  (describe "lister-remove-sublist-below"
    (it "removes all indented items below an item"
      (lister-add-sequence buf some-items)
      ;; remove below the last 0-level item, which is #2 in the list
      (lister-remove-sublist-below buf (lister-index-marker buf 1))
      (expect (lister-get-all-data-tree buf)
	      :to-equal
	      (lister-test-remove-elt-by-index some-items 2)))))

;; * Reordering and sorting items

(describe "Reordering items:"
  :var (buf)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer)))
  (after-each
    (kill-buffer buf))

  (describe "lister-reorder-list"
    (it "deletes a sublist"
      (let* ((data '(0 1 2 3 4 (41 42 43 44) 5 6 7 8 9 10))
	     (sublist-pos (elt (lister-test-positions-of data) 5)))
	(lister-set-list buf data)
	(lister-reorder-this-level buf sublist-pos 'ignore)
	(expect buf :to-have-as-data-tree
		'(0 1 2 3 4 5 6 7 8 9 10))))
    (it "reverses a list"
      (let ((data (number-sequence 0 10)))
	(lister-set-list buf data)
	(lister-reorder-list buf 'reverse)
	(expect buf :to-have-as-data-tree
		(reverse data))))
    (it "returns nil if there is no list"
      (expect (lister-reorder-list buf 'reverse)
	      :to-be nil))
    (it "reverses only part of the list"
      (let* ((data (number-sequence 0 20))
	     (pos  (lister-test-positions-of data)))
	(lister-set-list buf data)
	(lister-reorder-list buf 'reverse (elt pos 5) (elt pos 10))
	(expect buf :to-have-as-data-tree
		'(0 1 2 3 4 10 9 8 7 6 5 11 12 13 14 15 16 17 18 19 20)))))

  (describe "lister-sort-list:"
    (it "sorts a flat list"
      (let ((data (number-sequence 0 20)))
	(lister-set-list buf data)
	(lister-sort-list buf #'>)
	(expect buf :to-have-as-data-tree
		(cl-sort data #'>))))
    (it "sorts a part of a flat list"
      (let* ((data (number-sequence 0 20))
	     (pos  (lister-test-positions-of data)))
	(lister-set-list buf data)
	(lister-sort-list buf #'> (elt pos 5) (elt pos 15))
	(expect buf :to-have-as-data-tree
		'(0 1 2 3 4 15 14 13 12 11 10 9 8 7 6 5 16 17 18 19 20))))
    (it "sorts a nested list"
      (let ((data '(0 1 2 3 (31 32 33 34 35 36) 4 5 6)))
	(lister-set-list buf data)
	(lister-sort-list buf #'>)
	(expect buf :to-have-as-data-tree
		'(6 5 4 3 (36 35 34 33 32 31) 2 1 0))))
    (it "returns nil if there is no data"
      (expect (lister-sort-list buf #'>)
	      :to-be nil)))

  (describe "lister-sort-dwim"
    (it "sorts a sublist below point"
      (let* ((data '(0 1 99 3 4 (43 42 41) 5 6))
	     (pos  (elt (lister-test-positions-of data) 4)))
	(lister-set-list buf data)
	(lister-sort-dwim buf pos #'<)
	(expect buf :to-have-as-data-tree
		'(0 1 99 3 4 (41 42 43) 5 6))))
    (it "sorts the current level if there is no sublist"
      (let* ((data '(0 1 99 3 4 (43 42 41) 5 6))
	     (pos  (elt (lister-test-positions-of data) 2)))
	(lister-set-list buf data)
	(lister-sort-dwim buf pos #'<)
	(expect buf :to-have-as-data-tree
		'(0 1 3 4 (41 42 43) 5 6 99))))))

;; * Walk items

(describe "Walk items:"
  :var (buf data walk-positions walk-path)
  (before-each
    (setq buf  (lister-setup (generate-new-buffer "*LISTER*")
			     (apply-partially #'format "%d")))
    ;; item at index 0 has value 0, at 1 has 1, etc.
    (setq data (number-sequence 0 10))
    ;; define a subset of items
    (setq walk-path '(3 6 8 9))
    ;; find the positions of this subset
    (lister-add-sequence buf data)
    (setq walk-positions (mapcar (apply-partially 'lister-index-marker buf) walk-path)))
  (after-each
    (kill-buffer buf))

  (describe "lister-walk-some"
    (it "walks those items passed as marker list"
      (expect (lister-walk-some buf walk-positions #'identity)
	      :to-equal walk-path))
    (it "silently skips positions out of bound"
      (expect (lister-walk-some buf
				(append walk-positions '(23000 26000 234000))
				#'identity)
	      :to-equal walk-path))
    (it "silently skips positions which are not on the cursor gap"
      (expect (lister-walk-some buf
				(append walk-positions (mapcar #'1+ walk-positions))
				#'identity)
	      :to-equal walk-path))
    (it "skips positions where predicate is not matching"
      (expect (lister-walk-some buf walk-positions #'identity  #'cl-evenp)
	      :to-equal (seq-filter #'cl-evenp walk-path)))))


(describe "Callback function"
  :var (value buf callbackfn some-items)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer))
    (setq some-items '("A" "B" "C" "D"))
    (lister-add-sequence buf some-items)
    (switch-to-buffer buf) ;; we need the window to be set.
    (setq callbackfn (lambda ()
		       (setq value (lister-get-data
				    buf
				    :point))))
    (setq value nil))
  (after-each
    (kill-buffer buf))

  (it "Call the callback when entering item"
    (lister-add-enter-callback buf callbackfn)
    (lister-goto buf :last)
    (expect value :to-equal "D"))
  (it "Call the callback when leaving item"
    (lister-add-leave-callback buf callbackfn)
    (lister-goto buf :last)
    (lister-goto buf :first)
    (expect value :to-equal "D"))
  (it "Inhibit callback with `lister-with-locked-cursor'"
    (lister-add-enter-callback buf callbackfn)
    (let (in-between-value)
      (lister-goto buf :first)
      (lister-with-locked-cursor buf
	(lister-goto buf :last)
	(setq in-between-value value)
	(lister-goto buf :first))
      (expect value :to-equal "A")
      (expect in-between-value :to-equal "A"))))

(describe "Mark and unmark items"
  :var (buf data)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer))
    (setq data '("A" "B" 1 2 3 "C" "D"))
    (lister-add-sequence buf data))
  (after-each
    (kill-buffer buf))
  ;;
  (describe "lister-mark-item:"
    (it "marks a single item"
      (let* ((m (seq-random-elt
		 (with-current-buffer buf lister-local-marker-list))))
	(lister-mark-item buf m t)
	(expect (get-text-property m 'mark buf) :to-be  t)
	(expect (lister-get-mark-state buf m)   :to-be  t)))
    (it "unmarks a single item"
      (let* ((m (seq-random-elt
		 (with-current-buffer buf lister-local-marker-list))))
	(lister-mark-item buf m t)
	(lister-mark-item buf m nil)
	(expect (get-text-property m 'mark buf) :to-be  nil)
	(expect (lister-get-mark-state buf m)   :to-be  nil))))

  (describe "lister-markable-p:"
    (it "identifies markable items according to buffer local predicate"
      (with-current-buffer buf
	(setq lister-local-marking-predicate #'numberp))
      (expect (mapcar (lambda (pos) (lister-markable-p buf pos))
		      (lister-test-positions-of data))
	      :to-equal
	      (mapcar #'numberp data)))))

(provide 'lister-tests)
;;; lister-tests.el ends here
